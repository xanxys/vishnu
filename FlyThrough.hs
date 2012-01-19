module FlyThrough(flyThroughView) where
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Foreign.Marshal
import Data.Word
import Data.Maybe
import Data.IORef
import Data.Time.Clock
import Data.Maybe
import Data.Ord
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vec as V
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL as G
import System.Posix.Process
import World

-- | 
flyThroughView :: ViewIO ()
flyThroughView=safeTrampoline $ \cvc->do
    getArgsAndInitialize
    
    -- embed view in display w/ window
    w<-createWindow "vishnu: fly view"
    reshapeCallback $= Just reshapeCB    
    actionOnWindowClose $= MainLoopReturns
        
    -- register key event handler to track ContinuousCommand, enter/leave and exit
    foc<-newIORef False
    ccs<-newIORef S.empty
    keyboardMouseCallback $= Just (handleKeys ccs foc)
    
    -- register mouse event handler to track view orientation change
    dp<-newIORef (0,0)
    passiveMotionCallback $= Just (handleMotion dp foc)

    -- create viewer
    vs<-newIORef (ViewerState (V.Vec3D 0 0 0) (V.Vec3D 0 0 0) 0 0)
    ve<-forkIO $ forever $ do
        ccs0<-readIORef ccs
        dp0<-readIORef dp
        writeIORef dp (0,0)
        modifyIORef vs $ evolveVS 0.02 ccs0 dp0
        threadDelay (20*1000)
    
    -- init draw
    t0<-getCurrentTime
    let rFrame=do
            threadDelay $ 20*1000
            vs0<-readIORef vs
            cv0<-takeMVar cvc
            disp vs0 cv0
        
    G.texture G.Texture2D G.$= G.Enabled
    mkTexture >>= bindTexture
    
    idleCallback $= Just rFrame
    mainLoop
    
    -- deallocate
    killThread ve

bindTexture t=textureBinding Texture2D $= Just t

mkTexture=do
	[tn]<-genObjectNames 1
	textureBinding Texture2D $= Just tn
	
	textureWrapMode Texture2D G.S $= (G.Mirrored,G.Repeat)
	textureWrapMode Texture2D G.T $= (G.Mirrored,G.Repeat)
	
	ptr<-mallocBytes (4*n^2)
	let
	    calcRGBA :: Int -> Int -> [Word8]
	    calcRGBA ix iy=[255,255,255,floor $ 255*calcV r]
	        where r=((fromIntegral ix)^2+(fromIntegral iy)^2)/(fromIntegral n^2)
        
	    calcV :: Double -> Double
	    calcV r=exp $ (-16)*r
		
	pokeArray ptr $ concat [calcRGBA x y|x<-[0..n-1],y<-[0..n-1]]
	texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D (fromIntegral n) (fromIntegral n))
	     0 (PixelData G.RGBA UnsignedByte ptr)
	free ptr

	textureFilter Texture2D $= ((Nearest,Nothing),Linear') -- Linear'
	
	--unbindTexture
	
	return tn
	where n=64 :: Int
	

-- | utility function to ensure f is executed in separate OS thread.
safeTrampoline :: (MVar (Frame,SharedWorld) -> IO ()) -> ViewIO ()
safeTrampoline f=do
    (cvc,mv)<-liftIO $ do
        cvc<-newEmptyMVar
        mv<-newEmptyMVar
        forkOS $ (f cvc) `finally` (putMVar mv ())
        return (cvc,mv)
    
    let
        iter :: ViewIO ()
        iter=do
            x<-liftIO $ tryTakeMVar mv
            when (isNothing x) $ unsafePeek >>= (\x->liftIO $ tryPutMVar cvc x) >> iter
    
    iter



handleKeys ks foc key st mod pos
    |key==Char '\x1b' && st==Down = leaveMainLoop -- exit when escape key pressed
    |key==Char 'e' && st==Down = cursor $= None >> writeIORef foc True
    |key==Char 'q' && st==Down = cursor $= Inherit >> writeIORef foc False
    |otherwise = modifyIORef ks $ maybe id id others
    where
        others=do
            cc<-lookup key table
            return $ (if st==Down then S.insert else S.delete) cc
        
        table=zip (map Char "wsad x") [MoveForward,MoveBackward,MoveLeft,MoveRight,MoveUp,MoveDown]

handleMotion dp foc (Position px py)=do
    Size w h<-get windowSize
    focused<-readIORef foc
    when focused $ do
        let [cx,cy]=map (`div` 2) [w,h]
        when (px/=cx || py/=cy) $ do
            pointerPosition $= Position cx cy
            modifyIORef dp $ \(x0,y0)->(x0+fromIntegral (px-cx),y0+fromIntegral (py-cy))


-- | FPS-like viewer state. That is, 6-button for translation in each direction and mouse to view around
-- right-handed cartesian coordinate.
-- X+ is right, Y+ is forward, Z+ is up
data ViewerState=ViewerState
    V.Vec3D -- ^ position
    V.Vec3D -- ^ velocity
    Double -- ^ pitch (around X)
    Double -- ^ yaw (around Z)

evolveVS :: Double -> S.Set ContinuousCommand -> (Double,Double) -> ViewerState -> ViewerState
evolveVS dt ccs (dx,dy) (ViewerState trans trans' pitch yaw)=ViewerState ntrans ntrans' npitch nyaw
    where
        ntrans=trans+V.map (*dt) trans'
        ntrans'=trans'+V.map (*dt) trans''
        npitch=min (0.5*pi) $ max (-0.5*pi) $ pitch+dy*0.01
        nyaw=yaw+dx*0.01
        trans''=V.map (*50) $ trans_rs''+trans_cc''
        trans_rs''=V.map (*(-0.1)) trans'
        trans_cc''=applyRotation $ sum $ map translateCC (S.toList ccs)
        applyRotation=rotateV (V.Vec3D 0 0 1) (-yaw)

-- | rotate given vector by axis and angle
rotateV :: V.Vec3D -> Double -> V.Vec3D -> V.Vec3D
rotateV axis theta v=projv + V.map (*(cos theta)) perpv + V.map (*(sin theta)) ppv
    where
        projv=V.map (*(axis `V.dot` v)) axis
        perpv=v-projv
        ppv=axis `cross3D` perpv

-- | ContinuousCommand represents a force acting to ViewerState at given moment
data ContinuousCommand
    =MoveForward | MoveLeft |MoveRight | MoveBackward | MoveUp | MoveDown
    deriving(Eq,Ord,Show)

-- | convert command to force
translateCC :: ContinuousCommand -> V.Vec3D
translateCC MoveForward=V.Vec3D 0 1 0
translateCC MoveLeft=V.Vec3D (-1) 0 0
translateCC MoveRight=V.Vec3D 1 0 0
translateCC MoveBackward=V.Vec3D 0 (-1) 0
translateCC MoveUp=V.Vec3D 0 0 0.8
translateCC MoveDown=V.Vec3D 0 0 (-0.8)





reshapeCB (Size w h)=do
    G.viewport G.$= (G.Position 0 0,G.Size (fromIntegral w) (fromIntegral h))

data PlayState
    =Stop
    |Play

disp :: ViewerState -> (Frame,SharedWorld) -> IO ()
disp vs (fr,SharedWorld m)=do
    -- frame-global configuration
    clearDepth $= 1
    G.clear [G.DepthBuffer,G.ColorBuffer]
    
    G.depthFunc G.$= Just G.Always
    G.blend G.$= G.Enabled
    G.blendFunc G.$= (G.SrcAlpha,G.OneMinusSrcAlpha)

    -- render
    let ViewerState pcam _ _ _=vs
    withVS vs $ G.renderPrimitive Triangles $
        mapM_ (renderPoint m pcam) $ sortBy (comparing (negate . pointDistance vs)) $ M.keys m -- pointsInView vs
        
    -- end frame
    swapBuffers


-- | convert action in frame-coordinate to viewer-coordinate
withVS :: ViewerState -> IO a -> IO a
withVS (ViewerState (V.Vec3D tx ty tz) _ pitch yaw) f=G.preservingMatrix $ do
        G.frustum (-1) 1 (-1) 1 1 100
        G.rotate (-90::Double) (G.Vector3 1 0 0)
        G.rotate (180/pi*pitch) (G.Vector3 1 0 0)
        G.rotate (180/pi*yaw) (G.Vector3 0 0 1)
        G.translate $ G.Vector3 (-tx) (-ty) (-tz)
        f

pointDistance :: ViewerState -> (Int,Int,Int) -> Double
pointDistance (ViewerState p _ _ _) (x,y,z)=
    V.norm $ p-(V.Vec3D (fromIntegral x) (fromIntegral y) (fromIntegral z))

-- | Return points in view in approximately farthest-first order
pointsInView :: ViewerState -> [(Int,Int,Int)]
pointsInView (ViewerState (V.Vec3D tx ty tz) _ _ _)=
    map (\(x,y,z)->(ox+x,oy+y,oz+z)) $
    sortBy (comparing (\(x,y,z)->negate $ abs x+abs y+abs z)) ps
    where
        ps=liftM3 (,,) [-n..n] [-n..n] [-n..n]
        ox=round tx
        oy=round ty
        oz=round tz
        n=20



renderPoint m pcam p@(ix,iy,iz)=
    case M.lookup p m of
        Nothing -> return ()
        Just (World.RGBA r g b a) -> do
            G.color $ G.Color4 r g b a
            let p=V.Vec3D (fromIntegral ix) (fromIntegral iy) (fromIntegral iz)
            pointTriangle p pcam 4


pointTriangle :: V.Vec3D -> V.Vec3D -> Double -> IO ()
pointTriangle p q sigma=pt 1 0 >> pt (-0.5) 0.87 >> pt (-0.5) (-0.87)
    where
        pt s t=do
            G.texCoord $ G.TexCoord2 s t
            G.vertex $ v2v $ p + V.map (*(s*sigma)) eS + V.map (*(t*sigma)) eT
        
        n=normalize $ q-p
        eS=normalize $ (V.Vec3D 1 0 0) `cross3D` n
        eT=n `cross3D` eS
        
        normalize v=V.map (/(V.norm v)) v
        



v2v (V.Vec3D x y z)=G.Vertex3 x y z

