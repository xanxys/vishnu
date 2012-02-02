module FlyThrough(flyThroughView) where
import Control.Arrow
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
import World

-- | 
flyThroughView :: BiParticleIO ()
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
    ss<-newMVar []
    let rFrame=do
            threadDelay $ 20*1000
            vs0<-readIORef vs
            cv0<-takeMVar cvc
            samples<-takeMVar ss
            let samples'=evolveSamples $ map (\(d,t)->(0.3,d,t)) cv0++samples
            putMVar ss samples'
            render samples'
        
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
safeTrampoline :: (MVar [(V.Vec3D,PhotonType)] -> IO ()) -> BiParticleIO ()
safeTrampoline f=do
    (cvc,mv)<-liftIO $ do
        cvc<-newEmptyMVar
        mv<-newEmptyMVar
        forkOS $ (f cvc) `finally` (putMVar mv ())
        return (cvc,mv)
    
    let
        iter :: BiParticleIO ()
        iter=do
            x<-liftIO $ tryTakeMVar mv
            when (isNothing x) $ get_photon >>= (\x->liftIO $ tryPutMVar cvc x) >> iter
    
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

-- | assign [-1,1]^2 to 2:1 frame
reshapeCB (Size w h)
    |w'>h'*2    = G.viewport G.$= (G.Position ((w'-h'*2) `div` 2) 0,G.Size (h'*2) h')
    |otherwise = G.viewport G.$= (G.Position 0 ((h'-w' `div` 2) `div` 2),G.Size w' (w' `div` 2))
    where
        [w',h']=map fromIntegral [w,h]

evolveSamples :: [(Double,V.Vec3D,PhotonType)] -> [(Double,V.Vec3D,PhotonType)]
evolveSamples=filter large . map f
    where
        f (w,d,ty)=(w*0.995,d,ty)
        large (w,_,_)=w>0.01

-- | map photons onto plane as panorama
render :: [(Double,V.Vec3D,PhotonType)] -> IO ()
render samples=do
    -- frame-global configuration
    clearDepth $= 1
    G.clear [G.DepthBuffer,G.ColorBuffer]
    
    G.depthFunc G.$= Just G.Always
    G.blend G.$= G.Enabled
    G.blendFunc G.$= (G.SrcAlpha,G.One) -- MinusSrcAlpha)

    -- render
    G.renderPrimitive Triangles $ mapM_ renderSample samples
    
    -- end frame
    swapBuffers

-- | 
renderSample :: (Double,V.Vec3D,PhotonType) -> IO ()
renderSample (w,dir,ty)=do
    G.color $ toColor w ty
    pt 1 0 >> pt (-0.5) 0.87 >> pt (-0.5) (-0.87)
    where
        pt s t=do
            G.texCoord $ G.TexCoord2 s t
            G.vertex $ G.Vertex3 (px+s*sx) (py+t*sy) 0
        
        -- convert to NDC (top:theta=0, bottom:theta=pi, left:phi=1.5pi, right:phi=-0.5pi)
        px=2*(if phi>1.5*pi then 1.75-phi/(2*pi) else 0.75-phi/(2*pi))-1
        py=2*(1-theta/pi)-1
        
        sx=sigma*0.5/sin theta
        sy=sigma*1
        
        sigma=2
        (theta,phi)=directionInSpherical $ negate dir

-- | Convert unit vector in cartesian coordinates to spherical coordinates (theta,phi)
-- theta: [0,pi], measured from Z+
-- phi: [0,2pi], measured from X+ in X-Y plane.
directionInSpherical :: V.Vec3D -> (Double,Double)
directionInSpherical (V.Vec3D dx dy dz)=(0.5*pi-asin dz,pi+atan2 (-dy) (-dx))


toColor w World.Red=G.Color4 1 0 0 w
toColor w World.Green=G.Color4 0 1 0 w
toColor w World.Blue=G.Color4 0 0 1 w

v2v (V.Vec3D x y z)=G.Vertex3 x y z

