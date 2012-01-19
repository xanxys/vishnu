module World where
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.List
import Data.Either
import Data.Maybe
import qualified Data.Vec as V
import qualified Data.Map as M
import Data.Time.Clock
import System.IO
import System.Environment
import Text.Printf

-- | view IO
--
-- 2 reasonable alternatives:
-- * separate observe :: pos -> IO val ,put :: pos -> val -> IO () interface
-- I cannot (yet) construct simmetry beween observe and put...
-- So there's no way but separate them.
--
-- Maybe, just maybe, use of stationary frame forbids symmetrical time treatment...
-- 'cause if the frame is slightly moving, you can't stay at the same point for long time.
--
type ViewIO a=ReaderT (Frame,Chan Action,IO ()->IO ()) IO a

-- | sec
observe :: Double -> V.Vec3D -> ViewIO RGBA
observe dt p=do
    (f,ch,_)<-ask
    liftIO $ do
        mv<-newEmptyMVar
        writeChan ch $ ActionObserve n 0 (frame_to_world f p) (RGBA 0 0 0 0) mv
        readMVar mv
    where n=max 1 $ ceiling $ 100*dt


put :: Double -> V.Vec3D -> RGBA -> ViewIO ()
put dt p val=do
    (f,ch,_)<-ask
    liftIO $ do
        mv<-newEmptyMVar
        writeChan ch $ ActionPut n 0 (frame_to_world f p) val mv
        readMVar mv
    where n=max 1 $ ceiling $ 100*dt

unsafePeek :: ViewIO (Frame,SharedWorld)
unsafePeek=do
    (f,ch,_)<-ask
    liftIO $ do
        mv<-newEmptyMVar
        writeChan ch $ ActionUnsafePeek mv
        w<-readMVar mv
        return (f,w)

spawn :: Frame -> ViewIO () -> ViewIO ()
spawn df view=do
    (f,ch,sp)<-ask
    liftIO $ sp $ runReaderT view (compose_frame f df,ch,sp)



-- | shared model is incorrect, but a reasonable approximation.
--
-- Using gaussian basis with sigma=delta erases any data on underlying lattice (proof left to the reader).
-- (Part of it can be reduced to whether \Sigma_{i \elem Z} exp(-(i+d)^2)=const for all d)
--
-- However, gaussian basis is not compact. By introducing noise to observed value, you can make it compact(?)
-- No. A view can observe infinitely many times to reduce noise to 0.
--
data SharedWorld=SharedWorld (M.Map (Int,Int,Int) RGBA)

data RGBA=RGBA !Float !Float !Float !Float deriving(Show)

emptyWorld=SharedWorld M.empty

observe_frame :: SharedWorld -> Frame -> V.Vec3D -> RGBA
observe_frame w f p=observe_raw w (frame_to_world f p)

observe_raw :: SharedWorld -> V.Vec3D -> RGBA
observe_raw (SharedWorld m) p=
    sumV $ map (\(pi,w)->mulV w $ M.findWithDefault def pi m) $ discreteGaussian p
    where
        def=RGBA 0 0 0 0


put_frame :: SharedWorld -> Frame -> V.Vec3D -> Double -> RGBA -> SharedWorld
put_frame w f p dt v=put_raw w (frame_to_world f p) dt v


put_raw :: SharedWorld -> V.Vec3D -> Double -> RGBA -> SharedWorld
put_raw (SharedWorld m) p dt v=SharedWorld m'
    where
        m'=foldl' (\m (pi,w)->M.alter (f w) pi m) m $ discreteGaussian p
        def=RGBA 1 1 1 0
        f w v0=Just $ mixV w' (maybe def id v0) v
            where w'=w*(1-exp (realToFrac (-dt)))


mulV k (RGBA r g b a)=RGBA (k*r) (k*g) (k*b) (k*a)
sumV=foldl1' addV
addV (RGBA r0 g0 b0 a0) (RGBA r1 g1 b1 a1)=RGBA (r0+r1) (g0+g1) (b0+b1) (a0+a1)


-- (1-w)a+wb
mixV :: Float -> RGBA -> RGBA -> RGBA
mixV w (RGBA r0 g0 b0 a0) (RGBA r1 g1 b1 a1)=RGBA 
    (r0*ka0+r1*ka1) (g0*ka0+g1*ka1) (b0*ka0+b1*ka1) (a')
    where
        a'=(1-w)*a0+w*a1
        ka0=(1-w)*a0/a'
        ka1=w*a1/a'

-- | discretize gaussian kernel
discreteGaussian :: V.Vec3D -> [((Int,Int,Int),Float)]
discreteGaussian (V.Vec3D x y z)=map f ps
    where
        [ix,iy,iz]=map round [x,y,z]
        ps=liftM3 (,,) [ix-3..ix+3] [iy-3..iy+3] [iz-3..iz+3]
        f p@(ix,iy,iz)=(p,realToFrac w)
            where w=exp $ negate $ sum $ map (^2) $ zipWith (-) (map fromIntegral [ix,iy,iz]) [x,y,z]



-- | with current computers, light propagation is difficult to handle. ActionUnsafePeek is backdoor
-- to internal structure to accelerate rendering. This backdoor should be removed as soon as possible.
data Action
    =ActionObserve Int Int V.Vec3D RGBA (MVar RGBA)
    |ActionPut Int Int V.Vec3D RGBA (MVar ())
    |ActionUnsafePeek (MVar SharedWorld)

data Result
    =ResultObserve RGBA (MVar RGBA)
    |ResultPut (MVar ())
    |ResultUnsafePeek SharedWorld (MVar SharedWorld)

commitAction :: Action -> SharedWorld -> (SharedWorld,Either Action Result)
commitAction (ActionObserve n k p va mv) w
    |n==k' = (w,Right $ ResultObserve (mulV (1/fromIntegral n) va') mv)
    |n>k' = (w,Left $ ActionObserve n k' p va' mv)
    where
        k'=k+1
        va'=addV va $ observe_raw w p
commitAction (ActionPut n k p v mv) w
    |n==k' =(w',Right $ ResultPut mv)
    |n>k' =(w',Left $ ActionPut n k' p v mv)
    where
        w'=put_raw w p dt v
        k'=k+1
        dt=0.1
commitAction (ActionUnsafePeek mv) w=(w,Right $ ResultUnsafePeek w mv)

finishResult :: Result -> IO ()
finishResult (ResultObserve v mv)=putMVar mv v
finishResult (ResultPut mv)=putMVar mv ()
finishResult (ResultUnsafePeek w mv)=putMVar mv w

-- | modify world at 10 ms tick
processAction :: Chan Action -> SharedWorld -> IO ()
processAction ch w=do
    t_tick<-getCurrentTime
    acs<-readChanAll ch
    let
        (w',rs)=mapAccumL (flip commitAction) w acs
        (acs',fs)=partitionEithers rs
    mapM_ finishResult fs
    mapM_ (writeChan ch) acs'
    
    t_now<-getCurrentTime
    let t_elapsed=realToFrac (t_now `diffUTCTime` t_tick)
    threadDelay $ max 0 $ floor $ 1e6*(10e-3-t_elapsed)
    processAction ch w'



-- | read all currently available values from a Chan in FIFO order. Other thread mustn't read.
readChanAll :: Chan a -> IO [a]
readChanAll ch=aux []
    where
        aux xs=do
            e<-isEmptyChan ch
            if e then return (reverse xs) else readChan ch >>= (aux . (:xs))





-- | Stationary frame
data Frame=Frame !V.Vec3D !QD deriving(Show)

-- | Quaternion Double
data QD=QD !V.Vec3D !Double deriving(Show,Eq)

instance Num QD where
    (QD v0 w0) + (QD v1 w1)=QD (v0+v1) (w0+w1)
    (QD v0 w0) * (QD v1 w1)=QD
        (V.map (w0*) v1 + V.map (w1*) v0 + v0 `cross3D` v1) (w0*w1 - v0 `V.dot` v1)
    abs=undefined
    signum=undefined
    fromInteger=undefined

cross3D v0 v1=V.pack $ V.unpack v0 `V.cross` V.unpack v1

conjugate :: QD -> QD
conjugate (QD v w)=QD (-v) w

liftV3 v=QD v 0
dropQ (QD v w)=v

rotateVQ :: QD -> V.Vec3D -> V.Vec3D
rotateVQ q v=dropQ $ q*liftV3 v*conjugate q

frame_to_world :: Frame -> V.Vec3D -> V.Vec3D
frame_to_world (Frame trans rot) pos=trans+rotateVQ rot pos





compose_frame :: Frame -> Frame -> Frame
compose_frame (Frame t0 r0) (Frame t1 r1)=Frame (t0+rotateVQ r0 t1) (r0*r1)

identityFrame :: Frame
identityFrame=Frame (V.Vec3D 0 0 0) (QD (V.Vec3D 0 0 0) 1)

