module CUI(cuiView) where
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vec as V
import Text.Printf
import System.IO
import World
import System.Random
import Control.Concurrent
import FlyThrough
--import MsgPack
--import IPR

cuiView :: BiParticleIO ()
cuiView=do
    liftIO $ putStrLn "CUI view"
    cuiIter

cuiIter=do
    x<-liftIO $ putStr "> " >> hFlush stdout >> getLine
    cuiProcess (words x) >>= flip when cuiIter

light ix=do
    set_velocity $ if ix<100
        then V.Vec3D 1 0 0
        else V.Vec3D (cos $ 0.01*fromIntegral ix) (sin $ 0.01*fromIntegral ix) 0
    
    x<-liftIO $ randomRIO (-1,1)
    y<-liftIO $ randomRIO (-1,1)
    z<-liftIO $ randomRIO (-1,1)
    let
        v=V.Vec3D x y z
        v0=V.map (/(V.norm v)) v
    
    emit_photon (v0,Red)
    liftIO $ threadDelay $ 10*1000
    light $ ix+1

-- | parse and run command
cuiProcess ["light"]=do
    spawn $ light 0
    return True
cuiProcess ["get_photon"]=do
    get_photon >>= liftIO . print
    return True
cuiProcess ("spawn":view_desc)=do
    case decodeView view_desc of
        Nothing -> liftIO $ printf "unknown view: %s\n" (show view_desc)
        Just v -> spawn v
    return True

cuiProcess ["q"]=do
    return False

cuiProcess ["run",path]=do
    mapM_ cuiProcess =<< liftM (map words . lines) (liftIO (readFile path))
    return True

cuiProcess x=do
    liftIO $ printf "unknown command: %s\n" (show x)
    return True


decodeView :: [String] -> Maybe (BiParticleIO ())
decodeView ["fly"]=return flyThroughView
{-
decodeView ["mp",host,port]=return $ msgpackView host (read port)
decodeView ["ipr"]=return iprView
-}
decodeView _=Nothing


wordsToVec ws=V.Vec3D x y z
    where [x,y,z]=map read ws

