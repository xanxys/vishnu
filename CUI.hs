module CUI(cuiView) where
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vec as V
import Text.Printf
import System.IO
import World
--import FlyThrough
--import MsgPack
--import IPR

cuiView :: BiParticleIO ()
cuiView=do
    liftIO $ putStrLn "CUI view"
    cuiIter

cuiIter=do
    x<-liftIO $ putStr "> " >> hFlush stdout >> getLine
    cuiProcess (words x) >>= flip when cuiIter

-- | parse and run command
cuiProcess ("spawn":dx:dy:dz:view_desc)=do
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
{-
decodeView ["fly"]=return flyThroughView
decodeView ["mp",host,port]=return $ msgpackView host (read port)
decodeView ["ipr"]=return iprView
-}
decodeView _=Nothing


wordsToVec ws=V.Vec3D x y z
    where [x,y,z]=map read ws

