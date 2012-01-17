module CUI(cuiView) where
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vec as V
import Text.Printf
import System.IO
import World
import FlyThrough
import MsgPack
import IPR

cuiView :: ViewIO ()
cuiView=do
    liftIO $ putStrLn "CUI view"
    cuiIter

cuiIter=do
    x<-liftIO $ putStr "> " >> hFlush stdout >> getLine
    cuiProcess (words x) >>= flip when cuiIter

cuiProcess ["fly"]=do
    spawn identityFrame flyThroughView
    return True

cuiProcess ["mp",host,port]=do
    spawn identityFrame $ msgpackView host (read port)
    return True

cuiProcess ["ipr"]=do
    spawn identityFrame $ iprView
    return True

cuiProcess ["q"]=do
    return False

cuiProcess ["put",px,py,pz,cr,cg,cb,ca]=do
    put 0.1 (wordsToVec [px,py,pz]) (wordsToRGBA [cr,cg,cb,ca])
    return True

cuiProcess ["run",path]=do
    mapM_ cuiProcess =<< liftM (map words . lines) (liftIO (readFile path))
    return True

cuiProcess x=do
    liftIO $ printf "unknown command: %s\n" (show x)
    return True

wordsToVec ws=V.Vec3D x y z
    where [x,y,z]=map read ws

wordsToRGBA ws=RGBA r g b a
    where [r,g,b,a]=map read ws

