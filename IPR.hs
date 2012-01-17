{-# LANGUAGE TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
module IPR(iprView) where
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vec as V
import qualified Data.Map as M
import Data.Maybe
import Data.IORef
import Graphics.UI.Gtk
import Text.Printf
import Text.Peggy
import System.IO
import System.Posix.Process
import World

iprView :: ViewIO ()
iprView=do
    ist<-liftIO $ newMVar (IntState M.empty M.empty [])
    
    liftIO $ forkOS $ do
        initGUI
        
        window <- windowNew
        text <- textViewNew
        
        set window [ containerBorderWidth := 10, containerChild := text ]
        
        tbuf<-textViewGetBuffer text
        let
            regenerate=do
                -- get current text
                iter_b<-textBufferGetStartIter tbuf
                iter_e<-textBufferGetEndIter tbuf
                s<-textBufferGetText tbuf iter_b iter_e False
                textBufferSetModified tbuf False
                
                -- apply
                let ms=mapMaybe parseLine $ lines s
                print ms
                modifyMVar_ ist (\(IntState mp mc _)->return $ IntState mp mc (if null ms then [] else cycle ms))
        
        forkIO $ forever $ do
            mod<-textBufferGetModified tbuf
            when mod $ regenerate
            threadDelay $ 100*1000
        
        widgetShowAll window
        mainGUI
    
    forever $ do
        st<-liftIO $ takeMVar ist
        evolveIS st >>= liftIO . putMVar ist
        liftIO $ threadDelay $ 1*1000
        
    



-- | example
-- 
-- p <- p+V(1,2,1)
-- 0.1: x<=@p
-- 0.2: @(p+V(1,0,0))<=C(1,0.2,0.8)
-- p<-p+V(0,0,1)
--
-- p <- p+V(1,2,1)
-- 0.1: x<=@ p
-- 0.2: p+V(1,0,0) @<= C(1,2,3)
-- p<- p+V(0,0,1)
--
-- true symmetrical op:
-- @p <-> q
data IntState=IntState
    (M.Map String V.Vec3D) -- ^ pos
    (M.Map String RGBA) -- ^ col
    [Mode]

data Mode
    =BindPos Double ExprPos ExprPos
    |BindCol Double ExprCol ExprCol
    deriving(Show)

data ExprPos
    =VAdd ExprPos ExprPos -- ^ alternative: VMove ExprPos V.Vec3D
    |Pos V.Vec3D
    |PRef String
    deriving(Show)

data ExprCol
    =Port ExprPos
    |Col RGBA
    |CRef String
    deriving(Show)
    
evolveIS :: IntState -> ViewIO IntState
evolveIS ist@(IntState mpos mcol modes)=case modes of
    [] -> return ist
    (m:ms) -> do
        (mpos',mcol')<-pMode m
        return $ IntState mpos' mcol' ms
    where
        pMode (BindPos dt (PRef v) pe)=do
            idleW dt
            return (M.insert v (evalPE pe) mpos,mcol)
        pMode (BindPos dt _ _)=do
            idleW dt
            return (mpos,mcol)
        pMode (BindCol dt (CRef v) (Port pe))=do
            x<-observe dt (evalPE pe)
            return (mpos,M.insert v x mcol)
        pMode (BindCol dt (CRef v) ce)=do
            idleW dt
            return (mpos,M.insert v (evalCE ce) mcol)
        pMode (BindCol dt (Port pe) ce)=do
            put dt (evalPE pe) (evalCE ce)
            return (mpos,mcol)
        pMode (BindCol dt _ _)=do
            idleW dt
            return (mpos,mcol)
        
        evalPE :: ExprPos -> V.Vec3D
        evalPE (PRef v)=M.findWithDefault (V.Vec3D 0 0 0) v mpos
        evalPE (Pos p)=p
        evalPE (VAdd p0 p1)=evalPE p0+evalPE p1
        
        evalCE :: ExprCol -> RGBA
        evalCE (Port _)=RGBA 0 0 0 0
        evalCE (Col c)=c
        evalCE (CRef v)=M.findWithDefault (RGBA 0 0 0 0) v mcol
        
        idleW dt=liftIO (threadDelay $ floor $ 1000*1000*dt)


[peggy|
mode :: Mode
    = time ":" epos "<-" epos !. { BindPos $1 $2 $3 }
    / time ":" ecol "<=" ecol !. { BindCol $1 $2 $3 }

epos :: ExprPos
    = epos "+" epos { VAdd $1 $2 }
    / "V(" num "," num "," num ")" { Pos (V.Vec3D $1 $2 $3) }
    / ident {PRef $1}

ecol :: ExprCol
    = "@" epos { Port $1}
    / "C(" num "," num "," num ")" { Col (RGBA (realToFrac $1) (realToFrac $2) (realToFrac $3) 1) }
    / ident {CRef $1}

ident :: String = [a-z]+ { $1 }

time :: Double = num

num :: Double
    = "-" [0-9]+ "." [0-9]+ { read ("-" ++ $1 ++"."++ $2) }
    / [0-9]+ "." [0-9]+ { read ($1 ++"."++ $2) }
    / "-" [0-9]+ { read ("-"++$1)}
    / [0-9]+ { read $1}

delimiter :: ()
    = [:(),@+] { () }
    / "<-" { () }
    / "<=" { () }
|]

parseLine :: String -> Maybe Mode
parseLine x=case parseString mode "" x of
    Right x -> Just x
    _ -> Nothing

