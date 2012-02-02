-- | temporarily modified for experimenting with 2-d visual programming language
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
import Data.Word
import Data.Int
import Data.List
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Text.Printf
import System.IO
import System.Exit
import System.Random
-- import World


data NodeId=NodeId !Word64 !Word64 deriving(Eq,Ord)

generateNodeId :: IO NodeId
generateNodeId=liftM2 NodeId randomIO randomIO

instance Show NodeId where
    show (NodeId p0 p1)=printf "%016x%016x" p0 p1

instance Random Word64 where
    randomIO=liftM fromIntegral $ (randomRIO (0,0xffffffffffffffffffffffff) :: IO Integer)

-- desirable order:
--  list all: O(N)
--  find within distance R: O(N_R log(N))
-- lookup node info from id: 
-- 
data World=World [(NodeId,V.Vec2D,Node)] [((NodeId,PortId),(NodeId,PortId))] [(NodeId,Data)]

emptyWorld=World [] [] []




-- operational model: (array is very different from others... since its size is not bounded)
-- changing array to tuple won't help. they will just get bigger (because of overhead).
--
-- data can be stored by FF or creating new constnode
--
-- storage should be done in array of flip flops(length 1 delay line) or long delay line.
-- but let's not take that approach for now.
--
-- topology change cause "change" event to propagate downstream
--
-- whenever a node receives an event, it will act. if results changed, emit change event from output port.
--  change detection: array update always cause event
--
-- you can expect that each action takes somewhat constant time for given type of node (expect for array)
--
-- when a new event came while in action, previous calculation is thrown away and re-start with new data.
-- when restarting, output will become EmptyData
--  (so if you input very often, there will be single event (->EmptyData) on output line)
--
-- port-port transfer takes O(1+distance * size of data) time
--

type PortId=Int

-- lots of primitives. but you should keep them under 100 or so.
data Node
    =ConstNode Data -- | *
    |PrimNode String -- polymorphic node. wait. PrimNode isn't enough for everything?
    |TapNode -- * | *,*
    |IdNode -- * | *
    -- arith
    |AddNode Data Data Data -- Num,Num | Num
    |MulNode -- Num,Num | Num
    |CompareNode -- Num,Num | Bool
    |EqualNode -- Int,Int | Bool
    -- bool
    |NandNode -- Bool, Bool | Bool
    |MuxNode -- Bool, *, * | *
    -- array
    |LengthNode -- array | int
    |ConcatNode -- array, array | array
    |EncapNode -- * | [*]
    |IndexNode -- array, int | array
    -- special
    |SearchNode -- Float | [Node] (order undefined. but nearest-first is preferred)
    |LocateNode -- Node | Float Float
    |MoveNode -- Node Float Float |
    |ConnectNode -- Node,Int,Node,Int |
    |DisconnectNode -- Node,Int |
    |WrapNode -- * | Node
    |ReplicateNode -- Node | Node
    |DeleteNode -- Node |


data Data
    =ArrayData [Data] -- N<2^31
    |IntData Int32
    |FloatData Double
    |NodeData NodeId
    |BoolData Bool
    |EmptyData
    deriving(Show)


iprView :: IO ()
iprView=do
    initGUI
    
    window <- windowNew
    darea <- drawingAreaNew
    
    set window [ containerBorderWidth := 10, containerChild := darea ]

    widgetModifyBg darea StateNormal (Color 0xffff 0xffff 0xffff)
    
    w<-newIORef emptyWorld
    n0<-ioInsSpecialNode w (V.Vec2D 10 10) (IntData 0) "int src"
    n1<-ioInsSpecialNode w (V.Vec2D 10 30) (IntData 0) "int src"
    nadd<-ioInsNormalNode w (V.Vec2D 30 30) (AddNode EmptyData EmptyData EmptyData)
    n2<-ioInsSpecialNode w (V.Vec2D 50 50) EmptyData "sink"
    ioConnect w (n0,0) (nadd,0)
    ioConnect w (n1,0) (nadd,1)
    ioConnect w (nadd,2) (n2,0)
    dragging<-newIORef Nothing
    
    --register
    window `on` destroyEvent $ tryEvent $ liftIO exitSuccess -- mainQuit
    
    widgetAddEvents darea [PointerMotionMask]
    darea `on` motionNotifyEvent $ tryEvent $ move darea dragging w
    darea `on` exposeEvent $ tryEvent $ draw darea w
    darea `on` buttonPressEvent $ tryEvent $ press darea dragging w
    
    widgetShowAll window
    mainGUI

ioUpdateOPort :: IORef World -> (NodeId,Int) -> Data -> IO ()
ioUpdateOPort w port d=do
    (World nodes conns exts)<-readIORef w
    case find ((==port) . fst) conns of
        Nothing -> return () -- no outgoing connection
        Just (_,to) -> do
            ioUpdateIPort w to d

fst3 (a,b,c)=a

ioUpdateIPort :: IORef World -> (NodeId,Int) -> Data -> IO ()
ioUpdateIPort w (ni,pi) d=do
    (World nodes conns exts)<-readIORef w
    case find (\(i,p,n)->i==ni) nodes of
        Nothing -> putStrLn "dangling connection"
        Just (_,p,n) -> do
            execNodeAction w n ni pi d

-- maybe it should be good to move input/output buffer to connections
execNodeAction w (PrimNode "sink") ni 0 d=do
    modifyIORef w $ \(World nodes conns exts)->World nodes conns (map (\t@(i,_)->if ni/=i then t else (i,d)) exts)
execNodeAction w (AddNode _ i1 o) ni 0 d=do
    let o'=addD d i1
    modifyIORef w $ \(World nodes conns exts)->World (map (\t@(i,p,_)->if ni/=i then t else (i,p,AddNode d i1 o')) nodes) conns exts
    ioUpdateOPort w (ni,2) o'
execNodeAction w (AddNode i0 _ o) ni 1 d=do
    let o'=addD i0 d
    modifyIORef w $ \(World nodes conns exts)->World (map (\t@(i,p,_)->if ni/=i then t else (i,p,AddNode i0 d o')) nodes) conns exts
    ioUpdateOPort w (ni,2) o'
execNodeAction w _ _ _ _=return ()


    
    
        

ioInsNormalNode :: IORef World -> V.Vec2D -> Node -> IO NodeId
ioInsNormalNode w pos n=do
    nid<-generateNodeId
    modifyIORef w $ \(World nodes conn exts)->World ((nid,pos,n):nodes) conn exts
    return nid

ioInsSpecialNode :: IORef World -> V.Vec2D -> Data -> String -> IO NodeId
ioInsSpecialNode w pos d ty=do
    nid<-generateNodeId
    modifyIORef w $ \(World nodes conn exts)->World ((nid,pos,PrimNode ty):nodes) conn ((nid,d):exts)
    return nid

ioConnect :: IORef World -> (NodeId,PortId) -> (NodeId,PortId) -> IO ()
ioConnect w s d=do
    modifyIORef w $ \(World nodes conns exts)->World nodes ((s,d):conns) exts

insNormalNode :: V.Vec2D -> Node -> World -> IO World
insNormalNode pos n (World nodes conn exts)=do
    nid<-generateNodeId
    return $ World ((nid,pos,n):nodes) conn exts

insSpecialNode :: V.Vec2D -> Data -> String -> World -> IO World
insSpecialNode pos d ty (World nodes conn exts)=do
    nid<-generateNodeId
    return $ World ((nid,pos,PrimNode ty):nodes) conn ((nid,d):exts)

draw :: DrawingArea -> IORef World -> EventM EExpose ()
draw da w=liftIO $ do
    dw<-widgetGetDrawWindow da
    w<-readIORef w
    renderWithDrawable dw $ renderWorld w

renderWorld (World nodes conns exts)=do
    -- basic structure
    setSourceRGB 0 0 0
    mapM_ (\(_,V.Vec2D x y,_)->arc x y 2 0 (2*pi) >> fill) nodes
    
    let m=M.fromList $ map (\(i,p,n)->(i,(p,n))) nodes
    mapM_ (\((np,_),(nq,_))->arrow (fst $ m M.! np) (fst $ m M.! nq)) conns
    
    -- extended  structure
    let me=M.fromList exts
    
    mapM_ (ext me) nodes
    where
        applyV f (V.Vec2D x y)=f x y
        arrow from to=do
            applyV moveTo from
            applyV lineTo to
            stroke
            let
                dir=normalizeV $ to-from
                dp=rotateCCW dir
            applyV moveTo to
            applyV lineTo $ to-V.map (*5) dir+V.map (*5) dp
            applyV lineTo $ to-V.map (*5) dir-V.map (*5) dp
            closePath
            fill
                
            
        ext me (nid,V.Vec2D x y,PrimNode "int src")=do
            let IntData n=me M.! nid
            setLineWidth 1
            rectangle (x-30) (y-12.5) 60 25
            stroke
            save
            translate x y
            showText (show n)
            restore
        ext me (nid,V.Vec2D x y,PrimNode "sink")=do
            let d=me M.! nid
            setLineWidth 1
            rectangle (x-30) (y-12.5) 60 25
            stroke
            save
            translate x y
            showText (show d)
            restore
        ext _ _=return ()

normalizeV v=V.map (/(V.norm v)) v
rotateCCW (V.Vec2D x y)=V.Vec2D (-y) x


move :: DrawingArea -> IORef (Maybe NodeId) -> IORef World -> EventM EMotion ()
move widget d w=do
    dragging<-liftIO $ readIORef d
    case dragging of
        Nothing -> return ()
        Just nid -> do
            (px,py)<-eventCoordinates
            (World nodes conns exts)<-liftIO $ readIORef w
            let nodes'=map (\t@(i,pos,n)->if nid/=i then t else (i,V.Vec2D px py,n)) nodes
            liftIO $ writeIORef w $ World nodes' conns exts
            liftIO $ widgetQueueDraw widget

press :: DrawingArea -> IORef (Maybe NodeId) -> IORef World -> EventM EButton ()
press widget dragging w=do
    pos<-eventCoordinates
    liftIO $ do
        (World nodes _ _)<-readIORef w
        mapM_ (check pos) nodes
    where
        check (px,py) (nid,V.Vec2D cx cy,PrimNode _)
            |abs (px-cx)<5 && abs (py-cy)<5 = do
                dr<-readIORef dragging
                case dr of
                    Just _ -> writeIORef dragging $ Nothing
                    Nothing -> do
                        (World nodes conns exts)<-readIORef w
                        let
                            exts'=map (\(i,n)->if i/=nid then (i,n) else (i,addD n (IntData 1))) exts
                            Just ttt=find ((==nid) . fst) exts'
                        writeIORef w $ World nodes conns exts'
                        ioUpdateOPort w (nid,0) $ snd ttt
                        widgetQueueDraw widget
            |abs (px-cx)<30 && abs (py-cy)<12.5 = do
                dr<-readIORef dragging
                case dr of
                    Nothing -> writeIORef dragging $ Just nid
                    Just _ -> writeIORef dragging $ Nothing
            |otherwise = return ()
        check (px,py) (nid,V.Vec2D cx cy,n)
            |abs (px-cx)<5 && abs (py-cy)<5 = do
                dr<-readIORef dragging
                case dr of
                    Nothing -> writeIORef dragging $ Just nid
                    Just _ -> writeIORef dragging $ Nothing
            |otherwise = return ()

addD (IntData x) (IntData y)=IntData $ x+y
addD _ _=EmptyData
    
