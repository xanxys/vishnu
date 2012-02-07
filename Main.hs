{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleContexts #-}
module Main where
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vec as V
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.IORef
import Data.Word
import Data.Int
import Data.List
import Data.Ord
import Graphics.UI.Gtk hiding (Cursor)
import Graphics.Rendering.Cairo
import Text.Printf
import System.IO
import System.Random
import System.Environment
import Data.Time

-- it idle<->grab interface
--
-- plan 1. no ports
-- store (children::[NodeId],[PortDesc],name::String) in exts.
--  provide big circle centerd at CoM of children.
--  
main :: IO ()
main=do
    initGUI
    
    nodes<-liftM parseAddition getArgs
    
    -- initialize UI strucutre
    window <- windowNew
    darea <- drawingAreaNew
    set window [ containerBorderWidth := 10, containerChild := darea ]
    widgetModifyBg darea StateNormal (Color 0xffff 0xffff 0xffff)
    
    -- create new world from file
    w<-newIORef emptyWorld
    printf "loading state from %s\n" path
    (writeIORef w . read =<< readFile path) `catch` \e -> do
        printf "load failed. creating default image\n"
        
        n0<-ioInsSpecialNode w (V.Vec2D 50 50) (IntData 0) "int upcount"
        n1<-ioInsSpecialNode w (V.Vec2D 50 100) (IntData 0) "int upcount"
        n2<-ioInsSpecialNode w (V.Vec2D 50 150) EmptyData "display"
        
        let n=10
        mapM_ (\(i,t)->ioInsNormalNode w
            (V.Vec2D (50+fromIntegral (i `div` n)*80) (200+fromIntegral (i `mod` n)*50)) t)
            $ zip [0..] [ConstNode $ IntData 1,ConstNode $ FloatData 0,ConstNode $ FloatData 100,
                            TapNode,
                            AddNode,MulNode,CompareNode,EqualNode,
                            NandNode,MuxNode,
                            LengthNode,ConcatNode,IndexNode,EncapNode,
                            SearchNode,LocateNode,MoveNode,
                            ConnectNode,DisconnectNode,WrapNode False,ReplicateNode False,DeleteNode]
    
    -- add elements specified in command line
    mapM (\add_n->randomN2IO >>= add_n w . (+V.Vec2D 300 300) . (V.map (*100))) nodes
    
    -- create cursor state
    cursor<-newIORef $ Cursor False False (V.Vec2D 0 0) Idle
    
    -- window close
    onDestroy window $ mainQuit -- window `on` destroyEvent doesn't work 
    
    -- node control w/ mouse
    msg_count<-newRateCounter
    
    widgetAddEvents darea [PointerMotionMask]
    darea `on` exposeEvent $ tryEvent $ liftIO (summary msg_count >>= drawDebugString darea)
         >> draw darea cursor w
    darea `on` motionNotifyEvent $ tryEvent $ move darea cursor w >> liftIO (widgetQueueDraw darea)
    darea `on` buttonPressEvent $ tryEvent $ press darea cursor w >> liftIO (widgetQueueDraw darea)
    darea `on` buttonReleaseEvent $ tryEvent $ release darea cursor w >> liftIO (widgetQueueDraw darea)
    
    -- update
    idleAdd (count msg_count >> seqStep1 cursor w >>  return True) priorityDefaultIdle
    timeoutAdd (widgetQueueDraw darea >> return True) 100
    
    widgetShowAll window
    mainGUI
    
    -- save
    printf "saving state to %s\n" path
    writeFile path . show =<< readIORef w
    where path="world.image"

parseAddition ("--add":x:xs)=(\w p->ioInsNormalNode w p (read x)):parseAddition xs
parseAddition ("--add_sp":x:xs)=(\w p->ioInsSpecialNode w p EmptyData x):parseAddition xs
parseAddition _=[]

-- | count total number of events and rate of events. # is limited by Int
data RateCount=RateCount (MVar (Integer,Int,UTCTime))

newRateCounter :: IO RateCount
newRateCounter=do
    t0<-getCurrentTime
    liftM RateCount $ newMVar (0,0,t0)

count :: RateCount -> IO ()
count (RateCount mv)=modifyMVar_ mv $ \(acc,d,t)->return (acc+1,d+1,t)

summary :: RateCount -> IO String
summary (RateCount mv)=do
    (acc,d,t0)<-takeMVar mv
    t1<-getCurrentTime
    putMVar mv $ (acc,0,t1)
    let r=fromIntegral d/realToFrac (t1 `diffUTCTime` t0) :: Double
    return $ printf "throughput:%.2f/sec total: %d" r acc




-- bare with the position, since gtk doesn't (seem to) provide cursor position outside of EventM monad.
data Cursor=Cursor
    Bool -- ^ activate (changed to False when handled or moved)
    Bool -- ^ pressed (continuous state)
    V.Vec2D CursorState

data CursorState=Idle|Grab NodeId|ConnectFrom PortDesc|Pan deriving(Eq)

-- | desirable order:
--  list all: O(N)
--  find within distance R: O(N_R log(N))
-- lookup node info from id: 
-- 
data World=World [(NodeId,V.Vec2D,Node)] [((PortDesc,PortDesc),Data)] [(NodeId,Data)]
    deriving(Show,Read)

instance Read V.Vec2D where
    readsPrec n s=map (first $ uncurry V.Vec2D) $ readsPrec n s

instance Show V.Vec2D where
    show (V.Vec2D x y)=show (x,y)

emptyWorld=World [] [] []


-- | operational model (obsolete):
-- (array is very different from others... since its size is not bounded)
-- changing array to tuple won't help. they will just get bigger (because of overhead).
--
-- nodes are stateless. connections are.
-- (WrapNode and ReplicateNode detect rising edge, so they actually have 1 bit state)
--
-- data can be stored by FF or creating new ConstNode (by using WrapNode)
--
-- storage should be done in array of flip flops(length 1 delay line) or long delay line.
-- but let's forget about that for now.
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
-- lots of primitives. but you should keep them under 100 or so.
--
-- TODO: rename "connect" or "disconenct" to "keep" or "inhibit"? because they're continuous action
data Node
    =ConstNode Data -- | *
    |PrimNode String -- polymorphic node. wait. PrimNode isn't enough for everything?
    |TapNode -- * | *,*
    -- arith
    |AddNode -- Num,Num | Num
    |MulNode -- Num, Num | Num
    |CompareNode -- Num, Num | Bool (>=)
    |EqualNode -- Int, Int | Bool
    -- bool
    |NandNode -- Bool, Bool | Bool
    |MuxNode -- Bool, *, * | *
    -- array
    |LengthNode -- [] | int
    |ConcatNode -- [], [] | array
    |EncapNode -- * | []
    |IndexNode -- array, int | array
    -- spatial
    |SearchNode -- Float | [] [] (order undefined. but nearest-first is preferred)
    |LocateNode -- Node | Float, Float
    |MoveNode -- Node, Float, Float |
    -- structural
    |ConnectNode -- Node, Int, Node, Int |
    |DisconnectNode -- Node, Int |
    |WrapNode Bool -- * | Node (create ConstNode, since it's parametric)
    |ReplicateNode Bool -- Node | Node
    |DeleteNode -- Node |
    deriving(Show,Read) 

-- | /primitive/ values. TODO: make them bounded (i.e. remove Array, Tuple etc.)
data Data
    =ArrayData [Data] -- N<2^31
    |IntData Int32
    |FloatData Double
    |NodeData NodeId
    |BoolData Bool
    |EmptyData
    deriving(Show,Read)

extractFloat (IntData x)=Just $ fromIntegral x
extractFloat (FloatData x)=Just x
extractFloat _=Nothing

addD (IntData x) (IntData y)=IntData $ x+y
addD (IntData x) (FloatData y)=FloatData $ fromIntegral x+y
addD (FloatData x) (IntData y)=FloatData $ x+fromIntegral y
addD (FloatData x) (FloatData y)=FloatData $ x+y
addD _ _=EmptyData

mulD (IntData x) (IntData y)=IntData $ x*y
mulD (IntData x) (FloatData y)=FloatData $ fromIntegral x*y
mulD (FloatData x) (IntData y)=FloatData $ x*fromIntegral y
mulD (FloatData x) (FloatData y)=FloatData $ x*y
mulD _ _=EmptyData

cmpD (IntData x) (IntData y)=BoolData $ x>=y
cmpD (IntData x) (FloatData y)=BoolData $ fromIntegral x>=y
cmpD (FloatData x) (IntData y)=BoolData $ x>=fromIntegral y
cmpD (FloatData x) (FloatData y)=BoolData $ x>=y
cmpD _ _=EmptyData

-- | only Int,Bool,Node can be compared for equality
eqD (IntData x) (IntData y)=BoolData $ x==y
eqD (BoolData x) (BoolData y)=BoolData $ x==y
eqD (NodeData x) (NodeData y)=BoolData $ x==y
eqD _ _=EmptyData

lengthD (ArrayData xs)=IntData $ fromIntegral $ length xs
lengthD _=EmptyData

concatD (ArrayData xs) (ArrayData ys)=ArrayData $ xs++ys -- TODO length clipping
concatD (ArrayData xs) EmptyData=ArrayData xs
concatD EmptyData (ArrayData xs)=ArrayData xs
concatD EmptyData EmptyData=ArrayData []
concatD _ _=EmptyData

encapD EmptyData=EmptyData
encapD d=ArrayData [d]

nandD (BoolData x) (BoolData y)=BoolData $ not $ x&&y
nandD _ _=EmptyData

indexD (ArrayData xs) (IntData i)
    |0<=i' && i'<length xs = xs!!i'
    |otherwise = EmptyData
    where i'=fromIntegral i
indexD _ _=EmptyData




data NodeId=NodeId !Word64 !Word64 deriving(Eq,Ord,Show,Read)

generateNodeId :: IO NodeId
generateNodeId=liftM2 NodeId randomIO randomIO



instance Random Word64 where
    randomIO=liftM fromIntegral $ (randomRIO (0,0xffffffffffffffffffffffff) :: IO Integer)





type PortDesc=(NodeId,PortId)
type PortId=Int


-- approximate area-proprtional calculation by selecting nearest node from randomly sampled point
seqStep1 :: IORef Cursor -> IORef World -> IO ()
seqStep1 c w=do
    (World nodes conns exts)<-readIORef w
    let
        n=length nodes
        ps=map (\(_,p,_)->p) nodes
    
    when (n>0) $ do
        let
            minx=minimum $ map (\(V.Vec2D x _)->x) ps
            maxx=maximum $ map (\(V.Vec2D x _)->x) ps
            miny=minimum $ map (\(V.Vec2D _ y)->y) ps
            maxy=maximum $ map (\(V.Vec2D _ y)->y) ps
        
        x<-randomRIO (minx,maxx)
        y<-randomRIO (miny,maxy)
        let p=V.Vec2D x y
        
        let (ni,_,n)=minimumBy (comparing $ \(_,q,_)->V.normSq (q-p)) nodes
        
        execCommonAction c w ni
        execNodeAction c w n ni


ioUpdateOPort :: IORef World -> PortDesc -> Data -> IO ()
ioUpdateOPort w port d=do
    (World nodes conns exts)<-readIORef w
    case find f conns of
        Nothing -> return () -- no outgoing connection
        Just ((_,to),_) -> do
            writeIORef w $ World nodes (map (\x->if f x then (fst x,d) else x) conns) exts
    where f=(==port) . fst . fst

execCommonAction :: IORef Cursor -> IORef World -> NodeId -> IO ()
execCommonAction c w ni=do
    npos<-getNodePosition w ni
    Cursor activate pressed pos st<-readIORef c
    let
        dp=pos-npos
        onBody=V.norm dp<5
        onPort n=V.norm (dp-center)<5
            where
                center=V.map (*30) (V.Vec2D (cos theta) (sin theta))
                theta=pi*0.1*fromIntegral n
        bind cond action
            |cond = action >> return True
            |otherwise = return False
        
        onIdlePortActivate n=bind (onPort n) $ do
            ac<-portHasConnection w (ni,n)
            if ac
                then ioDisconnect w (ni,n) >> writeIORef c (Cursor False pressed pos Idle)
                else writeIORef c $ Cursor False pressed pos (ConnectFrom (ni,n))
        onConnectFromPortActivate srcport n=bind (onPort n) $ do
            ac<-portHasConnection w (ni,n)
            unless (ac || (ni,n)==srcport) $ ioConnect w srcport (ni,n)
            writeIORef c $ Cursor False pressed pos Idle
    
    -- cursor modification is only allowed to nodes that is 1.being grabbed or 2.onBody or 3.onPort
    if activate
        then void $ sequenceC $
            case st of
                Idle ->
                    [bind (onBody) $ writeIORef c $ Cursor False pressed pos (Grab ni)]++
                    map onIdlePortActivate [0..5]
                ConnectFrom srcport -> map (onConnectFromPortActivate srcport) [0..5]
                Grab x -> 
                    [bind (x==ni) $ writeIORef c $ Cursor False pressed pos Idle
                    ]
        else do
            void $ sequenceC $
                [bind (st==Grab ni) $ modifyNodePosition w ni pos]
        
            

-- TODO: IORef Cursor -> Interface
execNodeAction :: IORef Cursor -> IORef World -> Node -> NodeId -> IO ()
execNodeAction _ w (ConstNode d) ni=
    ioUpdateOPort w (ni,0) d

execNodeAction c w (PrimNode "int upcount") ni=do
    p<-getNodePosition w ni
    Cursor act pr cpos st<-readIORef c
    let do_upcount=V.norm (cpos-p)<20 && act
    count<-getNodeExtension w ni
    if do_upcount
        then do
            let count'=addD count (IntData 1)
            modifyNodeExtension w ni count'
            writeIORef c $ Cursor False pr cpos st
            ioUpdateOPort w (ni,0) count'
        else do
            ioUpdateOPort w (ni,0) count

execNodeAction c w (PrimNode "button") ni=do
    p<-getNodePosition w ni
    Cursor _ pr cpos _<-readIORef c
    let intst=BoolData $ V.norm (cpos-p)<20 && pr
    -- TODO is it really necessary to store button state internally?
    modifyNodeExtension w ni $ intst
    ioUpdateOPort w (ni,0) intst

execNodeAction c w (PrimNode "display") ni=do
    d<-readIPort w (ni,0)
    modifyNodeExtension w ni d
    
execNodeAction c w (PrimNode "cursor") ni=do
    i<-readIPort w (ni,0)
    p<-getNodePosition w ni
    Cursor _ _ cpos _<-readIORef c
    case i of
        FloatData radius ->
            if V.norm (cpos-p)<radius then let V.Vec2D dx dy=cpos-p in do
                ioUpdateOPort w (ni,1) (FloatData dx)
                ioUpdateOPort w (ni,2) (FloatData dy)
            else do
                ioUpdateOPort w (ni,1) EmptyData
                ioUpdateOPort w (ni,2) EmptyData
        _ -> return ()

execNodeAction _ w (PrimNode name) _=do
    printf "unknown PrimNode: %s\n" name

execNodeAction _ w TapNode ni=do
    i<-readIPort w (ni,0)
    ioUpdateOPort w (ni,1) i
    ioUpdateOPort w (ni,2) i

-- arith
execNodeAction _ w AddNode ni=pure2i1o addD w ni
execNodeAction _ w MulNode ni=pure2i1o mulD w ni
execNodeAction _ w CompareNode ni=pure2i1o cmpD w ni
execNodeAction _ w EqualNode ni=pure2i1o eqD w ni

-- bool
execNodeAction _ w NandNode ni=pure2i1o nandD w ni

execNodeAction _ w MuxNode ni=do
    cond<-readIPort w (ni,0)
    i_true<-readIPort w (ni,1)
    i_false<-readIPort w (ni,2)
    case cond of
        BoolData True -> ioUpdateOPort w (ni,3) i_true
        BoolData False -> ioUpdateOPort w (ni,3) i_false
        _ -> ioUpdateOPort w (ni,3) EmptyData

-- array
execNodeAction _ w LengthNode ni=pure1i1o lengthD w ni
execNodeAction _ w ConcatNode ni=pure2i1o concatD w ni
execNodeAction _ w EncapNode ni=pure1i1o encapD w ni
execNodeAction _ w IndexNode ni=pure2i1o indexD w ni

-- spatial
execNodeAction _ w SearchNode ni=do
    p<-getNodePosition w ni
    i<-readIPort w (ni,0)
    case i of
        FloatData radius -> do
            (World nodes conns _)<-readIORef w
            let
                nis=map snd $ takeWhile ((<radius) . fst) $
                    sortBy (comparing fst) $ map (\(ni,q,_)->(V.norm (p-q),ni)) nodes
                niss=S.fromList nis
            ioUpdateOPort w (ni,1) $ ArrayData $ map NodeData nis
            ioUpdateOPort w (ni,2) $ ArrayData $ map
                (\((sn,sp),(dn,dp))->ArrayData [NodeData sn,IntData (fromIntegral sp),NodeData dn,IntData (fromIntegral dp)]) $ filter
                (\x->S.member (fst $ fst x) niss || S.member (fst $ snd x) niss) $ map fst conns
        _ -> ioUpdateOPort w (ni,1) EmptyData

execNodeAction _ w LocateNode ni=do
    sn<-readIPort w (ni,0)
    case sn of
        NodeData sn -> do
            V.Vec2D x y<-getNodePosition w sn
            ioUpdateOPort w (ni,1) $ FloatData x
            ioUpdateOPort w (ni,2) $ FloatData y
        _ -> do
            ioUpdateOPort w (ni,1) $ EmptyData
            ioUpdateOPort w (ni,2) $ EmptyData
        
execNodeAction _ w MoveNode ni=do
    tg<-readIPort w (ni,0)
    dx<-readIPort w (ni,1)
    dy<-readIPort w (ni,2)
    case (tg,extractFloat dx,extractFloat dy) of
        (NodeData tg,Just dx,Just dy) -> do
            p<-getNodePosition w ni
            dp<-randomN2IO
            modifyNodePosition w tg $ p+dp+V.Vec2D dx dy
        _ -> return ()
    
-- structural
execNodeAction _ w ConnectNode ni=do
    sn<-readIPort w (ni,0)
    sp<-readIPort w (ni,1)
    dn<-readIPort w (ni,2)
    dp<-readIPort w (ni,3)
    case (sn,sp,dn,dp) of
        (NodeData sn,IntData sp,NodeData dn,IntData dp) -> ioConnect w
            (sn,fromIntegral sp) (dn,fromIntegral dp)
        _ -> return ()

execNodeAction _ w DisconnectNode ni=do
    sn<-readIPort w (ni,0)
    sp<-readIPort w (ni,1)
    case (sn,sp) of
        (NodeData sn,IntData sp) -> ioDisconnect w (sn,fromIntegral sp)
        _ -> return ()

execNodeAction _ w (WrapNode False) ni=do
    i<-readIPort w (ni,0)
    case i of
        EmptyData -> ioUpdateOPort w (ni,1) EmptyData
        _ -> do
            Just (p,_)<-getNode w ni
            dp<-randomN2IO
            ni'<-ioInsNormalNode w (p+V.map (*100) dp) $ ConstNode i    
            ioUpdateOPort w (ni,1) $ NodeData ni'

execNodeAction _ w (WrapNode True) ni=do
    i<-readIPort w (ni,0)
    case i of
        EmptyData -> modifyNodeContent w ni $ WrapNode False
        _ -> return ()

execNodeAction _ w (ReplicateNode False) ni=do
    i<-readIPort w (ni,0)
    case i of
        NodeData ni_src -> do
            Just (_,x_src)<-getNode w ni_src
            Just (p,_)<-getNode w ni
            modifyNodeContent w ni $ ReplicateNode True
            dp<-randomN2IO
            ni'<-ioInsNormalNode w (p+V.map (*100) dp) x_src
            ioUpdateOPort w (ni,1) $ NodeData ni'
        _ -> ioUpdateOPort w (ni,1) EmptyData

execNodeAction _ w (ReplicateNode True) ni=do
    i<-readIPort w (ni,0)
    case i of
        EmptyData -> modifyNodeContent w ni $ ReplicateNode False
        _ -> return () -- TODO: ReplicateNode (Maybe NodeId) is preferrable?
    
execNodeAction _ w DeleteNode ni=do
    i<-readIPort w (ni,0)
    case i of
        NodeData ni -> ioRemoveNode w ni
        _ -> return ()


getNode :: IORef World -> NodeId -> IO (Maybe (V.Vec2D,Node))
getNode w nid=do
    World nodes _ _<-readIORef w
    case find ((==nid) . fst3) nodes of
        Nothing -> return Nothing
        Just (_,p,n) -> return $ Just (p,n)

getNodePosition w nid=liftM (fst .fromJust)$ getNode w nid

getNodeExtension :: IORef World -> NodeId -> IO Data
getNodeExtension w ni=do
    World _ _ exts<-readIORef w
    case find ((==ni) . fst) exts of
        Nothing -> error "getNodeExtension: no data"
        Just (_,d) -> return d
    

modifyNodeContent :: IORef World -> NodeId -> Node -> IO ()
modifyNodeContent w nid n=do
    World nodes conns exts<-readIORef w
    writeIORef w $ World (map (\t@(i,p,_)->if i/=nid then t else (nid,p,n)) nodes) conns exts

modifyNodePosition :: IORef World -> NodeId -> V.Vec2D -> IO ()    
modifyNodePosition w ni p=do
    World nodes conns exts<-readIORef w
    writeIORef w $ World (map (\t@(i,_,n)->if i/=ni then t else (i,p,n)) nodes) conns exts

modifyNodeExtension :: IORef World -> NodeId -> Data -> IO ()
modifyNodeExtension w ni d=do
    World nodes conns exts<-readIORef w
    writeIORef w $ World nodes conns (map (\t@(i,_)->if i/=ni then t else (ni,d)) exts)

pure1i1o f w ni=do
    i0<-readIPort w (ni,0)
    ioUpdateOPort w (ni,1) $ f i0

pure2i1o f w ni=do
    i0<-readIPort w (ni,0)
    i1<-readIPort w (ni,1)
    ioUpdateOPort w (ni,2) $ f i0 i1




readIPort :: IORef World -> PortDesc -> IO Data
readIPort w pd=do
    (World nodes conns exts)<-readIORef w
    case find f conns of
        Nothing -> return EmptyData
        Just ((_,_),d) -> return d
    where  f=(==pd) . snd . fst



portHasConnection w pdesc=do
    (World _ conns _)<-readIORef w
    return $ not $ null $ filter (\((s,d),_)->s==pdesc || d==pdesc) conns
        

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

ioRemoveNode :: IORef World -> NodeId -> IO ()
ioRemoveNode w ni=do
    (World nodes conns exts)<-readIORef w
    let dsts=nub $ map (fst . snd) $ filter (\(s,d)->fst s==ni && fst d/=ni) $ map fst conns
    writeIORef w $ World 
        (filter ((/=ni) . fst3) nodes) (filter (\((s,d),_)->fst s/=ni && fst d/=ni) conns) (filter ((/=ni) . fst) exts)

-- | ignores duplicate connection request
ioConnect :: IORef World -> PortDesc -> PortDesc -> IO ()
ioConnect w s d=do
    World nodes conns exts<-readIORef w
    case find ((==c) .fst) conns of
        Nothing -> do
            writeIORef w $ World nodes ((c,EmptyData):conns) exts
        _ -> return ()
    where c=(s,d)

ioDisconnect :: IORef World -> PortDesc -> IO ()
ioDisconnect w p=do
    World nodes conns exts<-readIORef w
    let dsts=map (fst . snd) $ filter (\(s,d)->s==p && d/=p) $ map fst conns
    writeIORef w $ World
        nodes (filter (\((s,d),_)->s/=p && d/=p) conns) exts

drawDebugString :: DrawingArea -> String -> IO ()
drawDebugString da s= do
    dw<-widgetGetDrawWindow da
    renderWithDrawable dw $ do
        setSourceRGBA 0 0 0 0.5
        save
        translate 5 15
        showText s
        restore


draw :: DrawingArea ->  IORef Cursor -> IORef World -> EventM EExpose ()
draw da cursor w=liftIO $ do
    dw<-widgetGetDrawWindow da
    w<-readIORef w
    c<-readIORef cursor
    renderWithDrawable dw $ renderWorld c w

renderWorld cursor (World nodes conns exts)=do
    -- basic structure
    mapM_ node nodes
    mapM_ (\((src,dst),_)->arrow (portPos src) (portPos dst)) conns
    
    case cursor of
        Cursor _ _ cpos (ConnectFrom src) -> arrow (portPos src)  cpos
        _ -> return ()
    
    -- extended  structure
    let me=M.fromList exts
    
    mapM_ (ext me) nodes
    where
        m=M.fromList $ map (\(i,p,n)->(i,(p,n))) nodes
        
        portPos :: PortDesc -> V.Vec2D
        portPos (n,pix)=fst (m M.! n)+V.map (*portRadius) (V.Vec2D (cos theta) (sin theta))
            where theta=pi*0.1*fromIntegral pix
        
        portRadius=30
        
        node (nid,p,t)=do
            -- center
            setSourceRGB 0 0 0
            (arc `applyV` p) 2 0 (2*pi)
            fill
            
            -- desc
            save
            setSourceRGBA 0 0 0 0.5 
            translate `applyV` (p+V.Vec2D 0 (-10))
            showText $ show t
            restore
            
            -- ports
            setSourceRGBA 0 0 0 0.5 
            mapM_ (\pix->(arc `applyV` (portPos (nid,pix))) 2 0 (2*pi) >> fill) [0..5]
            
        
        arrow from to=do
            setSourceRGBA 0.1 0.1 1 0.8
            applyV moveTo from
            applyV lineTo  to
            stroke
            let
                dir=normalizeV $ to-from
                dp=rotateCCW dir
            applyV moveTo to
            applyV lineTo $ to-V.map (*5) dir+V.map (*5) dp
            applyV lineTo $ to-V.map (*5) dir-V.map (*5) dp
            closePath
            fill
                
            
        ext me (nid,p,PrimNode "int upcount")=do
            setSourceRGBA 0 1 0 0.2
            (arc `applyV` p) 20 0 (2*pi)
            fill
            
            setSourceRGB 0 0 0
            save
            translate `applyV` p
            let IntData n=M.findWithDefault EmptyData nid me -- TODO inappropriate default value?
            showText (show n)
            restore
        ext me (nid,p,PrimNode "button")=do
            let BoolData b=M.findWithDefault EmptyData nid me
            setSourceRGBA 0 1 0 $ if b then 0.8 else 0.2
            (arc `applyV` p) 20 0 (2*pi)
            fill
        ext me (nid,p,PrimNode "display")=do
            setSourceRGB 0 0 0
            let d=M.findWithDefault EmptyData nid me
            save
            translate `applyV` p
            showText (show d) -- TODO: hide NodeId intn. representation completely by visualizing them as arrows
            restore
        ext _ _=return ()

normalizeV v=V.map (/(V.norm v)) v
rotateCCW (V.Vec2D x y)=V.Vec2D (-y) x


move :: DrawingArea -> IORef Cursor -> IORef World -> EventM EMotion ()
move widget c w=do
    pos<-liftM (uncurry $ V.Vec2D) eventCoordinates
    Cursor act pr _ st<-liftIO $ readIORef c
    case (act,st) of
        (True,ConnectFrom _) -> 
            liftIO $ writeIORef c $ Cursor False pr pos Idle -- cancel connection
            -- TODO this canceleation behavior is a little bit unintuitive. timer is better.
        _ ->
            liftIO $ writeIORef c $ Cursor False pr pos st

press :: DrawingArea -> IORef Cursor -> IORef World -> EventM EButton ()
press widget c w=do
    pos<-liftM (uncurry $ V.Vec2D) eventCoordinates
    Cursor _ _ _ st<-liftIO $ readIORef c
    liftIO $ writeIORef c $ Cursor True True pos st

release :: DrawingArea -> IORef Cursor -> IORef World -> EventM EButton ()
release darea c w=do
    pos<-liftM (uncurry $ V.Vec2D) eventCoordinates
    Cursor act _ _ st<-liftIO $ readIORef c
    liftIO $ writeIORef c $ Cursor act False pos st

applyV f (V.Vec2D x y)=f x y

-- | Sample from 2d normal distribution
randomN2IO=liftM2 V.Vec2D randomNIO randomNIO

-- | Sample from 1d normal distribution
randomNIO :: IO Double
randomNIO=do
    u<-randomRIO (0,1)
    v<-randomRIO (0,1)
    return $ sqrt (-2*log(u)) * cos (2*pi*v)


sequenceC :: Monad m => [m Bool] -> m Bool
sequenceC []=return False
sequenceC (f:fs)=f `chain` (sequenceC fs)

chain :: Monad m => m Bool -> m Bool -> m Bool
chain f g=do{x<-f; if x then return True else g}

fst3 (a,b,c)=a



