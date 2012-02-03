-- | temporarily modified for experimenting with 2-d visual programming language
{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleContexts #-}
module IPR(iprView) where
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
-- import Control.Exception
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
import System.Exit
import System.Random
-- import World



iprView :: IO ()
iprView=do
    initGUI
    
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
        
        n0<-ioInsSpecialNode w (V.Vec2D 50 50) (IntData 0) "int src"
        n1<-ioInsSpecialNode w (V.Vec2D 50 100) (IntData 0) "int src"
        n2<-ioInsSpecialNode w (V.Vec2D 50 150) EmptyData "sink"
        
        mapM_ (\(i,t)->ioInsNormalNode w (V.Vec2D 50 (200+fromIntegral i*50)) t)
            $ zip [0..] [ConstNode $ IntData 1,ConstNode $ FloatData 0,ConstNode $ FloatData 100,
                            AddNode,
                            SearchNode,IndexNode,TapNode,
                            ReplicateNode,WrapNode,MuxNode,EqualNode,NandNode]
    
    cursor<-newIORef Idle
    
    -- window close
    onDestroy window $ mainQuit -- window `on` destroyEvent doesn't work 
    
    -- node control w/ mouse
    widgetAddEvents darea [PointerMotionMask]
    darea `on` motionNotifyEvent $ tryEvent $ move darea cursor w
    darea `on` exposeEvent $ tryEvent $ draw darea cursor w
    darea `on` buttonPressEvent $ tryEvent $ press darea cursor w
    
    widgetShowAll window
    mainGUI
    
    -- save
    printf "saving state to %s\n" path
    writeFile path . show =<< readIORef w
    where path="world.image"

-- TODO: moving cursor position around is not clean...
data Cursor=Idle|Grab NodeId|ConnectFrom PortDesc V.Vec2D 

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

-- | operational model:
-- (array is very different from others... since its size is not bounded)
-- changing array to tuple won't help. they will just get bigger (because of overhead).
--
-- nodes are stateless. connections are.
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
data Node
    =ConstNode Data -- | *
    |PrimNode String -- polymorphic node. wait. PrimNode isn't enough for everything?
    |TapNode -- * | *,*
--    |IdNode -- * | *
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
    |WrapNode -- * | Node (create ConstNode, since it's parametric)
    |ReplicateNode -- Node | Node
    |DeleteNode -- Node |
    deriving(Show,Read)

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
addD _ _=EmptyData

-- | only Int,Bool,Node can be compared for equality
eqD (IntData x) (IntData y)=BoolData $ x==y
eqD (BoolData x) (BoolData y)=BoolData $ x==y
eqD (NodeData x) (NodeData y)=BoolData $ x==y
eqD _ _=EmptyData


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

{-
instance Show NodeId where
    show (NodeId p0 p1)=printf "%016x%016x" p0 p1

instance Read NodeId where
    readsPrec _ s=[(NodeId (read $ "0x"++take 16 n) (read $ "0x"++drop 16 n),x)] -- TODO: exception
        where (n,x)=splitAt 32 s
-}

instance Random Word64 where
    randomIO=liftM fromIntegral $ (randomRIO (0,0xffffffffffffffffffffffff) :: IO Integer)





type PortDesc=(NodeId,PortId)
type PortId=Int




ioUpdateOPort :: IORef World -> PortDesc -> Data -> IO ()
ioUpdateOPort w port d=do
    (World nodes conns exts)<-readIORef w
    case find f conns of
        Nothing -> return () -- no outgoing connection
        Just ((_,to),_) -> do
            writeIORef w $ World nodes (map (\x->if f x then (fst x,d) else x) conns) exts
            ioUpdateAnyIPort w $ fst to -- make this asynchronous
    where f=(==port) . fst . fst


ioUpdateAnyIPort :: IORef World -> NodeId -> IO ()
ioUpdateAnyIPort w ni=do
    (World nodes conns exts)<-readIORef w
    case find (\(i,p,n)->i==ni) nodes of
        Nothing -> putStrLn "dangling connection"
        Just (_,p,n) -> do
            execNodeAction w n ni


execNodeAction w (PrimNode "sink") ni=do
    d<-readIPort w (ni,0)
    modifyIORef w $ \(World nodes conns exts)->World nodes conns (map (\t@(i,_)->if ni/=i then t else (i,d)) exts)

execNodeAction w (ConstNode d) ni=
    ioUpdateOPort w (ni,0) d

execNodeAction w AddNode ni=do
    i0<-readIPort w (ni,0)
    i1<-readIPort w (ni,1)
    ioUpdateOPort w (ni,2) $ addD i0 i1

execNodeAction w TapNode ni=do
    i<-readIPort w (ni,0)
    ioUpdateOPort w (ni,1) i
    ioUpdateOPort w (ni,2) i

execNodeAction w NandNode ni=do
    i0<-readIPort w (ni,0)
    i1<-readIPort w (ni,1)
    ioUpdateOPort w (ni,2) $ nandD i0 i1

execNodeAction w MuxNode ni=do
    cond<-readIPort w (ni,0)
    i_true<-readIPort w (ni,1)
    i_false<-readIPort w (ni,2)
    case cond of
        BoolData True -> ioUpdateOPort w (ni,3) i_true
        BoolData False -> ioUpdateOPort w (ni,3) i_false
        _ -> ioUpdateOPort w (ni,3) EmptyData

execNodeAction w EqualNode ni=do
    i0<-readIPort w (ni,0)
    i1<-readIPort w (ni,1)
    ioUpdateOPort w (ni,2) $ eqD i0 i1

execNodeAction w IndexNode ni=do
    ar<-readIPort w (ni,0)
    ix<-readIPort w (ni,1)
    ioUpdateOPort w (ni,2) $ indexD ar ix

execNodeAction w SearchNode ni=do
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

execNodeAction w MoveNode ni=do
    tg<-readIPort w (ni,0)
    dx<-readIPort w (ni,1)
    dy<-readIPort w (ni,2)
    case (tg,extractFloat dx,extractFloat dy) of
        (NodeData tg,Just dx,Just dy) -> do
            World nodes conns exts<-readIORef w
            let Just (_,p,_)=find ((==tg) . fst3) nodes
            setNodePosition w tg $ p+V.Vec2D dx dy
        _ -> return ()
    

execNodeAction w ReplicateNode ni=do
    i<-readIPort w (ni,0)
    case i of
        NodeData ni -> do
            (World nodes conns exts)<-readIORef w
            case find ((==ni) . fst3) nodes of
                Just (_,p,x) -> do
                    ni'<-generateNodeId
                    dp<-liftM2 V.Vec2D randomNIO randomNIO
                    writeIORef w $ World ((ni',p+V.map (*100) dp,x):nodes) conns exts
                    ioUpdateOPort w (ni,1) $ NodeData ni'
                Nothing -> ioUpdateOPort w (ni,1) EmptyData
        _ -> ioUpdateOPort w (ni,1) EmptyData

execNodeAction w DeleteNode ni=do
    i<-readIPort w (ni,0)
    case i of
        NodeData ni -> ioRemoveNode w ni
        _ -> return ()

execNodeAction w ConnectNode ni=do
    sn<-readIPort w (ni,0)
    sp<-readIPort w (ni,1)
    dn<-readIPort w (ni,2)
    dp<-readIPort w (ni,3)
    case (sn,sp,dn,dp) of
        (NodeData sn,IntData sp,NodeData dn,IntData dp) -> ioConnect w (sn,fromIntegral sp) (dn,fromIntegral dp)
        _ -> return ()

            
execNodeAction w _ _=return ()



getNodePosition w ni=do
    World nodes _ _<-readIORef w
    let Just (_,p,_)=find ((==ni) . fst3) nodes
    return p

-- TODO: should this function cause updateAnyIPort of SearchNode --> it's impossible?
setNodePosition w ni p=do
    World nodes conns exts<-readIORef w
    let nodes'=map (\t@(i,_,n)->if i/=ni then t else (i,p,n)) nodes
    writeIORef w $ World nodes' conns exts
    



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
    mapM_ (ioUpdateAnyIPort w) dsts

-- | ignores duplicate connection request
ioConnect :: IORef World -> PortDesc -> PortDesc -> IO ()
ioConnect w s d=do
    World nodes conns exts<-readIORef w
    case find ((==c) .fst) conns of
        Nothing -> do
            writeIORef w $ World nodes ((c,EmptyData):conns) exts
            ioUpdateAnyIPort w $ fst s
        _ -> return ()
    where c=(s,d)

ioDisconnect :: IORef World -> PortDesc -> IO ()
ioDisconnect w p=do
    World nodes conns exts<-readIORef w
    let dsts=map (fst . snd) $ filter (\(s,d)->s==p && d/=p) $ map fst conns
    writeIORef w $ World
        nodes (filter (\((s,d),_)->s/=p && d/=p) conns) exts
    mapM_ (ioUpdateAnyIPort w) dsts

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
        ConnectFrom src cpos -> arrow (portPos src)  cpos
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
        
        applyV f (V.Vec2D x y)=f x y
        
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
                
            
        ext me (nid,V.Vec2D x y,PrimNode "int src")=do
            setSourceRGB 0 0 0
            let IntData n=me M.! nid
            setLineWidth 1
            rectangle (x-30) (y-12.5) 60 25
            stroke
            save
            translate x y
            showText (show n)
            restore
        ext me (nid,V.Vec2D x y,PrimNode "sink")=do
            setSourceRGB 0 0 0
            let d=me M.! nid
            setLineWidth 1
            rectangle (x-30) (y-12.5) 60 25
            stroke
            save
            translate x y
            showText (show d) -- problematic. show NodeId graphically and hide NodeId completely
            restore
        ext _ _=return ()

normalizeV v=V.map (/(V.norm v)) v
rotateCCW (V.Vec2D x y)=V.Vec2D (-y) x


move :: DrawingArea -> IORef Cursor -> IORef World -> EventM EMotion ()
move widget cursor w=do
    cst<-liftIO $ readIORef cursor
    case cst of
        Idle -> return ()
        Grab nid -> do
            (px,py)<-eventCoordinates
            (World nodes conns exts)<-liftIO $ readIORef w
            let nodes'=map (\t@(i,pos,n)->if nid/=i then t else (i,V.Vec2D px py,n)) nodes
            liftIO $ writeIORef w $ World nodes' conns exts
            liftIO $ widgetQueueDraw widget
        ConnectFrom srcport pos -> do
            (px,py)<-eventCoordinates
            liftIO $ writeIORef cursor $ ConnectFrom srcport $ V.Vec2D px py
            liftIO $ widgetQueueDraw widget

press :: DrawingArea -> IORef Cursor -> IORef World -> EventM EButton ()
press widget dragging w=do
    pos<-liftM (uncurry $ V.Vec2D) eventCoordinates
    liftIO $ do
        (World nodes _ _)<-readIORef w
        dr<-readIORef dragging
        case dr of
            Grab ni -> drop ni
            ConnectFrom _ _ -> do
                handled<-sequenceC $ map (conn pos) nodes
                unless handled $ cancelConn
                widgetQueueDraw widget
            Idle -> do
                handled<-sequenceC $ map (activate pos) nodes
                when handled $ widgetQueueDraw widget
    where
        startConn sp cpos=writeIORef dragging $ ConnectFrom sp cpos
        cancelConn=writeIORef dragging $ Idle
        finishConn dp=do
            d<-readIORef dragging
            case d of
                ConnectFrom sp _ -> ioConnect w sp dp >> cancelConn
                _ -> error "finishConn: impossible state"
        drop ni=writeIORef dragging Idle
        pick ni=writeIORef dragging $ Grab ni
        
        conn pos (nid,c,n)=sequenceC $ map (connPort (pos-c) nid) [0..5]
        
        
        activate pos (nid,c,n)=checkPorts pos (pos-c) nid `chain` checkNode (pos-c) nid n
        
        checkPorts cpos d nid=sequenceC $ map (checkPort cpos d nid) [0..5]
        
        connPort d nid pix=do
            let
                center=V.map (*30) (V.Vec2D (cos theta) (sin theta))
                theta=pi*0.1*fromIntegral pix
            
            if V.norm (center-d)<5
                then finishConn (nid,pix) >> return True
                else return False
        
        checkPort cpos d nid pix=do
            let
                center=V.map (*30) (V.Vec2D (cos theta) (sin theta))
                theta=pi*0.1*fromIntegral pix
            
            if V.norm (center-d)<5
                then do
                    ex<-portHasConnection w (nid,pix)
                    if ex then ioDisconnect w (nid,pix) else startConn (nid,pix) cpos
                    return True
                else return False
            
        
        checkNode (V.Vec2D dx dy) nid (PrimNode _)
            |abs dx<5 && abs dy<5 = do
                    (World nodes conns exts)<-readIORef w
                    let
                        exts'=map (\(i,n)->if i/=nid then (i,n) else (i,addD n (IntData 1))) exts
                        Just ttt=find ((==nid) . fst) exts'
                    writeIORef w $ World nodes conns exts'
                    ioUpdateOPort w (nid,0) $ snd ttt
                    widgetQueueDraw widget
                    return True
            |abs dx<30 && abs dy<12.5 = pick nid >> return True
            |otherwise = return False
        checkNode (V.Vec2D dx dy) nid _
            |abs dx<5 && abs dy<5 = pick nid >> return True
            |otherwise = return False


-- | Sample from the normal distribution
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


