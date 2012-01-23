{-# LANGUAGE TupleSections #-}
-- | expose world via 'BiParticle'
--
-- why "biparticle"?
--
-- externally viewed (i.e. from our world), it's a piece of program that somehow
-- connects to vishnu and pull information out.
--
-- internally viewed (i.e. from vishnu), it's a particle that somehow "decides" on its own behavior.
--
-- So the same thing can be viewed as a particle from both sides, thus the name bi-particle.
--
module World where
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
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
import System.Random
import System.Environment
import Text.Printf


-- | 'BiParticleIO' defines external behavior of 'BiParticle'
--
-- Definition of BiParticle:
-- external -> internal:
--  * feed continuous velocity and angular velocity
--  * spawn new TransPariticle with externally defined behavior
--
-- internal -> external:
--  * stream of discrete photons
--
-- However, there are multiple way to expose this model in real programming language.
-- Here, we define stateful accumulative interface.
--  * get_photon
--  * set_velocity
--  * set_rotation
--  * spawn
--
-- set_velocity and set_rotation are separated merely for convenience, and each BiPariticle
-- should keep the values on their own if needed.
-- 
type BiParticleIO a=ReaderT (Chan (ThreadId,Action)) IO a

-- | emit a single photon
emit_photon :: (V.Vec3D,PhotonType) -> BiParticleIO ()
emit_photon ph=requestAction $ EmitPhoton $ first (V.map realToFrac) ph

-- | Get all previously absorbed photons and empty the photon accumulator.
get_photon :: BiParticleIO [(V.Vec3D,PhotonType)]
get_photon=do
    mv<-liftIO $ newEmptyMVar
    requestAction $ GetPhoton mv
    liftM (map $ first $ V.map realToFrac) $ liftIO $ readMVar mv

-- | Set velocity and keep it until further call to this function.
set_velocity :: V.Vec3D -> BiParticleIO ()
set_velocity v=requestAction $ SetVelocity $ V.map realToFrac v

-- | Set angular velocity and keep it until further call to this function.
set_rotation :: V.Vec3D -> BiParticleIO ()
set_rotation r=requestAction $ SetRotation $ V.map realToFrac r

-- | Spawn new 'BiParticle' that shares current position. velocity will be 0.
spawn :: BiParticleIO () -> BiParticleIO ()
spawn f=requestAction $ Spawn f

requestAction :: Action -> BiParticleIO ()
requestAction a=do
    ch<-ask
    ti<-liftIO $ myThreadId
    liftIO $ writeChan ch (ti,a)

-- | shared model is incorrect, but a reasonable approximation.
--
data SharedWorld=SharedWorld (M.Map V.Vec3I [BiParticle]) (M.Map V.Vec3I [Photon])

data BiParticle=BiParticle BiParticleS ThreadId

data BiParticleS=BiParticleS
    !V.Vec3F -- ^ fractional position (internal)
    !V.Vec3F -- ^ velocity (external, used for interfacing)
    !QD -- ^ orientation (internal)
    !V.Vec3F -- ^ angular velocity (external)
    [(V.Vec3F,PhotonType)] -- ^ absorbed photons

data Photon=Photon
    V.Vec3F -- ^ direction
    V.Vec3F -- ^ fractional coordinate [-0.5,0.5]^3
    PhotonType -- ^ color of photon

-- | Color of photon is discretized. And there's no such concept as wavelength,
-- since vishnu is based on geometric optics.
data PhotonType=Red|Green|Blue





type Absorption=[(V.Vec3I,Photon)]
type Emission=[(V.Vec3I,Photon)]

stepLight :: Emission -> SharedWorld -> (SharedWorld,Absorption)
stepLight emission=(first $ emitLight emission) . (first propagateLight) . absorbLight

absorbLight :: SharedWorld -> (SharedWorld,Absorption)
absorbLight (SharedWorld pas phs)=(SharedWorld pas phs',mmAssocs dphs)
    where
        (dphs,phs')=M.partitionWithKey (const . occupied) phs
        occupied=not . null . flip (M.findWithDefault []) pas


propagateLight :: SharedWorld -> SharedWorld
propagateLight (SharedWorld pas phs)=SharedWorld pas phs'
    where
        phs'=M.filterWithKey (const . keepIndex) $ mmUpdate propagatePhoton phs
        keepIndex v=V.normSq v<500^2


emitLight :: Emission -> SharedWorld -> SharedWorld
emitLight es (SharedWorld pas phs)=SharedWorld pas phs'
    where
        phs'=foldl' (\m (i,p)->mmInsert i p m) phs es


-- TODO: fix minor numerical error in calculation of pos' (+0.01 thing)
propagatePhoton :: (V.Vec3I,Photon) -> (V.Vec3I,Photon)
propagatePhoton (pos,Photon dir fpos col)=(pos+pi',Photon dir pf' col)
    where
        (pi',pf')=decomposePos $ fpos+V.map (*(dt+0.01)) dir
        dt=V.minimum dts
        dts=(V.map ((*0.5) . signum) dir - fpos)/dir


data Action
    =EmitPhoton (V.Vec3F,PhotonType)
    |GetPhoton (MVar [(V.Vec3F,PhotonType)])
    |SetVelocity V.Vec3F
    |SetRotation V.Vec3F
    |Spawn (BiParticleIO ())
    |Die



-- | move 'BiParticle's and return particles that crossed cell border
moveBiParticles :: Float -> SharedWorld -> (SharedWorld,[(ThreadId,V.Vec3I)])
moveBiParticles dt (SharedWorld pas phs)=(SharedWorld pas' phs,diff)
    where
        pas'=mmFromList $ map (\(i,(di,p))->(i+di,p)) ps
        diff=map (\(i,(di,BiParticle _ tid))->(tid,i+di)) dps
        dps=filter ((/=V.Vec3I 0 0 0) . fst . snd) ps
        ps=map (second $ moveBiParticle dt) $ mmAssocs pas

moveBiParticle :: Float -> BiParticle -> (V.Vec3I,BiParticle)
moveBiParticle dt (BiParticle (BiParticleS p p' o o' ps) tid)=
    (pn_i,BiParticle (BiParticleS pn_f p' on o' ps) tid)
    where
        (pn_i,pn_f)=decomposePos $ p+V.map (*dt) p'
        on=o -- TODO: rotate o by dt*o'


extractEmission :: M.Map ThreadId V.Vec3I -> [(ThreadId,Action)] -> IO Emission
extractEmission tm=sequence . mapMaybe f
    where
        f (ti,EmitPhoton (dir,ty))=Just $ do
            fpos<-liftM3 V.Vec3F randomNIO randomNIO randomNIO
            return (tm M.! ti,Photon dir fpos ty)
        f _=Nothing

distributePhoton :: Absorption -> SharedWorld -> IO SharedWorld
distributePhoton=flip $ foldM f
    where
        f (SharedWorld pas phs) (ix,Photon dir _ ty)=do
            let ps=pas M.! ix
            let addPhoton (BiParticle (BiParticleS p p' o o' ps) ti)=BiParticle (BiParticleS p p' o o' ((dir,ty):ps)) ti
            i<-randomRIO (0,length ps-1)
            let ps'=map (\(p0,i0)->if i0/=i then p0 else addPhoton p0) $ zip ps [0..]
            let pas'=M.insert ix ps' pas
            return $ SharedWorld pas' phs

execAction
    :: Chan (ThreadId,Action)
    -> (SharedWorld,M.Map ThreadId V.Vec3I)
    -> (ThreadId,Action)
    -> IO (SharedWorld,M.Map ThreadId V.Vec3I)
execAction ch tp@((SharedWorld pas phs),tm) (ti,ac)=case ac of
    EmitPhoton _ -> return tp
    GetPhoton mv -> do
        sc<-tryPutMVar mv photons
        unless sc $ error "execAction: MVar already occupied"
        let bp=BiParticle (BiParticleS p p' o o' []) ti
        return (SharedWorld (mmInsert ix bp pas') phs,tm)
    SetVelocity np' -> do
        let bp=BiParticle (BiParticleS p np' o o' photons) ti
        return (SharedWorld (mmInsert ix bp pas') phs,tm)
    SetRotation no' -> do
        let bp=BiParticle (BiParticleS p p' o no' photons) ti
        return (SharedWorld (mmInsert ix bp pas') phs,tm)
    Spawn f -> do
        tid<-forkIO $ runReaderT f ch `finally` (myThreadId >>= writeChan ch . (,Die))
        let bs=BiParticleS p (V.Vec3F 0 0 0) o (V.Vec3F 0 0 0) []
        let bp=BiParticle bs tid
        return (SharedWorld (mmInsert ix bp pas) phs,M.insert tid ix tm)
    Die -> do
        return (SharedWorld pas' phs,M.delete ti tm)
    where
        pas'=mmFilterAt (\(BiParticle _ i)->i/=ti) ix pas
        ([BiParticle (BiParticleS p p' o o' photons) _],ps)=
            partition (\(BiParticle _ i)->i==ti) $ M.findWithDefault [] ix pas
        ix=tm M.! ti

-- | modify world at 10 ms tick
processAction :: Chan (ThreadId,Action) -> M.Map ThreadId V.Vec3I -> SharedWorld -> IO ()
processAction ch tm w=do
    t_tick<-getCurrentTime
    acs<-readChanAll ch
    
    -- commit all actions & tick world
    emission<-extractEmission tm acs
    let (w',absorption)=stepLight emission w
    w''<-distributePhoton absorption w'
    (w''',tm')<-foldM (execAction ch) (w'',tm) acs
    let (w'''',rmap)=moveBiParticles 0.01 w'''
    let tm''=M.union (M.fromList rmap) tm'
    
    -- adjust tick duration
    t_now<-getCurrentTime
    let t_elapsed=realToFrac (t_now `diffUTCTime` t_tick)
    threadDelay $ max 0 $ floor $ 1e6*(10e-3-t_elapsed)
    
    unless (M.null tm'') $ processAction ch tm'' w''''

createWorld :: BiParticleIO () -> IO ()
createWorld f=do
    ch<-newChan
    tid<-forkIO $ runReaderT f ch `finally` (myThreadId >>= writeChan ch . (,Die))
    let bs=BiParticleS (V.Vec3F 0 0 0) (V.Vec3F 0 0 0) identityQD (V.Vec3F 0 0 0) []
    let w=SharedWorld (M.fromList [(V.Vec3I 0 0 0,[BiParticle bs tid])]) M.empty
    processAction ch (M.fromList [(tid,V.Vec3I 0 0 0)]) w

-- | read all currently available values from a Chan in FIFO order. Other thread mustn't read.
readChanAll :: Chan a -> IO [a]
readChanAll ch=aux []
    where
        aux xs=do
            e<-isEmptyChan ch
            if e then return (reverse xs) else readChan ch >>= (aux . (:xs))

-- | Sample from the normal distribution
randomNIO :: IO Float
randomNIO=do
    u<-randomRIO (0,1)
    v<-randomRIO (0,1)
    return $ sqrt (-2*log(u)) * cos (2*pi*v)

mmFromList :: Ord k => [(k,a)] -> M.Map k [a]
mmFromList=foldl' (\m (k,v)->M.insertWith (++) k [v] m) M.empty

mmFilterAt :: Ord k => (a -> Bool) -> k -> M.Map k [a] -> M.Map k [a]
mmFilterAt f k m=M.update update k m
    where
        update xs
            |null xs' = Nothing
            |otherwise = Just xs'
            where xs'=filter f xs

mmUpdate :: Ord k => ((k,a)->(k,a)) -> M.Map k [a] -> M.Map k [a]
mmUpdate f m=mmFromList $ concatMap (\(i,xs)->map (curry f i) xs) $ M.assocs m

mmInsert :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
mmInsert k v=M.insertWith (++) k [v]

mmAssocs :: Ord k => M.Map k [a] -> [(k,a)]
mmAssocs=concatMap (\(k,vs)->map (k,) vs) . M.assocs



-- | decompose coordinate into cell index and displacement
decomposePos :: V.Vec3F -> (V.Vec3I,V.Vec3F)
decomposePos p=(pi,p-V.map fromIntegral pi)
    where pi=V.map round p


-- | semi-stationary frame
data Frame=Frame !V.Vec3D !QD deriving(Show)

identityFrame :: Frame
identityFrame=Frame (V.Vec3D 0 0 0) identityQD


-- | Quaternion Double
data QD=QD !V.Vec3D !Double deriving(Show,Eq)

instance Num QD where
    (QD v0 w0) + (QD v1 w1)=QD (v0+v1) (w0+w1)
    (QD v0 w0) * (QD v1 w1)=QD
        (V.map (w0*) v1 + V.map (w1*) v0 + v0 `cross3D` v1) (w0*w1 - v0 `V.dot` v1)
    abs=undefined
    signum=undefined
    fromInteger=undefined

identityQD :: QD
identityQD=QD (V.Vec3D 0 0 0) 1

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

