{-# LANGUAGE ScopedTypeVariables #-}
-- | Connect to externally defined view.
--
-- You can't use msgpack-rpc because...
-- * If vishnu become a RPC server (as in MMO), vishnu will lose control of views.
-- * If vishnu become a RPC client, polling and session management will be needed.
--
-- So, we use custom protocol based on msgpack datagrams on TCP stream.
--
-- vishnu connect to external view server via TCP.
-- The TCP connection, a magpackView thread and a session in external view server constitute a view.
-- Address and port (hereafter called the location) of external view server represents a kind of view
-- the server provides.
--
-- server -> vishnu datagram: (method,args)
-- vishnu -> server datagram: results
--
-- examples: all values are in float
--
-- s->v: ("observe",[0.01,(1,2,3)])
-- v->s: [(1.1,0.3,0.2,0.5)]
--
-- s->v: ("put",[0.05,(1,4,2),(1,0,1,0.1)])
-- v->s: []
--
-- s->v: ("mp",["db08:1f:2ac::231a:12",10821])
-- v->s: []
--
-- all real numbers are in float
--
-- when vishnu receives next packet when processing previous packet,
-- the best thing to do is reply asap and start working on next packet.
--
-- any error in syntax result in termination of connection and view.
--
module MsgPack(msgpackView) where
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vec as V
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.Attoparsec
import Data.Attoparsec.ByteString
import Data.MessagePack
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Text.Printf
import Network
import System.IO
import World

-- | Create a view instance using view the service provided at a given port.
msgpackView :: String -> Int -> ViewIO ()
msgpackView host port=do
    sock<-liftIO $ connectTo host (PortNumber $ fromIntegral port)
    liftIO $ hSetBuffering sock NoBuffering >> hSetBinaryMode sock True
    forever $ liftIO (getData sock) >>= dispatchMethod . fromObject >>= \x->liftIO (putData sock x)

dispatchMethod :: (String,Object) -> ViewIO Object
dispatchMethod ("observe",obj)=case tryFromObject obj of
    Right (dt :: Float,pos) -> liftM (\x->toObject [rC4 x]) $ observe (realToFrac dt) (pV3 pos)

dispatchMethod ("put",obj)=case tryFromObject obj of
    Right (dt :: Float,pos,val) -> World.put (realToFrac dt) (pV3 pos) (pC4 val) >> return (toObject ([] :: [Double]))

dispatchMethod ("mp",obj)=case tryFromObject obj of
    Right (host,port) -> spawn identityFrame (msgpackView host port) >> return (toObject ([] :: [Double]))

pV3 :: Object -> V.Vec3D
pV3 (ObjectArray [px,py,pz])=V.Vec3D x y z
    where
        [x,y,z]=map pF [px,py,pz]
        pF (ObjectFloat x)=realToFrac x

pC4 :: Object -> RGBA
pC4 (ObjectArray [pr,pg,pb,pa])=RGBA r g b a
    where
        [r,g,b,a]=map pF [pr,pg,pb,pa]
        pF (ObjectFloat x)=realToFrac x

rC4 :: RGBA -> [Float]
rC4 (RGBA r g b a)=map realToFrac [r,g,b,a]


getData :: Handle -> IO Object
getData h=do
    runResourceT $ sourceHandle h $$ sinkParser get
--getData h=BS.hGet h 1 >>= aux . parse get . check1
--    where
--        aux (Done _ r)=return r
--        aux (Partial f)=BS.hGet h 1 >>= aux . (f$) . check1
--        check1 bs
--            |BS.length bs==1 = bs
--            |otherwise = error "null connection"

putData :: Handle -> Object -> IO ()
putData h x=BSL.hPut h $ pack x

