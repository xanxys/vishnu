-- | MsgpackView for vishnu. suitable for interactive use with ghci
module VsThin(ViewBundle,serve,put) where
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vec as V
import Data.Attoparsec.ByteString
import Data.MessagePack
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Text.Printf
import Network
import System.IO


data ViewBundle=ViewBundle (Chan Command)
data Command=PutCommand Float (Float,Float,Float) (Float,Float,Float)

serve :: Int -> IO ViewBundle
serve port=do
    ch<-newChan
    sock<-listenOn (PortNumber $ fromIntegral port)
    forkIO $ forever $ do
        (h,hn,pn)<-accept sock
        hSetBuffering h NoBuffering
        hSetBinaryMode h True
        ch'<-dupChan ch
        forkIO $ endpoint ch' h
    return $ ViewBundle ch

put :: ViewBundle -> Float -> (Float,Float,Float) -> (Float,Float,Float) -> IO ()
put (ViewBundle ch) dt pos col=writeChan ch $ PutCommand dt pos col

-- s->v: ("put",[0.05,(1,4,2),(1,0,1,0.1)])
-- v->s: []
endpoint :: Chan Command -> Handle -> IO ()
endpoint ch h=forever $ do
    com<-readChan ch
    case com of
        PutCommand dt pos rgb -> do
            BSL.hPut h $ pack ("put",(dt,pos,toRGBA rgb))
            []<-liftM fromObject $ getData h :: IO [Double] -- Double is dummy.
            return ()
    where toRGBA (r,g,b)=(r,g,b,1::Float)

getData :: Handle -> IO Object
getData h=BS.hGet h 1 >>= aux . parse get . check1
    where
        aux (Done _ r)=return r
        aux (Partial f)=BS.hGet h 1 >>= aux . (f$) . check1
        check1 bs
            |BS.length bs==1 = bs
            |otherwise = error "null connection"

