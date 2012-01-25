import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad

main=do
    mapM_ (\i->forkIO $ mapM_ (\j->threadDelay 1000) [1..500] >> print i) [1..10000]
    threadDelay $ 20*1000*1000
