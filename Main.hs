-- | vishnu: multi-view 3d playground
--
module Main where
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Reader
import System.IO
import System.Environment
import World
import CUI
import Graphics.UI.Gtk

main=do
    
    
    views<-newMVar []
    ch_action<-newChan
    controller<-forkIO $ processAction ch_action emptyWorld
    
    let
        unassociateView vs=do
            tid<-myThreadId
            return $ filter (/=tid) vs
        associateView vbody=do
            vs<-takeMVar views
            tid<-forkIO $ vbody `finally` modifyMVar_ views unassociateView
            putMVar views $ tid:vs
        waitAllView=do
            threadDelay $ 100*1000
            vs<-readMVar views
            if null vs
                then killThread controller
                else waitAllView
    
    associateView $ runReaderT cuiView (identityFrame,ch_action,associateView)
    waitAllView

