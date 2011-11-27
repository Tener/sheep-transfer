module Main where

import Graphics.UI.Gtk

import DataTypes
import Serialize ()
import SheepServer
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Default
import System.IO (fixIO)

main = do
  initGUI

  logbox <- vBoxNew False 5
  logview <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport logview logbox

  let log msg = postGUISync $ do
         l <- labelNew (Just (show msg))
         boxPackStart logbox l PackNatural 0 
         widgetShowAll logbox
         return ()
      printlog msg = print msg >> log msg
      newconncb ser host = do
         let newFileCb fid fp = do
                let progress i = printlog ("progress", i, fid, fp, host)
                    finished = printlog ("finished", fid, fp, host)
                printlog ("new file", fid, fp, host)
                return (progress,finished)
         printlog host
         return newFileCb

      peerschangedcb ser = do
         printlog "Peers changed..."
         printlog =<< (getPeers ser)
     
      sheepConf ser = def { confNewConnCb = newconncb ser, peersChangedCb = peerschangedcb ser } 

  forkIO (forever $ do
            log "Ping.."
            threadDelay (10^8))

  serv <- fixIO (\ ser -> startServer (sheepConf ser)) :: IO SheepServer
  debugSheepServer serv

  w <- windowNew
  windowSetDefaultSize w 200 200
  windowSetPosition w WinPosCenterAlways

  -- mainWindowChild <- buttonNewWithLabel "Transfer file..."

  set w [ containerBorderWidth := 10,
          windowTitle := "Sheep Transfer (GTK)",
          containerChild := logview,
          windowAllowGrow := True ]
  widgetShowAll w
  onDestroy w mainQuit
  windowPresent w

  let setupTray = postGUIAsync $ do
         trayIcon <- statusIconNewFromFile "sheep.png"
         statusIconSetTooltip trayIcon "Sheep Transfer"
         statusIconSetBlinking trayIcon False
         statusIconSetName trayIcon "Sheep Transfer"
         statusIconSetVisible trayIcon True
         on trayIcon statusIconActivate $ do
                    b <- windowIsActive w
                    print ("statusIconActivate", b)
                    if not b then windowIconify w else windowDeiconify w
                    return ()
         return ()

--  forkIO (threadDelay (10^6) >> setupTray >> return ())
--  setupTray

  connections <- newTVarIO []

  mainGUI
