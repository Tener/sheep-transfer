{-# LANGUAGE ViewPatterns #-}
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
import Text.Printf
import Data.ByteString (ByteString)

--class InjectableWidget w where

class WidgetLike w where
    extractWidget :: w -> Widget

data ConnectionWidget = ConnectionWidget { cw_wholeBox :: VBox, cw_fileTransferBox :: VBox }
instance WidgetLike ConnectionWidget where
    extractWidget = toWidget . cw_wholeBox

data FileTransferWidget = FileTransferWidget { ft_wholeBox :: VBox, ft_progress :: ProgressBar }
instance WidgetLike FileTransferWidget where
    extractWidget = toWidget . ft_wholeBox

postGUIAsync' action = do
  var <- newEmptyMVar
  postGUIAsync $ putMVar var =<< action
  takeMVar var

scrollWindowLookAtBottom sw = do
  adj <- scrolledWindowGetVAdjustment sw
  adjustmentSetValue adj =<< adjustmentGetUpper adj
  return ()


main = do
  initGUI

  logbox <- vBoxNew False 5
  logview <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport logview logbox

  hostbox <- vBoxNew False 5
  hostview <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport hostview hostbox

  connbox <- vBoxNew False 5
  connview <- scrolledWindowNew Nothing Nothing
  scrolledWindowAddWithViewport connview connbox

  mainview <- vBoxNew True 5
  containerAdd mainview logview
  containerAdd mainview hostview
  containerAdd mainview connview

  let log msg = postGUIAsync $ do
         l <- labelNew (Just (show msg))
         boxPackStart logbox l PackNatural 0
         widgetShowAll logbox
         scrollWindowLookAtBottom logview
         return ()
      printlog msg = print msg >> log msg

      newconncb ser host = do
         var <- newEmptyMVar
         postGUIAsync $ do
          printlog ("New connection ",host)
          peers <- getPeers ser
          connwidget <- newConnectionWidget peers host
          containerAdd connbox (extractWidget connwidget)
          widgetShowAll connbox
          putMVar var connwidget
         connwidget <- takeMVar var

         let newFileCb fid fp = do
                ftw <- postGUIAsync' $ newFileTransferWidget fid fp connwidget
                printlog ("new file", fid, fp, host)
                let progress p = postGUIAsync $ progressBarSetFraction (ft_progress ftw) (p/100) >> printlog ("progress", p, fid, fp, host)
                    finished = postGUIAsync $ progressBarSetFraction (ft_progress ftw) 1 >> printlog ("finished", fid, fp, host)
                return (progress,finished)

         return newFileCb

      peerschangedcb ser = postGUIAsync $ do
         printlog "Peers changed..."
         peers <- getPeers ser
         printlog peers

         containerForeach hostbox (containerRemove hostbox)
         let addPeer p = do
               containerAdd hostbox =<< newPeerWidget p
         mapM_ addPeer peers
         widgetShowAll hostbox

      newFileTransferWidget :: FileId -> ByteString -> ConnectionWidget -> IO FileTransferWidget
      newFileTransferWidget fid fpath (cw_fileTransferBox -> ftbox) = do
         let txt = show fpath ++ " " ++ show fid
         l <- labelNew (Just txt)
         pgbar <- progressBarNew
         progressBarSetText pgbar txt
         wholeBox <- vBoxNew False 1
--         containerAdd wholeBox l
         containerAdd wholeBox pgbar
         containerAdd ftbox wholeBox
         widgetShowAll ftbox
         return (FileTransferWidget wholeBox pgbar)

      newConnectionWidget :: [Peer] -> Address -> IO ConnectionWidget
      newConnectionWidget peers addr = do
         -- TODO: drop connections for which the nick is not known. it requires different API.
         let formatMarkupNick nick = markSpan [FontSize SizeLarge, FontWeight WeightBold] (printf "%s (%s)" (escapeMarkup nick) addrMarkup)
             addrMarkup = markSpan [FontSize SizeMedium, FontWeight WeightNormal] (escapeMarkup (show addr))
             txt = case filter (\ (_, a) -> a == addr) peers of
                          [] -> show addr
                          (head -> (nick,_)) -> formatMarkupNick nick
         l <- labelNew Nothing
         labelSetMarkup l txt
         connListBox <- vBoxNew False 1
         exp <- expanderNew "Files"
         expanderSetExpanded exp True
         containerAdd exp connListBox
         wholeBox <- vBoxNew False 1
         containerAdd wholeBox l
         containerAdd wholeBox exp
         return (ConnectionWidget wholeBox connListBox)

      newPeerWidget :: Peer -> IO Widget
      newPeerWidget (nick,addr) = do
         let txt = markSpan [] (printf "%s -- %s" sub0 sub1)
             sub0 = markSpan [FontSize SizeLarge, FontWeight WeightBold] (escapeMarkup nick)
             sub1 = markSpan [FontSize SizeSmall, FontBackground "gray"] (escapeMarkup (show addr))
         l <- labelNew Nothing
         labelSetMarkup l txt
         return (toWidget l)

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
          containerChild := mainview,
          windowAllowGrow := True ]
  widgetShowAll w
  onDestroy w mainQuit
  windowPresent w

--  let setupTray = postGUIAsync $ do
--         trayIcon <- statusIconNewFromFile "sheep.png"
--         statusIconSetTooltip trayIcon "Sheep Transfer"
--         statusIconSetBlinking trayIcon False
--         statusIconSetName trayIcon "Sheep Transfer"
--         statusIconSetVisible trayIcon True
--         on trayIcon statusIconActivate $ do
--                    b <- windowIsActive w
--                    print ("statusIconActivate", b)
--                    if not b then windowIconify w else windowDeiconify w
--                    return ()
--         return ()
--  forkIO (threadDelay (10^6) >> setupTray >> return ())
--  setupTray

  connections <- newTVarIO []

  mainGUI
