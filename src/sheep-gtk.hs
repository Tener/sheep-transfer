{-# LANGUAGE ViewPatterns, MultiParamTypeClasses #-}
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
import Data.IORef
import Network.Socket(getNameInfo)

import System.Directory
import System.FilePath

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

mkAbsolute path = do
  canon <- canonicalizePath =<< (makeRelativeToCurrentDirectory path)
  curr <- getCurrentDirectory
  canonicalizePath (curr </> canon)

mkSheepConf logbox logview hostbox hostview connbox connview mainview servRef =
  (log, serverConf)
   where
      serverConf ser = def { confNewConnCb = newconncb ser, peersChangedCb = peerschangedcb ser }
      log msg = postGUIAsync $ do
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

         let newFileCb fid fp@(file1,file2,fname) = do
                ftw <- postGUIAsync' $ do
                                  ft <- newFileTransferWidget fid fname connwidget
                                  buttonSaveAs <- buttonNewWithLabel "Save as..."
                                  on buttonSaveAs buttonActivated $ do
                                        let resp = [("Wybierz plik",ResponseOk)]
                                                    
                                        dialog <- fileChooserDialogNew (Just "Wybierz plik") Nothing FileChooserActionOpen resp
                                        dResp <- dialogRun dialog
                                        print dResp
                                        fname <- fileChooserGetFilename dialog
                                        widgetHide dialog
                                        print fname

                                        case fname of
                                          Nothing -> return ()
                                          Just fn -> copyFile file2 fn
                                                      
                                  canon <- mkAbsolute file2
                                  buttonOpen <- linkButtonNewWithLabel ("file://" ++ canon) "Open..."
                                  let wb = ft_wholeBox ft
                                  containerAdd wb buttonSaveAs
                                  containerAdd wb buttonOpen
                                  widgetShowAll wb
                                  return ft
                                  

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
         b <- buttonNew
         on b buttonActivated $ do
           let resp = [("Wybierz plik",ResponseOk)]
                       
           dialog <- fileChooserDialogNew (Just "Wybierz plik") Nothing FileChooserActionOpen resp
           dResp <- dialogRun dialog
           print dResp
           fname <- fileChooserGetFilename dialog
           widgetHide dialog

           print fname
           serv <- readIORef servRef
           (Just connAddr,_) <- getNameInfo [] True False addr
           print connAddr
           case fname of
             Nothing -> return ()
             Just fn -> forkIO (sendFilePeer serv fn connAddr (\d -> print ("gui:prog",d)) (print "gui:done")) >> return ()

         containerAdd b l
         return (toWidget b)



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

  servRef <- newIORef (error "empty ref")

  let (logMsg,sheepConf) = mkSheepConf logbox logview hostbox hostview connbox connview mainview servRef
  serv <- fixIO (\ ser -> startServer (sheepConf ser)) :: IO SheepServer
  writeIORef servRef serv
  debugSheepServer serv

  forkIO (forever $ do
            logMsg "Ping.."
            threadDelay (10^8))
  
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

  connections <- newTVarIO []
  let showMainAll = postGUIAsync $ do
                      widgetShowAll w
  forkIO $ forever (threadDelay (10^5) >> showMainAll)
  mainGUI
  stopServer serv
