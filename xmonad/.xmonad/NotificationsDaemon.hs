{-# LANGUAGE OverloadedStrings, RankNTypes, KindSignatures, FlexibleContexts #-}
module Plugins.NotificationDaemon where

-- xmobar plugin API
import Plugins

import DBus.Bus
import DBus.Client
import DBus.Constants

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (unless, forever)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (liftIO)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Word (Word32)

-- Tweakable stuff.  The formatting color could be extracted out into
-- a configuration option

buildMessage :: Notification -> [Char]
buildMessage itm = noteSummary itm ++ body
  where body = case noteBody itm of
          "" -> ""
          body' -> " / " ++ body'

formatMessage :: forall a. (IsString [a]) => [a] -> [a]
formatMessage itm = "<fc=#FFFF00>" ++ itm ++ "</fc>"

type NotificationState = MVar EventState
data Notification = Notification { noteAppName :: String
                                 , noteReplaceId :: Word32
                                 , noteSummary :: String
                                 , noteBody :: String
                                 , noteExpireTimeout :: Int32
                                 }
                    deriving (Show, Eq)
data EventToken = EventToken deriving (Show)
data EventState = EventState { notifyQueue :: Queue Notification
                             , notifyIdSource :: Word32
                             }
                         deriving (Show)

-- Arguments are: Max Message Len (chars), Animation Timestep (ms),
-- Max note timeout (ms), Def note timeout (ms)
data NotificationDaemon = NotificationDaemon Int32 Int32 Int32 Int32
                        deriving (Show, Read)

instance Exec NotificationDaemon where
  alias _ = "NotificationDaemon"
  start conf cb = daemonMain conf cb

-- This implementation of the Freedesktop Notification Spec
-- (apparently not a formal one) complies to version 1.1 of the spec:
-- http://people.canonical.com/~agateau/notifications-1.1/spec/index.html
daemonInterface:: NotificationState -> MVar Bool -> Int32 -> Int32 -> Object
daemonInterface state box maxTimeout defTimeout =
  object [ ("org.freedesktop.Notifications",
            interface [ ("GetServerInformation", getServerInformation)
                      , ("GetCapabilities", getCapabilities)
                      , ("CloseNotification", closeNotification state)
                      , ("Notify", notify state box maxTimeout defTimeout)
                      ])
         ]

getServerInformation :: Member
getServerInformation = method "" "ssss" $ \ctx -> replyReturn ctx retVal
  where appName = "notibar" :: String
        vendorName = "nochair.net" :: String
        versionNumber = "0.0.1" :: String
        specNumber = "1.1" :: String
        retVal = [ toVariant appName
                 , toVariant vendorName
                 , toVariant versionNumber
                 , toVariant specNumber
                 ]

getCapabilities :: Member
getCapabilities = method "" "as" $ \ctx -> case arr of
    Just a -> replyReturn ctx [ toVariant a ]
    Nothing -> replyError ctx errorFailed [toVariant errMsg]
  where bodyCap = "body" :: String
        markupCap = "body-markup" :: String
        arr = arrayFromItems DBusString [ toVariant bodyCap, toVariant markupCap ]
        errMsg :: String
        errMsg = "Could not construct return array"

-- This implementation of CloseNotification is not quite correct.  It
-- will not close an active notification, just remove pending ones
-- from the queue.  This is not a big deal since this implementation
-- does not support notifications with unlimited lifetimes.
closeNotification :: NotificationState -> Member
closeNotification state = method "u" "" $ \ctx -> do
  let [ arg ] = methodCtxBody ctx
      msgId :: Word32
      msgId = fromMaybe 0 $ fromVariant arg

  if msgId > 0
    then tryRemoveNote ctx msgId
    else replyError ctx errorFailed []
  where tryRemoveNote ctx msgId = do
          removedNote <- removeNoteById state msgId
          unless removedNote (replyError ctx errorFailed [])

notify :: NotificationState -> MVar Bool -> Int32 -> Int32 -> Member
notify state box maxTimeout defTimeout = method "susssasa{sv}i" "u" $ \ctx -> do
  -- Ignore icon, actions, and hints
  let [ appName_, replaceId_, _, summary_, body_, _, _, expireTimeout_ ] = methodCtxBody ctx
      appName :: String
      appName = fromMaybe "" $ fromVariant appName_
      replaceId :: Word32
      replaceId = fromMaybe 0 $ fromVariant replaceId_
      summary :: String
      summary = fromMaybe "" $ fromVariant summary_
      body :: String
      body = fromMaybe "" $ fromVariant body_
      expireTimeout :: Int32
      expireTimeout = boundTimeout $ fromMaybe (-1) $ fromVariant expireTimeout_

  -- If the replace_id was zero in the message, we have to generate
  -- one to send back.
  replaceId' <- case replaceId of
    0 -> liftIO $ getNextReplaceId state
    other -> return other

  let note = Notification { noteAppName = appName
                          , noteReplaceId = replaceId'
                          , noteSummary = summary
                          , noteBody = body
                          , noteExpireTimeout = expireTimeout
                          }

  -- Atomically enqueue a new notification
  addNote state note

  -- Wake the other thread up to make it start processing
  -- notifications.  If there is already a token in the box, the other
  -- thread will wake up eventually.  We don't need to put more since
  -- state is actually tracked in the queue
  liftIO $ tryPutMVar box True

  -- Give the sender the ID we assigned to this notification
  replyReturn ctx [ toVariant replaceId' ]
  where boundTimeout :: Int32 -> Int32
        boundTimeout t =
          if t <= 0 || t > maxTimeout
            then defTimeout
            else t

-- Stub to initialize the notification service.  Claim the appropriate
-- names on the session bus and export the notification objects.
notificationDaemon :: NotificationState -> MVar Bool -> Int32 -> Int32 -> DBus ()
notificationDaemon state box maxTimeout defTimeout = do
  _ <- requestName "org.freedesktop.Notifications" []
            (\_ -> liftIO $ putStrLn "There is a Notification daemon already running")
            (\_ -> return ())

  export "/org/freedesktop/Notifications" iface
  -- Just wait forever, processing events
  mainLoop
  where iface = daemonInterface state box maxTimeout defTimeout

-- Helper to get the next notification identifier atomically from the
-- MVar
getNextReplaceId :: NotificationState -> IO Word32
getNextReplaceId state = modifyMVar state $ \val -> do
  let thisId = notifyIdSource val
  return (val { notifyIdSource = thisId + 1 }, thisId)

-- Helper to atomically add a notification to the MVar
addNote :: forall (m :: * -> *). (MonadIO m) => NotificationState -> Notification -> m ()
addNote state note = liftIO $ modifyMVar_ state $ \val -> do
  let currentNotes = notifyQueue val
  return val { notifyQueue = enqueue note currentNotes }

-- Helper to remove the notification with the given ID from the queue.
-- Note: This is not an entirely compliant implementation because it
-- will only remove enqueued notifications - not active ones.  In
-- principle this is possible but not really worth the effort
-- (especially since this implementation does not allow notifications
-- to have unbounded lifetimes). Returns True if the notification was
-- still active.
removeNoteById :: forall (m :: * -> *). (MonadIO m) => NotificationState -> Word32 -> m Bool
removeNoteById state ident = liftIO $ modifyMVar state $ \val -> do
  let currentNotes = notifyQueue val
      remainingNotes = filterQueue ((==ident) . noteReplaceId) currentNotes
  return (val { notifyQueue = remainingNotes }, currentNotes == remainingNotes)

waitForNotifications :: forall a a1. ([Char] -> IO a) -> NotificationState -> MVar a1 -> Int -> Int -> IO ()
waitForNotifications callback state mbox maxMsgLen aniTimeStep = do
  -- This will block until this thread has something to do (the DBus
  -- daemon thread enqueued a notification)
  _ <- takeMVar mbox

  -- For each item in the queue:
  -- 1) Dequeue it
  -- 2) Use the callback function to display it
  -- 3) Wait for the prescribed timeout
  -- 4) Goto 1 unless the queue is empty
  showNotifications
  where showNotifications = do
          -- Atomically take an item off of the queue.  The dbus
          -- thread can't modify it from under us because it also uses
          -- modify (instead of put)
          itm <- modifyMVar state mdequeue
          case itm of
            -- Break the recursion and go back to waiting on the mailbox
            Nothing -> return ()
            -- Update the message and sleep for the required time,
            -- then recursively call this method to get the next note
            Just itm' -> postAndWait itm' >> showNotifications

        -- Helper to dequeue through the MVar
        mdequeue state' = do
          let q = notifyQueue state'
          case dequeue q of
            Nothing -> return (state', Nothing)
            Just (itm, q') -> return (state' { notifyQueue = q' }, Just itm)

        postAndWait itm = do
          if length msg < maxMsgLen
            then displayMessage msg timeout
            else animateMessage msgWindow timeout
          -- Clear the message after a delay
          callback ""
          where msg = buildMessage itm
                timeout = fromIntegral $ noteExpireTimeout itm
                msgWindow = makeMessageWindow msg maxMsgLen

        -- Display a message until the timeout
        displayMessage msg timeout = do
          callback $ formatMessage msg
          threadDelay $ 1000 * timeout

        -- Display the message scrolling back and forth if it is long.
        -- This uses the message window code.
        animateMessage msgWindow timeout
          | timeout <= 0 = return ()
          | otherwise = do
            callback $ formatMessage $ show msgWindow
            threadDelay $ 1000 * aniTimeStep
            animateMessage (advanceMessageWindow msgWindow) (timeout - aniTimeStep)

-- Idea: The DBus thread is the producer.  It will enqueue events as
-- they are received.  It only needs to signal an event if the queue
-- was empty when it added the event.

-- When woken up, the Update thread (the consumer) sets a new message.
-- If the message is expire, dequeue and clear the message.  If there
-- is another message still on the queue, use it to set the message
-- (but leave it there).
daemonMain :: forall a b. NotificationDaemon -> ([Char] -> IO a) -> IO b
daemonMain (NotificationDaemon msgLen timestep maxTimeout defTimeout) cb = do
  -- Initially, there is no message (clear it manually so that xmobar
  -- doesn't display "waiting...")
  cb ""

  -- Connect to the session bus
  client <- newClient =<< getSessionBus

  -- This MVar is used by the DBus thread to signal the display thread
  -- to wake up.
  mbox <- newEmptyMVar
  -- Shared state between both threads, stored in an MVar
  let defaultNotificationState = EventState {
        notifyQueue = emptyQueue
        , notifyIdSource = 1
        }
  state <- newMVar defaultNotificationState

  -- Spawn off a thread to handle DBus notifications
  forkIO $ runDBus client $ notificationDaemon state mbox maxTimeout defTimeout
  -- Have this thread just wait until the DBus thread wakes it up to
  -- do some display work
  forever $ waitForNotifications cb state mbox (fromIntegral msgLen) (fromIntegral timestep)



-- This is a small helper "library" to manage a fixed-length window
-- into a string.  Both the string and the window size are specified
-- at creation time.  There is one operation: advanceWindow.  This
-- function takes the window and moves it one character in the current
-- traversal direction.  It manages switching the traversal direction
-- transparently when it reaches either extent of the string.  Strings
-- smaller than the window size are always just returned.
data MessageScrollDirection = ScrollForward
                            | ScrollBackward
                              deriving (Show)

data MessageWindow = WindowedMessage { windowCurMsg :: String
                                     , windowPastMsgs :: [String]
                                     , windowSize :: Int
                                     , windowDirection :: MessageScrollDirection
                                     }
                   | TrivialMessage String

-- The show instance is trivial for strings smaller than the window
-- size.  For larger strings, show just the number of characters that
-- can fit in the window.
instance Show MessageWindow where
  show (TrivialMessage m) = m
  show (WindowedMessage { windowCurMsg = msg, windowSize = sz }) = take sz msg

makeMessageWindow :: String -> Int -> MessageWindow
makeMessageWindow str sz = if length str > sz then wm else tm
  where wm = WindowedMessage { windowCurMsg = str
                             , windowPastMsgs = []
                             , windowSize = sz
                             , windowDirection = ScrollForward }
        tm = TrivialMessage str

-- Advance the window along the string in the current traversal
-- direction.  Switch directions if the traversal reaches the "end" in
-- either direction.
advanceMessageWindow :: MessageWindow -> MessageWindow
advanceMessageWindow m@(TrivialMessage _) = m
-- Switch directions if we are going backwards and ran out of past messages.
advanceMessageWindow m@(WindowedMessage { windowCurMsg = curMsg
                                        , windowPastMsgs = []
                                        , windowSize = _
                                        , windowDirection = ScrollBackward }) =
  m { windowCurMsg = tail curMsg
    , windowPastMsgs = [ curMsg ]
    , windowDirection = ScrollForward }
-- Switch directions if we are going forwards and the current message
-- length is the window size (meaning we don't need to scroll anymore).
-- Otherwise, just keep going in the same direction
advanceMessageWindow m@(WindowedMessage { windowCurMsg = curMsg
                                        , windowPastMsgs = pastMsgs
                                        , windowSize = sz
                                        , windowDirection = dir }) =
  case dir of
    ScrollForward -> if curLen <= sz then switchWindow else forwardWindow
    ScrollBackward -> backwardWindow
  where curLen = length curMsg
        forwardWindow = m { windowCurMsg = tail curMsg
                          , windowPastMsgs = curMsg : pastMsgs
                          }
        backwardWindow = m { windowCurMsg = head pastMsgs
                           , windowPastMsgs = tail pastMsgs
                           }
        switchWindow = m { windowCurMsg = head pastMsgs
                         , windowPastMsgs = tail pastMsgs
                         , windowDirection = ScrollBackward
                         }

-- The trivial functional queue made up of two lists
data Queue a = Queue [a] [a]
               deriving (Show, Eq)

emptyQueue :: forall a. Queue a
emptyQueue = Queue [] []

enqueue :: forall a. a -> Queue a -> Queue a
enqueue itm (Queue enq deq) = Queue (itm : enq) deq

dequeue :: forall a. Queue a -> Maybe (a, Queue a)
dequeue (Queue enq deq) =
  case (enq, deq) of
    ([], []) -> Nothing
    (_, []) -> let (itm : enq') = reverse enq in Just (itm, Queue [] enq')
    (_, itm : []) -> Just (itm, Queue [] (reverse enq))
    (_, itm : deq') -> Just (itm, Queue enq deq')

filterQueue :: forall a. (a -> Bool) -> Queue a -> Queue a
filterQueue p (Queue enq deq) = Queue (filter p enq) (filter p deq)

isQueueEmpty :: forall t. Queue t -> Bool
isQueueEmpty (Queue [] []) = True
isQueueEmpty _ = False
