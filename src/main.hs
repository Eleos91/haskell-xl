module Main where

import GHC.IO.Unsafe 
  ( unsafePerformIO )
import GHC.Ptr
  ( Ptr )
import System.IO 
  ( writeFile
  , appendFile
  , FilePath 
  )
import System.Posix.User 
  ( getLoginName )
import System.Unix.Shadow 
  ( SUserEntry (sUserPassword)
  , getSUserEntryForName
  )
import Data.List.Split 
  ( splitOn )
import Foreign.C.String 
  ( CString, newCString, peekCString ) 
import Graphics.X11.Xlib
  ( XEventPtr
  , GrabStatus
  , Display
  , nextEvent
  , get_EventType
  , keyPress
  , asKeyEvent
  , lookupString
  , ungrabPointer
  , ungrabKeyboard
  , openDisplay
  , defaultRootWindow
  , allocaXEvent
  , keyPressMask
  , selectInput
  , xK_Return
  , grabPointer
  , buttonPressMask
  , grabModeAsync
  , grabKeyboard
  )
import Graphics.X11.Xlib.Extras
  ( currentTime
  , none
  , XErrorHandler
  , getErrorEvent
  , setErrorHandler
  , ErrorEvent (ev_error_code)
  )
import System.Exit
  ( exitSuccess )
import Foreign.Marshal.Alloc
  ( free )
import Data.Maybe
  ( fromJust )
import Control.Monad
  ( when )


foreign import ccall safe "crypt.h"
  crypt :: CString -> CString -> IO CString


type Salt = String
type Hash = String
data Spwd = Spwd
  { salt :: Salt
  , pwd :: Hash
  }
instance Show Spwd where
  show spwd = salt spwd ++ "$" ++ pwd spwd


instance Show ErrorEvent where
  show = show .  ev_error_code 


logFile :: FilePath
logFile = "/tmp/hxl.log"


logging :: (Show a) => IO a -> IO a
logging expr = do
  result <- expr
  appendFile logFile $ show result ++ "\n"
  return result


getSpwdForSUserEntry :: SUserEntry -> Spwd
getSpwdForSUserEntry sUserEntry =
  Spwd s p
  where
    splitpwd = tail $ splitOn "$" $ sUserPassword sUserEntry
    s = "$" ++ head splitpwd ++ "$" ++ splitpwd !! 1
    p = splitpwd !! 2


notEmpty :: String -> Bool
notEmpty [] = False
notEmpty _ = True


errorHandler :: XErrorHandler
errorHandler display eventPtr = do
  logging $ getErrorEvent eventPtr
  return ()


eventLoop ::
  String ->
  XEventPtr ->
  Display ->
  Spwd ->
  IO ()
eventLoop input event display spwd = do
  nextEvent display event
  eventType <- get_EventType event
  if eventType == keyPress
  then do
    let key_event = asKeyEvent event
    (maybe_keysim, user_input) <- lookupString key_event
    let keysim = fromJust maybe_keysim
    if keysim == xK_Return
    then do
      pw <- newCString input
      cSalt <- newCString $ salt spwd
      user_input_hash <- crypt pw cSalt
      free pw
      free cSalt
      user_input_hash <- peekCString user_input_hash -- idk how to do this the right way...
      if
         notEmpty user_input &&
         (user_input_hash == show spwd)
      then do
        ungrabPointer display currentTime
        ungrabKeyboard display currentTime
        exitSuccess
      else eventLoop "" event display spwd
    else eventLoop (input ++ user_input) event display spwd
  else eventLoop input event display spwd


main :: IO ()
main = do
  current_login_name <- getLoginName
  current_usershadow <- getSUserEntryForName current_login_name
  let spwd = getSpwdForSUserEntry current_usershadow
  display <- logging $ openDisplay ""
  let root_window = defaultRootWindow display
  logging $ return $ "Root_Window: " ++ show root_window
  setErrorHandler errorHandler
  mouseStatus <- logging $ 
    grabPointer 
      display 
      root_window 
      True 
      buttonPressMask 
      grabModeAsync 
      grabModeAsync 
      none 
      none 
      currentTime
  when (mouseStatus == success) $ do
    keyboardStatus <- logging $ 
      grabKeyboard 
        display 
        root_window 
        False 
        grabModeAsync 
        grabModeAsync 
        currentTime
    if keyboardStatus == success
    then do
      selectInput display root_window keyPressMask
      allocaXEvent $ \event -> eventLoop "" event display spwd
    else
      ungrabPointer display currentTime
  where
    success = 0 :: GrabStatus
