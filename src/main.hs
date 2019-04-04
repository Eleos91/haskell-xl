module Main where

import System.Posix.User
import System.Unix.Shadow
import Data.List.Split
import Foreign.C.String
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Exit
import Foreign.Marshal.Alloc
import Data.Maybe

foreign import ccall safe "crypt.h"
  crypt :: CString -> CString -> IO CString

type Salt = String
type Hash = String
data Spwd = Spwd {
  salt :: Salt,
  pwd :: Hash}

instance Show Spwd where
  show spwd = (salt spwd) ++ "$" ++ (pwd spwd)

getSpwdForSUserEntry :: SUserEntry -> Spwd 
getSpwdForSUserEntry sUserEntry = 
  Spwd s p
  where
    splitpwd = tail $ splitOn "$" $ sUserPassword sUserEntry
    s = "$" ++ (splitpwd !! 0) ++ "$" ++ (splitpwd !! 1)
    p = splitpwd !! 2

notEmpty :: String -> Bool
notEmpty [] = False
notEmpty _ = True

event_loop :: 
  String -> 
  XEventPtr -> 
  Display -> 
  Spwd -> 
  IO ()
event_loop input event display spwd = do
  nextEvent display event
  event_type <- get_EventType event
  if (event_type == keyPress) 
  then do 
    let key_event = asKeyEvent event
    (maybe_keysim, user_input) <- lookupString key_event 
    let keysim = fromJust maybe_keysim
    if (keysim == xK_Return)
    then do 
      pw <- newCString input
      cSalt <- newCString $ salt spwd
      user_input_hash <- crypt pw cSalt
      free pw
      free cSalt
      user_input_hash <- peekCString user_input_hash -- idk how to do this the right way...
      if (
         (notEmpty user_input) && 
         (user_input_hash == (show spwd)))
      then do 
        ungrabPointer display currentTime
        ungrabKeyboard display currentTime
        exitSuccess
      else do
        event_loop "" event display spwd
    else do
      event_loop (input ++ user_input) event display spwd
  else do
    event_loop input event display spwd

main :: IO ()
main = do
  current_login_name <- getLoginName
  current_usershadow <- getSUserEntryForName current_login_name
  let spwd = getSpwdForSUserEntry current_usershadow
  display <- openDisplay ""
  let root_window = defaultRootWindow display
  _ <- grabPointer display root_window True buttonPressMask grabModeAsync grabModeAsync none none currentTime
  _ <- grabKeyboard display root_window False grabModeAsync grabModeAsync currentTime
  selectInput display root_window keyPressMask
  allocaXEvent $ \event -> do event_loop "" event display spwd

