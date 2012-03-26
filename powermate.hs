module Main where

import PowerMate
import System.IO
import System.Process
import Text.Regex.Posix
import Data.Time

data State = State {
  stPowerMate     :: Handle,
  stVolume        :: Int,
  stPrevDir       :: Int,
  stPrevAction    :: Int,
  stPressed       :: Bool,
  stIndeterminate :: Bool,
  stLastPress     :: UTCTime
}

processEvent :: State -> Event -> IO State
processEvent state (Button True) = do
  time <- getCurrentTime
  state <- updateLastPress state (time)
  state <- updateButton state True
  state <- updateIndeterminate state True
  return state
processEvent state (Button False) = do
  time <- getCurrentTime
  if (stIndeterminate state)
    then do
    if (diffUTCTime (time) (stLastPress state) > 0.8)
      then do createProcess(proc "volume-toggle" []); return ()
      else do runCommand "music-toggle"; return ()
  else return ()
  state <- updateButton state False
  return state

processEvent state (Rotate dir) = do
  state <- updateIndeterminate state False
  state <- (if (stPressed state) == False
              && dir < 2
              && (stPrevDir state) == 1
              && (stPrevAction state) == 1
                then volumeUp
              else return) state
  state <- (if (stPressed state) == False
              && dir > 2
              && (stPrevDir state) == 0
              && (stPrevAction state) == 0
                then volumeDown
              else return) state
  if stPressed state
    then do
    if dir < 2 then do runCommand "next"; return ()
    else do runCommand "back"; return ()
  else return ()
  state <- updatePrevState state (if dir < 2 then 1 else 0)
  updateBrightness state
  state <- updatePrevAction state (if (stPrevAction state) == 1 then 0 else 1)
  return state

readState :: State -> IO State
readState state = do
  return state

next :: a -> (a -> IO a) -> IO ()
next state func = do
  newstate <- func state
  next newstate func
  return ()

updateBrightness :: State -> IO ()
updateBrightness state = do
  let brightness = (stVolume state)
  writeStatus (stPowerMate state) $
    statusInit { brightness=brightness }

updateIndeterminate :: State -> Bool -> IO State
updateIndeterminate state value = do
  state <- readState $ State {
    stPowerMate=(stPowerMate state),
    stVolume=(stVolume state),
    stPrevAction=(stPrevAction state),
    stPrevDir=(stPrevDir state),
    stPressed=(stPressed state),
    stLastPress=(stLastPress state),
    stIndeterminate=value }
  return state

volumeUp :: State -> IO State
volumeUp state = do
  createProcess (proc "volume-up" [])
  state <- readState $ State {
    stPowerMate=(stPowerMate state),
    stVolume=(max 0 $ 1+(stVolume state)),
    stPrevAction=(stPrevAction state),
    stPrevDir=(stPrevDir state),
    stPressed=(stPressed state),
    stLastPress=(stLastPress state),
    stIndeterminate=(stIndeterminate state) }
  state <- updatePrevAction state 1
  return state

volumeDown :: State -> IO State
volumeDown state = do
  createProcess (proc "volume-down" [])
  state <- readState $ State {
    stPowerMate=(stPowerMate state),
    stVolume=(max 0 $ (stVolume state)-1),
    stPrevAction=(stPrevAction state),
    stPrevDir=(stPrevDir state),
    stPressed=(stPressed state),
    stLastPress=(stLastPress state),
    stIndeterminate=(stIndeterminate state) }
  state <- updatePrevAction state 0
  return state

updatePrevState :: State -> Int -> IO State
updatePrevState state dir = do
  state <- readState $ State {
    stPowerMate=(stPowerMate state),
    stVolume=(stVolume state),
    stPrevAction=(stPrevAction state),
    stPrevDir=dir,
    stPressed=(stPressed state),
    stLastPress=(stLastPress state),
    stIndeterminate=(stIndeterminate state) }
  return state

updatePrevAction :: State -> Int -> IO State
updatePrevAction state action = do
  state <- readState $ State {
    stPowerMate=(stPowerMate state),
    stVolume=(stVolume state),
    stPrevAction=action,
    stPrevDir=(stPrevDir state),
    stPressed=(stPressed state),
    stLastPress=(stLastPress state),
    stIndeterminate=(stIndeterminate state) }
  return state

updateButton :: State -> Bool -> IO State
updateButton state button = do
  state <- readState $ State {
    stPowerMate=(stPowerMate state),
    stVolume=(stVolume state),
    stPrevAction=(stPrevAction state),
    stPrevDir=(stPrevDir state),
    stPressed=button,
    stLastPress=(stLastPress state),
    stIndeterminate=(stIndeterminate state) }
  return state

updateLastPress :: State -> UTCTime -> IO State
updateLastPress state lastPress = do
  state <- readState $ State {
    stPowerMate=(stPowerMate state),
    stVolume=(stVolume state),
    stPrevAction=(stPrevAction state),
    stPrevDir=(stPrevDir state),
    stPressed=(stPressed state),
    stLastPress=lastPress,
    stIndeterminate=(stIndeterminate state) }
  return state

loop :: FilePath -> IO ()
loop devname = do
  powermate <- openDevice devname

  alsaMixers <- readProcess "amixer" ["get", "Master"] []
  let alsaMaster = (alsaMixers =~ "\\[([0-9]{1,2})%\\]" :: String)
  let volume = read (drop 1
                      (take
                        (subtract 2
                          (length alsaMaster)) alsaMaster)) :: Int
  time <- getCurrentTime
  state <- readState $ State {
    stPowerMate=powermate,
    stVolume=volume,
    stPrevAction=0,
    stPrevDir=0,
    stPressed=False,
    stLastPress=time,
    stIndeterminate=False }
  updateBrightness state

  next state $ \call -> do
    event <- readEventWithSkip powermate Nothing
    case event of
      Nothing -> return call
      Just event -> processEvent call event

main :: IO ()
main = do
  powermate <- searchForDevice
  case powermate of
    Nothing  -> return ()
    Just work -> do
      loop work
