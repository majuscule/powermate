module Main where

import PowerMate
import System.IO
import System.Process
import Text.Regex.Posix

data State = State {
  stPowerMate   :: Handle,
  stVolume      :: Int,
  stPrevDir     :: Int,
  stPrevAction  :: Int
}

processEvent :: State -> Event -> IO State
processEvent state (Button True) = do
  createProcess (proc "music-toggle" [])
  return state
processEvent state (Button False) = do
  return state

processEvent state (Rotate dir) = do
  state <- (if dir < 2
              && (stPrevDir state) == 1
              && (stPrevAction state) == 1
                then volumeUp
              else return) state
  state <- (if dir > 2
              && (stPrevDir state) == 0
              && (stPrevAction state) == 0
                then volumeDown
              else return) state
  state <- updatePrevState state (if dir < 2 then 1 else 0)
  updateBrightness state
  state <- updatePrevAction state (if (stPrevAction state) == 1 then 0 else 1)
  return state

processEvent state (StatusChange status) = do
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

volumeUp :: State -> IO State
volumeUp state = do
  createProcess (proc "volume-up" [])
  state <- readState $ State {
    stPowerMate=(stPowerMate state),
    stVolume=(max 0 $ 1+(stVolume state)),
    stPrevAction=(stPrevAction state),
    stPrevDir=(stPrevDir state) }
  state <- updatePrevAction state 1
  return state

volumeDown :: State -> IO State
volumeDown state = do
  createProcess (proc "volume-down" [])
  state <- readState $ State {
    stPowerMate=(stPowerMate state),
    stVolume=(max 0 $ (stVolume state)-1),
    stPrevAction=(stPrevAction state),
    stPrevDir=(stPrevDir state) }
  state <- updatePrevAction state 0
  return state

updatePrevState :: State -> Int -> IO State
updatePrevState state dir = do
  state <- readState $ State {
    stPowerMate=(stPowerMate state),
    stVolume=(stVolume state),
    stPrevAction=(stPrevAction state),
    stPrevDir=dir }
  return state

updatePrevAction :: State -> Int -> IO State
updatePrevAction state action = do
  state <- readState $ State {
    stPowerMate=(stPowerMate state),
    stVolume=(stVolume state),
    stPrevAction=action,
    stPrevDir=(stPrevDir state) }
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
  state <- readState $ State {
    stPowerMate=powermate,
    stVolume=volume,
    stPrevAction=0,
    stPrevDir=0 }
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
