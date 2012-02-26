module Main where

import PowerMate
import System.IO
import System.Process

data State = State {
  stPowerMate  :: Handle,
  stVolume     :: Int
}

processEvent :: State -> Event -> IO State
processEvent state (Button True) = do
  createProcess (proc "music-toggle" [])
  return state
processEvent state (Button False) = return state

processEvent state (Rotate dir) = do
  if dir < 2 then createProcess (proc "volume-up" [])
  else createProcess (proc "volume-down" [])
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


loop :: FilePath -> IO ()
loop devname = do
  powermate <- openDevice devname

  state <- readState $ State { stPowerMate=powermate, stVolume=0 }

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
