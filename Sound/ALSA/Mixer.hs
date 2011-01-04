module Sound.ALSA.Mixer
    ( Volume(..)
    , Control()
    , contents
    , controls
    , channels
    , name
    , index
    , getMixerByName
    , Channel(..)
    ) where

import Control.Monad ( forM, liftM )
import Data.Maybe ( catMaybes )
import Debug.Trace
import Foreign.C.Error ( Errno(..) )
import Sound.ALSA.Exception ( catchErrno )
import Sound.ALSA.Mixer.Internal

data Control = Control { selem :: SimpleElement
                       , seId :: SimpleElementId
                       , index :: Integer
                       , name :: String
                       }

controls :: Mixer -> IO [Control]
controls mix = do
    es <- elements mix
    forM es $ \(idN, se) -> do
        n <- getName idN
        i <- getIndex idN
        return $! Control { selem = se
                          , seId = idN
                          , name = n
                          , index = i
                          }

data Mode = Playback | Capture
    deriving (Read, Show, Eq)

data Element e = Element { get :: IO e
                         , set :: e -> IO ()
                         }

-- | This function does not check that the channel exists before constructing
-- an 'Element'.  The caller must take care that 'mkSwitch' is not called
-- for a non-existent channel.
mkSwitch :: Control -> (Mode, Channel) -> IO (Maybe (Element Bool))
mkSwitch cn (m, ch) = do
    hasSw <- hasSwitch s
    case hasSw of
        False -> return Nothing
        True -> return $ Just Element { get = getSwitch s ch, set = setSwitch s ch }
  where s = selem cn
        hasSwitch :: SimpleElement -> IO Bool
        hasSwitch = case m of
                        Playback -> hasPlaybackSwitch
                        Capture -> hasCaptureSwitch
        getSwitch :: SimpleElement -> Channel -> IO Bool
        getSwitch = case m of
                        Playback -> getPlaybackSwitch
                        Capture -> getCaptureSwitch
        setSwitch :: SimpleElement -> Channel -> Bool -> IO ()
        setSwitch = case m of
                        Playback -> setPlaybackSwitch
                        Capture -> setCaptureSwitch

mkVolume :: Control -> (Mode, Channel) -> Bool -> IO (Maybe (Element Volume))
mkVolume cn (m, ch) db = do
    hasV <- hasVolume s
    case hasV of
        False -> return Nothing
        True -> case db of
            False -> return ret 
            True -> do
                dBv <- catchInvalidAsNothing $ getVolume s ch
                case dBv of
                    Nothing -> return Nothing
                    Just _ -> return ret
  where s = selem cn
        ret = Just Element { get = getVolume s ch
                           , set = setVolume s ch
                           }
        hasVolume = case m of
                        Playback -> hasPlaybackVolume
                        Capture -> hasCaptureVolume
        getVolume = case m of
                        Playback -> getPlayback db
                        Capture -> getCapture db
        setVolume = case m of
                        Playback -> setPlayback db
                        Capture -> setCapture db
      
data Volume = Volume { value :: Integer
                     , range :: (Integer, Integer)
                     }
    deriving (Read, Show, Eq)

getPlayback :: Bool -> SimpleElement -> Channel -> IO Volume
getPlayback db s ch = do
    v <- getVolume s ch
    (lo, hi) <- getRange s
    return Volume { value = v, range = (lo, hi) }
  where getVolume = case db of
                        False -> getPlaybackVolume
                        True -> getPlaybackDb
        getRange = case db of
                       False -> getPlaybackVolumeRange
                       True -> getPlaybackDbRange


getCapture :: Bool -> SimpleElement -> Channel -> IO Volume
getCapture db s ch = do
    v <- getVolume s ch
    (lo, hi) <- getRange s
    return Volume { value = v, range = (lo, hi) }
  where getVolume = case db of
                        False -> getCaptureVolume
                        True -> getCaptureDb
        getRange = case db of
                       False -> getCaptureVolumeRange
                       True -> getCaptureDbRange

setPlayback :: Bool -> SimpleElement -> Channel -> Volume -> IO ()
setPlayback db s ch v = do
    setVolume s ch $ value v
    --setRange s ch $ range v
  where setVolume = case db of
                        False -> setPlaybackVolume
                        True -> setPlaybackDb
        --setRange = case db of
                       --False -> setPlaybackVolumeRange
                       --True -> setPlaybackDbRange

setCapture :: Bool -> SimpleElement -> Channel -> Volume -> IO ()
setCapture db s ch v = do
    setVolume s ch $ value v
    --setRange s ch $ range v
  where setVolume = case db of
                        False -> setCaptureVolume
                        True -> setCaptureDb
        --setRange = case db of
                       --False -> setCaptureVolumeRange
                       --True -> setCaptureDbRange

data Content = Content { mode :: Mode
                       , channel :: Channel
                       , switch :: Maybe (Element Bool)
                       , volume :: Maybe (Element Volume)
                       , dB :: Maybe (Element Volume)
                       }

channels :: Control -> IO [(Mode, Channel)]
channels con = do
    hasPChan <- mapM (hasPlaybackChannel s) allChannels
    hasCChan <- mapM (hasCaptureChannel s) allChannels
    let pChans = map fst $ filter snd $ zip allChannels hasPChan
        cChans = map fst $ filter snd $ zip allChannels hasCChan
        chans = (map (\x -> (Playback, x)) pChans) ++
                (map (\x -> (Capture, x)) cChans)
    return chans
  where s = selem con
catchInvalidAsNothing a = catchErrno (Errno 22) (liftM Just a) (return Nothing)

contents :: Control -> IO [Content]
contents c = do
    chans <- channels c
    mapM worker chans
  where worker (m, ch) = do
            sw <- mkSwitch c (m, ch)
            v <- mkVolume c (m, ch) False
            d <- mkVolume c (m, ch) True
            return Content { mode = m
                           , channel = ch
                           , switch = sw
                           , volume = v
                           , dB = d
                           }
