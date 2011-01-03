module Sound.ALSA.Mixer
    ( Contents(..)
    , Volume(..)
    , Control()
    , contents
    , controls
    , channels
    , getVolume
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

data Control = Control SimpleElementId SimpleElement String Integer

name :: Control -> String
name (Control _ _ n _) = n

index :: Control -> Integer
index (Control _ _ _ i) = i

selem :: Control -> SimpleElement
selem (Control _ s _ _) = s

controls :: Mixer -> IO [Control]
controls mix = do
    es <- elements mix
    forM es $ \(idN, se) -> do
        n <- getName idN
        i <- getIndex idN
        return $! Control idN se n i

data ChannelMode = Playback | Capture
    deriving (Read, Show, Eq)

type Contents = [(ChannelMode, Channel, Maybe Bool, Maybe Volume)]

data Volume = Volume { integer :: Integer
                     , dB :: Maybe Integer
                     , integerRange :: (Integer, Integer)
                     , dbRange :: Maybe (Integer, Integer)
                     }
    deriving (Read, Show, Eq)

channels :: Control -> IO [(ChannelMode, Channel)]
channels con = do
    hasPChan <- mapM (hasPlaybackChannel s) allChannels
    hasCChan <- mapM (hasCaptureChannel s) allChannels
    let pChans = map fst $ filter snd $ zip allChannels hasPChan
        cChans = map fst $ filter snd $ zip allChannels hasCChan
        chans = (map (\x -> (Playback, x)) pChans) ++
                (map (\x -> (Capture, x)) cChans)
    return chans
  where s = selem con

getVolume :: Control -> ChannelMode -> Channel
          -> IO (Maybe (Maybe Bool, Maybe Volume))
getVolume con Playback chan = do
    exist <- hasPlaybackChannel (selem con) chan
    if exist then getVolume' con Playback chan else return Nothing
getVolume con Capture chan = do
    exist <- hasCaptureChannel (selem con) chan
    if exist then getVolume' con Capture chan else return Nothing

getVolume' :: Control -> ChannelMode -> Channel
           -> IO (Maybe (Maybe Bool, Maybe Volume))
getVolume' con Playback chan = do
    let s = selem con
    hasSw <- hasPlaybackSwitch s
    sw <- if hasSw then liftM Just (getPlaybackSwitch s chan)
                   else return Nothing
    hasV <- hasPlaybackVolume s
    v <- case hasV of
        True -> do
            i <- getPlaybackVolume s chan
            d <- catchInvalidAsNothing $ getPlaybackDb s chan
            ir <- getPlaybackVolumeRange s
            dr <- catchInvalidAsNothing $ getPlaybackDbRange s
            return $ Just $ Volume { integer = i
                                   , dB = d
                                   , integerRange = ir
                                   , dbRange = dr
                                   }
        False -> return Nothing
    return $! Just (sw, v)
getVolume' con Capture chan = do
    let s = selem con
    hasSw <- hasCaptureSwitch s
    sw <- if hasSw then liftM Just (getCaptureSwitch s chan)
                   else return Nothing
    hasV <- hasCaptureVolume s
    v <- case hasV of
        True -> do
            i <- getCaptureVolume s chan
            d <- catchInvalidAsNothing $ getCaptureDb s chan
            ir <- getCaptureVolumeRange s
            dr <- catchInvalidAsNothing $ getCaptureDbRange s
            return $ Just $ Volume { integer = i
                                   , dB = d
                                   , integerRange = ir
                                   , dbRange = dr
                                   }
        False -> return Nothing
    return $! Just (sw, v)

catchInvalidAsNothing a = catchErrno (Errno 22) (liftM Just a) (return Nothing)

contents :: Control -> IO Contents
contents c = do
    chans <- channels c
    liftM catMaybes $ mapM worker chans
  where worker (t, chan) = do
            maybeV <- getVolume c t chan
            case maybeV of
                Nothing -> return Nothing
                Just (sw, v) -> return $ Just (t, chan, sw, v)

