module Sound.ALSA.Mixer
    ( Contents(..)
    , Volume(..)
    , Control()
    , contents
    , controls
    , name
    , index
    , getMixerByName
    , Channel(..)
    ) where

import Control.Monad ( liftM )
import Data.Maybe ( catMaybes )
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
    mapM (\(idN, se) -> do
              n <- getName idN
              i <- getIndex idN
              return $! Control idN se n i
         ) es

data Contents = Contents { playback :: Maybe [(Channel, Volume)]
                         , capture :: Maybe [(Channel, Volume)]
                         }
    deriving (Read, Show, Eq)

data Volume = Volume { integer :: Integer
                     , dB :: Integer
                     , integerRange :: (Integer, Integer)
                     , dbRange :: (Integer, Integer)
                     , switch :: Maybe Bool
                     }
    deriving (Read, Show, Eq)


contents :: Control -> IO Contents
contents c = do
    pChans <- liftM catMaybes $ mapM (justChannel hasPlaybackChannel) channels
    cChans <- liftM catMaybes $ mapM (justChannel hasCaptureChannel) channels
    pVols <- liftM (zip pChans) $ mapM getPVol pChans
    cVols <- liftM (zip cChans) $ mapM getCVol cChans
    return $ Contents { playback = if null pVols then Nothing else Just pVols
                      , capture = if null cVols then Nothing else Just cVols
                      }
  where justChannel f chan = do
            b <- f s chan
            if b then return (Just chan) else return Nothing
        channels = enumFrom FrontLeft
        s = selem c
        getPVol chan = do
            hasS <- hasPlaybackSwitch s
            sw <- if hasS then liftM Just (getPlaybackSwitch s chan)
                          else return Nothing
            i <- getPlaybackVolume s chan
            d <- getPlaybackDb s chan
            ir <- getPlaybackVolumeRange s
            dr <- getPlaybackDbRange s
            return $ Volume { integer = i
                            , dB = d
                            , switch = sw
                            , integerRange = ir
                            , dbRange = dr
                            }
        getCVol chan = do
            hasS <- hasCaptureSwitch s
            sw <- if hasS then liftM Just (getCaptureSwitch s chan)
                          else return Nothing
            i <- getCaptureVolume s chan
            d <- getCaptureDb s chan
            ir <- getCaptureVolumeRange s
            dr <- getCaptureDbRange s
            return $ Volume { integer = i
                            , dB = d
                            , switch = sw
                            , integerRange = ir
                            , dbRange = dr
                            }
