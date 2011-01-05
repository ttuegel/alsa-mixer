module Sound.ALSA.Mixer
    ( -- * Types
      Control()
    , Mixer()
    , Channel(..)
    , PerChannel(..)
    , Volume(..)
    , Switch()
      -- * Functions
      -- ** Mixers
    , controls
    , getMixerByName
      -- ** Controls
    , getControlByName
    , name
    , index
    , switch
    , volume
    , common
    , playback
    , capture
      -- ** PerChannels
    , channels
    , joined
    , perChannel
    , getChannel
    , setChannel
    ) where

import Control.Monad ( forM, liftM, when )
import Data.Maybe ( catMaybes )
import Foreign.C.Error ( Errno(..) )
import Sound.ALSA.Exception ( catchErrno )
import Sound.ALSA.Mixer.Internal

-- | 'Control' represents one of the controls belonging to an ALSA mixer
-- element. Each control has a number of playback and capture channels.
-- The control may also have a switch and/or a volume capability associated
-- with it. The capability can be common to both playback and capture, or
-- there can be separate capabilities for each.
data Control = Control { index :: Integer
                       , name :: String
                       , switch :: Either Switch (Maybe Switch, Maybe Switch)
                       , volume :: Either Volume (Maybe Volume, Maybe Volume)
                       }

-- | 'PerChannel' represents a capability that with either a separate value for
-- each channel or with a common value for all channels.
data PerChannel e = Joined { getJoined :: IO e
                           , setJoined :: e -> IO ()
                           , joinedChannels :: [Channel]
                           }
                  | PerChannel { getPerChannel :: IO [(Channel, e)]
                               , setPerChannel :: [(Channel, e)] -> IO ()
                               , perChannels :: [Channel]
                               }

-- | True if the 'PerChannel' object has a common value for all channels.
joined :: PerChannel e -> Bool
joined j@(Joined _ _ _) = True
joined _ = False

-- | True if the 'PerChannel' object has a separate value for each channel.
perChannel :: PerChannel e -> Bool
perChannel p@(PerChannel _ _ _) = True
perChannel _ = False

-- | All channels supported by a 'PerChannel' object.
channels :: PerChannel e -> [Channel]
channels p | joined p = joinedChannels p
           | otherwise = perChannels p

-- | 'Switch' represents a switch capability for controls and channels that can
-- be muted and unmuted.
type Switch = PerChannel Bool

-- | 'Volume' represents a volume capability. There may be a separate value per
-- channel, but each capability has only one range.
data Volume = Volume { getRange :: IO (Integer, Integer)
                     , setRange :: (Integer, Integer) -> IO ()
                     , value :: PerChannel Integer
                     }

-- | Get the value associated with a particular channel, if that channel exists.
getChannel :: Channel -> PerChannel x -> IO (Maybe x)
getChannel c p | joined p = case c `elem` channels p of
                                True -> liftM Just $ getJoined p
                                False -> return Nothing
               | otherwise = liftM (lookup c) $ getPerChannel p

-- | Set the value associated with a particular channel, if that channel exists.
setChannel :: Channel -> PerChannel x -> x -> IO ()
setChannel c p v | joined p = when (c `elem` channels p) $ setJoined p v
                 | otherwise = setPerChannel p [(c, v)]

-- | For a given capability, which may be for either playback or capture, or
-- common to both, return the playback capability if it exists.
playback :: Either a (Maybe a, Maybe a) -> Maybe a
playback (Left _) = Nothing
playback (Right (x, _)) = x

-- | For a given capability, which may be for either playback or capture, or
-- common to both, return the capture capability if it exists.
capture :: Either a (Maybe a, Maybe a) -> Maybe a
capture (Left _) = Nothing
capture (Right (_, x)) = x

-- | For a given capability, which may be for either playback or capture, or
-- common to both, return the common capability if it exists.
common :: Either a (Maybe a, Maybe a) -> Maybe a
common (Left x) = Just x
common (Right _) = Nothing

mkSwitch :: SimpleElement -> IO (Either Switch (Maybe Switch, Maybe Switch))
mkSwitch se = do
    hasPlayChan <- mapM (hasPlaybackChannel se) allChannels
    hasCaptChan <- mapM (hasCaptureChannel se) allChannels
    let pChans = map fst $ filter snd $ zip allChannels hasPlayChan
        cChans = map fst $ filter snd $ zip allChannels hasCaptChan
    hasComSw <- hasCommonSwitch se
    hasPlaySw <- hasPlaybackSwitch se
    hasPlaySwJ <- hasPlaybackSwitchJoined se
    hasCaptSw <- hasCaptureSwitch se
    hasCaptSwJ <- hasCaptureSwitchJoined se
    return $ case hasComSw of
        True -> Left $ case hasPlaySwJ of
            True -> comJoinedSwitch pChans
            False -> comPerChannelSwitch pChans
        False ->
            let playSw = case hasPlaySw of
                    False -> Nothing
                    True -> Just $ case hasPlaySwJ of
                        True -> playJoinedSwitch pChans
                        False -> playPerChannelSwitch pChans
                captSw = case hasCaptSw of
                    False -> Nothing
                    True -> Just $ case hasCaptSwJ of
                        True -> captJoinedSwitch cChans
                        False -> captPerChannelSwitch cChans
            in Right (playSw, captSw)
  where joined fGet fSet chans =
            Joined { getJoined = fGet se (head chans)
                   , setJoined = fSet se (head chans)
                   , joinedChannels = chans
                   }
        perChannel fGet fSet chans =
            PerChannel { getPerChannel = liftM (zip chans)
                            $ mapM (fGet se) chans
                       , setPerChannel = mapM_ (uncurry (fSet se))
                       , perChannels = chans
                       }
        comJoinedSwitch = joined getPlaybackSwitch setPlaybackSwitch
        comPerChannelSwitch = perChannel getPlaybackSwitch setPlaybackSwitch
        playJoinedSwitch = comJoinedSwitch
        playPerChannelSwitch = comPerChannelSwitch
        captJoinedSwitch = joined getCaptureSwitch setCaptureSwitch
        captPerChannelSwitch = perChannel getCaptureSwitch setCaptureSwitch

mkVolume :: SimpleElement -> IO (Either Volume (Maybe Volume, Maybe Volume))
mkVolume se = do
    hasPlayChan <- mapM (hasPlaybackChannel se) allChannels
    hasCaptChan <- mapM (hasCaptureChannel se) allChannels
    let pChans = map fst $ filter snd $ zip allChannels hasPlayChan
        cChans = map fst $ filter snd $ zip allChannels hasCaptChan
    hasComV <- hasCommonVolume se
    hasPlayV <- hasPlaybackVolume se
    hasPlayVJ <- hasPlaybackVolumeJoined se
    hasCaptV <- hasCaptureVolume se
    hasCaptVJ <- hasCaptureVolumeJoined se
    return $ case hasComV of
        True -> Left $ playVolume { value = case hasPlayVJ of
            True -> comJoinedVolume pChans
            False -> comPerChannelVolume pChans }
        False ->
            let playVol = case hasPlayV of
                    False -> Nothing
                    True -> Just $ playVolume { value = case hasPlayVJ of
                        True -> playJoinedVolume pChans
                        False -> playPerChannelVolume pChans }
                captVol = case hasCaptV of
                    False -> Nothing
                    True -> Just $ captVolume { value = case hasCaptVJ of
                        True -> captJoinedVolume cChans
                        False -> captPerChannelVolume cChans }
            in Right (playVol, captVol)
  where joined fGet fSet chans =
            Joined { getJoined = fGet se (head chans)
                   , setJoined = fSet se (head chans)
                   , joinedChannels = chans
                   }
        perChannel fGet fSet chans =
            PerChannel { getPerChannel = liftM (zip chans)
                            $ mapM (fGet se) chans
                       , setPerChannel = mapM_ (uncurry (fSet se))
                       , perChannels = chans
                       }
        playVolume = Volume { getRange = getPlaybackVolumeRange se
                            , setRange = setPlaybackVolumeRange se
                            , value = undefined
                            }
        captVolume = Volume { getRange = getCaptureVolumeRange se
                            , setRange = setCaptureVolumeRange se
                            , value = undefined
                            }
        comJoinedVolume = joined getPlaybackVolume setPlaybackVolume
        comPerChannelVolume = perChannel getPlaybackVolume setPlaybackVolume
        playJoinedVolume = comJoinedVolume
        playPerChannelVolume = comPerChannelVolume
        captJoinedVolume = joined getCaptureVolume setCaptureVolume
        captPerChannelVolume = perChannel getCaptureVolume setCaptureVolume

-- | All the 'Control' objects associated with a particular 'Mixer'.
controls :: Mixer -> IO [Control]
controls mix = do
    es <- elements mix
    forM es $ \(idN, se) -> do
        n <- getName idN
        i <- getIndex idN
        sw <- mkSwitch se
        v <- mkVolume se
        return $! Control { name = n
                          , index = i
                          , switch = sw
                          , volume = v
                          }

-- | Get the named 'Control', if it exists, from the named 'Mixer'.
getControlByName :: String  -- ^ Mixer name
                 -> String  -- ^ Control name
                 -> IO (Maybe Control)
getControlByName mixerName controlName = do
    mix <- getMixerByName mixerName
    cs <- controls mix
    return $ lookup controlName $ zip (map name cs) cs
