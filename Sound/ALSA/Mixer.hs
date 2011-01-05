module Sound.ALSA.Mixer
    ( Volume(..)
    , Control()
    , controls
    , channels
    , name
    , index
    , switch
    , volume
    , common
    , playback
    , capture
    , Switch()
    , PerChannel(..)
    , getChannel
    , setChannel
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
                       , switch :: Either Switch (Maybe Switch, Maybe Switch)
                       , volume :: Either Volume (Maybe Volume, Maybe Volume)
                       , channels :: ([Channel], [Channel])
                       }

type Switch = PerChannel Bool

data PerChannel e = Joined { getJoined :: IO e
                           , setJoined :: e -> IO ()
                           }
                  | PerChannel { getPerChannel :: IO [(Channel, e)]
                               , setPerChannel :: [(Channel, e)] -> IO ()
                               }

data Volume = Volume { getRange :: IO (Integer, Integer)
                     , setRange :: (Integer, Integer) -> IO ()
                     , value :: PerChannel Integer
                     }

getChannel :: Channel -> PerChannel x -> IO (Maybe x)
getChannel _ j@(Joined f _) = liftM Just f
getChannel c p@(PerChannel f _) = liftM (lookup c) f

setChannel :: Channel -> PerChannel x -> x -> IO ()
setChannel _ j@(Joined _ f) v = f v
setChannel c p@(PerChannel _ f) v = f [(c, v)]

playback :: Either a (Maybe a, Maybe a) -> Maybe a
playback (Left _) = Nothing
playback (Right (x, _)) = x

capture :: Either a (Maybe a, Maybe a) -> Maybe a
capture (Left _) = Nothing
capture (Right (_, x)) = x

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
                   }
        perChannel fGet fSet chans =
            PerChannel { getPerChannel = liftM (zip chans)
                            $ mapM (fGet se) chans
                       , setPerChannel = mapM_ (uncurry (fSet se))
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
                   }
        perChannel fGet fSet chans =
            PerChannel { getPerChannel = liftM (zip chans)
                            $ mapM (fGet se) chans
                       , setPerChannel = mapM_ (uncurry (fSet se))
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

controls :: Mixer -> IO [Control]
controls mix = do
    es <- elements mix
    forM es $ \(idN, se) -> do
        n <- getName idN
        i <- getIndex idN
        sw <- mkSwitch se
        v <- mkVolume se
        hasPlayChan <- mapM (hasPlaybackChannel se) allChannels
        hasCaptChan <- mapM (hasCaptureChannel se) allChannels
        let pChans = map fst $ filter snd $ zip allChannels hasPlayChan
            cChans = map fst $ filter snd $ zip allChannels hasCaptChan
        return $! Control { selem = se
                          , seId = idN
                          , name = n
                          , index = i
                          , switch = sw
                          , volume = v
                          , channels = (pChans, cChans)
                          }
