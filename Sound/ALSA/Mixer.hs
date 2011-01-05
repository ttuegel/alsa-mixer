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

getChannel :: PerChannel x -> Channel -> IO (Maybe x)
getChannel j@(Joined f _) _ = liftM Just f
getChannel p@(PerChannel f _) c = liftM (lookup c) f

setChannel :: PerChannel x -> Channel -> x -> IO ()
setChannel j@(Joined _ f) _ v = f v
setChannel p@(PerChannel _ f) c v = f [(c, v)]

playback :: Either a (Maybe a, Maybe a) -> Maybe a
playback (Left _) = Nothing
playback (Right (x, _)) = x

capture :: Either a (Maybe a, Maybe a) -> Maybe a
capture (Left _) = Nothing
capture (Right (_, x)) = x

common :: Either a (Maybe a, Maybe a) -> Maybe a
common (Left x) = Just x
common (Right _) = Nothing

controls :: Mixer -> IO [Control]
controls mix = do
    es <- elements mix
    forM es $ \(idN, se) -> do
        n <- getName idN
        i <- getIndex idN
        hasPChan <- mapM (hasPlaybackChannel se) allChannels
        hasCChan <- mapM (hasCaptureChannel se) allChannels
        let pChans = map fst $ filter snd $ zip allChannels hasPChan
            cChans = map fst $ filter snd $ zip allChannels hasCChan
        hasCSw <- hasCommonSwitch se
        sw <- case hasCSw of
            True -> do
                hasJSw <- hasPlaybackSwitchJoined se
                return $ Left $ case hasJSw of
                    True -> commonJoinedSwitch se pChans
                    False -> commonPerChannelSwitch se pChans
            False -> do
                hasPSw <- hasPlaybackSwitch se
                pSwitch <- case hasPSw of
                    False -> return Nothing
                    True -> do
                        hasPJSw <- hasPlaybackSwitchJoined se
                        return $ Just $ case hasPJSw of
                            True -> playbackJoinedSwitch se pChans
                            False -> playbackPerChannelSwitch se pChans
                hasCSw <- hasCaptureSwitch se
                cSwitch <- case hasCSw of
                    False -> return Nothing
                    True -> do
                        hasCJSw <- hasCaptureSwitchJoined se
                        return $ Just $ case hasCJSw of
                            True -> captureJoinedSwitch se cChans
                            False -> capturePerChannelSwitch se cChans
                return $ Right (pSwitch, cSwitch)
        hasCV <- hasCommonVolume se
        v <- case hasCV of
            True -> do
                hasJV <- hasPlaybackVolumeJoined se
                let v = case hasJV of
                            True -> commonJoinedVolume se pChans
                            False -> commonPerChannelVolume se pChans
                return $ Left $ Volume { getRange = getPlaybackVolumeRange se
                                       , setRange = setPlaybackVolumeRange se
                                       , value = v
                                       }
            False -> do
                hasPV <- hasPlaybackVolume se
                pVolume <- case hasPV of
                    False -> return Nothing
                    True -> do
                        hasPJV <- hasPlaybackVolumeJoined se
                        return $ Just $
                            let v = case hasPJV of
                                        True -> playbackJoinedVolume se pChans
                                        False -> playbackPerChannelVolume se pChans
                            in Volume { setRange = setPlaybackVolumeRange se
                                      , getRange = getPlaybackVolumeRange se
                                      , value = v
                                      }
                hasCV <- hasCaptureVolume se
                cVolume <- case hasCV of
                    False -> return Nothing
                    True -> do
                        hasCJV <- hasCaptureVolumeJoined se
                        return $ Just $
                            let v = case hasCJV of
                                        True -> captureJoinedVolume se cChans
                                        False -> capturePerChannelVolume se cChans
                            in Volume { setRange = setCaptureVolumeRange se
                                      , getRange = getCaptureVolumeRange se
                                      , value = v
                                      }
                return $ Right (pVolume, cVolume)
        return $! Control { selem = se
                          , seId = idN
                          , name = n
                          , index = i
                          , switch = sw
                          , volume = v
                          , channels = (pChans, cChans)
                          }
  where joined fGet fSet se chans =
            Joined { getJoined = fGet se (head chans)
                   , setJoined = fSet se (head chans)
                   }
        perChannel fGet fSet se chans =
            PerChannel { getPerChannel = liftM (zip chans)
                            $ mapM (fGet se) chans
                       , setPerChannel = mapM_ (uncurry (fSet se))
                       }
        commonJoinedSwitch = joined getPlaybackSwitch setPlaybackSwitch
        commonPerChannelSwitch =
            perChannel getPlaybackSwitch setPlaybackSwitch
        commonJoinedVolume = joined getPlaybackVolume setPlaybackVolume
        commonPerChannelVolume =
            perChannel getPlaybackVolume setPlaybackVolume
        playbackJoinedSwitch = commonJoinedSwitch
        playbackPerChannelSwitch = commonPerChannelSwitch
        playbackJoinedVolume = commonJoinedVolume
        playbackPerChannelVolume = commonPerChannelVolume
        captureJoinedSwitch = joined getCaptureSwitch setCaptureSwitch
        capturePerChannelSwitch =
            perChannel getCaptureSwitch setCaptureSwitch
        captureJoinedVolume = joined getCaptureVolume setCaptureVolume
        capturePerChannelVolume =
            perChannel getCaptureVolume setCaptureVolume
