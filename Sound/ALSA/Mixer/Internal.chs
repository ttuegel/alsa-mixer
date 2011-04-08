{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Sound.ALSA.Mixer.Internal
    ( Mixer()
    , SimpleElement()
    , SimpleElementId()
    , Channel(..)
    , allChannels
    , elements
    , getMixerByName
    , isPlaybackMono
    , isCaptureMono
    , hasPlaybackChannel
    , hasCaptureChannel
    , hasCommonVolume
    , hasPlaybackVolume
    , hasPlaybackVolumeJoined
    , hasCaptureVolume
    , hasCaptureVolumeJoined
    , hasCommonSwitch
    , hasPlaybackSwitch
    , hasPlaybackSwitchJoined
    , hasCaptureSwitch
    , hasCaptureSwitchJoined
    , getPlaybackVolume
    , getCaptureVolume
    , getPlaybackDb
    , getCaptureDb
    , getPlaybackSwitch
    , getCaptureSwitch
    , setPlaybackVolume
    , setCaptureVolume
    , setPlaybackDb
    , setCaptureDb
    , setPlaybackVolumeAll
    , setCaptureVolumeAll
    , setPlaybackDbAll
    , setCaptureDbAll
    , setPlaybackSwitch
    , setCaptureSwitch
    , setPlaybackSwitchAll
    , setCaptureSwitchAll
    , getPlaybackVolumeRange
    , getPlaybackDbRange
    , getCaptureVolumeRange
    , getCaptureDbRange
    , setPlaybackVolumeRange
    , setCaptureVolumeRange
    , getName
    , getIndex
    ) where

import Foreign
import Foreign.C.Error ( eNOENT )
import Foreign.C.String
import Foreign.C.Types
import Sound.ALSA.Exception ( checkResult_, throw )

#include "alsa/asoundlib.h"
{#context lib = "asoundlib" #}

{#pointer *snd_mixer_t as Mixer foreign#}
{#pointer *snd_mixer_elem_t as Element#}
{#pointer *snd_mixer_selem_id_t as SimpleElementId foreign#}
type SimpleElement = (Mixer, Element)

{#enum snd_mixer_selem_channel_id_t as Channel
    { SND_MIXER_SCHN_UNKNOWN as Unknown
    , SND_MIXER_SCHN_FRONT_LEFT as FrontLeft
    , SND_MIXER_SCHN_FRONT_RIGHT as FrontRight
    , SND_MIXER_SCHN_REAR_LEFT as RearLeft
    , SND_MIXER_SCHN_REAR_RIGHT as RearRight
    , SND_MIXER_SCHN_FRONT_CENTER as FrontCenter
    , SND_MIXER_SCHN_WOOFER as Woofer
    , SND_MIXER_SCHN_SIDE_LEFT as SideLeft
    , SND_MIXER_SCHN_SIDE_RIGHT as SideRight
    , SND_MIXER_SCHN_REAR_CENTER as RearCenter
    , SND_MIXER_SCHN_LAST as Last
    } deriving (Eq, Read, Show) #}

allChannels :: [Channel]
allChannels = map toEnum $ enumFromTo (fromEnum FrontLeft) (fromEnum RearCenter)

-----------------------------------------------------------------------
-- open
-- --------------------------------------------------------------------

{#fun snd_mixer_open as open
    { mallocMixer- `Mixer' peekMixer*
    , modeZero- `Int' fromIntegral- } -> `Int' checkOpen*- #}

mallocMixer f = do
    pm <- malloc
    f pm

modeZero f = f $ fromIntegral 0
checkOpen = checkResult_ "snd_mixer_open"
peekMixer pm = peek pm >>= newForeignPtr snd_mixer_close

-- Static address import for finalizer. Suppression of the return type may
-- not be a good idea, but the workaround involves lots of C.
foreign import ccall "alsa/asoundlib.h &snd_mixer_close"
  snd_mixer_close :: FunPtr (Ptr () -> IO ())

-----------------------------------------------------------------------
-- attach
-- --------------------------------------------------------------------

{#fun snd_mixer_attach as attach
    { withForeignPtr* `Mixer', `String' } -> `Int' checkAttach*- #}

checkAttach = checkResult_ "snd_mixer_attach"

-----------------------------------------------------------------------
-- load
-- --------------------------------------------------------------------

{#fun snd_mixer_load as ^
    { withForeignPtr* `Mixer' } -> `Int' checkSndMixerLoad*- #}

checkSndMixerLoad = checkResult_ "snd_mixer_load"

{#fun snd_mixer_selem_register as ^
    { withForeignPtr* `Mixer'
    , id `Ptr ()'
    , id `Ptr (Ptr ())' } -> `Int' checkSndMixerSelemRegister*- #}

checkSndMixerSelemRegister = checkResult_ "snd_mixer_selem_register"

load :: Mixer -> IO ()
load fmix = do
    sndMixerSelemRegister fmix nullPtr nullPtr
    sndMixerLoad fmix

-----------------------------------------------------------------------
-- getId
-- --------------------------------------------------------------------

{#fun snd_mixer_selem_id_malloc as ^
    { alloca- `SimpleElementId' peekSimpleElementId* } -> `()' #}

{#fun snd_mixer_selem_get_id as ^
    { id `Element', withForeignPtr* `SimpleElementId' } -> `()' #}

peekSimpleElementId pid = peek pid >>= newForeignPtr snd_mixer_selem_id_free

foreign import ccall "alsa/asoundlib.h &snd_mixer_selem_id_free"
  snd_mixer_selem_id_free :: FunPtr (Ptr () -> IO ())

getId :: Element -> IO SimpleElementId
getId e = do
   newSid <- sndMixerSelemIdMalloc
   sndMixerSelemGetId e newSid
   return newSid

-----------------------------------------------------------------------
-- elements
-- --------------------------------------------------------------------

{#fun snd_mixer_first_elem as ^
    { withForeignPtr* `Mixer' } -> `Element' id #}

{#fun snd_mixer_last_elem as ^
    { withForeignPtr* `Mixer' } -> `Element' id #}

{#fun snd_mixer_elem_next as ^
    { id `Element' } -> `Element' id #}

elements :: Mixer -> IO [(SimpleElementId, SimpleElement)]
elements fMix = do
    pFirst <- sndMixerFirstElem fMix
    pLast <- sndMixerLastElem fMix
    es <- elements' pFirst [] pLast
    mapM (simpleElement fMix) es
  where elements' pThis xs pLast | pThis == pLast = return $ pThis : xs
                                 | otherwise = do
                                     pNext <- sndMixerElemNext pThis
                                     elements' pNext (pThis : xs) pLast

-----------------------------------------------------------------------
-- simpleElement
-- --------------------------------------------------------------------

{#fun snd_mixer_find_selem as ^
    { withForeignPtr* `Mixer'
    , withForeignPtr* `SimpleElementId' } -> `Element' id #}

simpleElement :: Mixer -> Element -> IO (SimpleElementId, SimpleElement)
simpleElement fMix pElem = do
    fId <- getId pElem
    pSElem <- sndMixerFindSelem fMix fId
    if pSElem == nullPtr
        then throw "snd_mixer_find_selem" eNOENT
        else return (fId, (fMix, pSElem))

-----------------------------------------------------------------------
-- getName
-- --------------------------------------------------------------------

{#fun snd_mixer_selem_id_get_name as getName
    { withForeignPtr* `SimpleElementId' } -> `String' #}

-----------------------------------------------------------------------
-- getIndex
-- --------------------------------------------------------------------

{#fun snd_mixer_selem_id_get_index as getIndex
    { withForeignPtr* `SimpleElementId' } -> `Integer' fromIntegral #}

-----------------------------------------------------------------------
-- getMixerByName
-- --------------------------------------------------------------------

-- | Returns the named 'Mixer'. Will throw an exception if the named mixer
-- cannot be found. A mixer named \"default\" should always exist.
getMixerByName :: String -> IO Mixer
getMixerByName name = do
    mix <- open
    attach mix name
    load mix
    return mix

-----------------------------------------------------------------------
-- utilities
-- --------------------------------------------------------------------

cToBool = toBool

cFromBool = fromBool

withSimpleElement :: SimpleElement -> (Element -> IO a) -> IO a
withSimpleElement (m, s) f = do
    r <- f s
    touchForeignPtr m
    return r

channelToC = toEnum . fromEnum

cToIntegral = (>>= return . fromIntegral) . peek 

cFromIntegral :: Integer -> (Ptr CLong -> IO a) -> IO a
cFromIntegral = with . fromIntegral

negOne f = f $! negate 1

-----------------------------------------------------------------------
-- has
-- --------------------------------------------------------------------

{#fun snd_mixer_selem_is_playback_mono as isPlaybackMono
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_is_capture_mono as isCaptureMono
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_has_common_volume as hasCommonVolume
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_has_playback_volume as hasPlaybackVolume
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_has_playback_volume_joined as hasPlaybackVolumeJoined
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_has_capture_volume as hasCaptureVolume
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_has_capture_volume_joined as hasCaptureVolumeJoined
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_has_common_switch as hasCommonSwitch
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_has_playback_switch as hasPlaybackSwitch
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_has_playback_switch_joined as hasPlaybackSwitchJoined
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_has_capture_switch as hasCaptureSwitch
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_has_capture_switch_joined as hasCaptureSwitchJoined
    { withSimpleElement* `SimpleElement' } -> `Bool' #}

{#fun snd_mixer_selem_has_playback_channel as hasPlaybackChannel
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel' } -> `Bool' #}

{#fun snd_mixer_selem_has_capture_channel as hasCaptureChannel
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel' } -> `Bool' #}

-----------------------------------------------------------------------
-- get
-- --------------------------------------------------------------------

{#fun snd_mixer_selem_get_playback_volume as getPlaybackVolume
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , alloca- `Integer' cToIntegral* } -> `Int' checkGetPlaybackVolume*- #}

checkGetPlaybackVolume = checkResult_ "snd_mixer_selem_get_playback_volume"

{#fun snd_mixer_selem_get_capture_volume as getCaptureVolume
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , alloca- `Integer' cToIntegral* } -> `Int' checkGetCaptureVolume*- #}

checkGetCaptureVolume = checkResult_ "snd_mixer_selem_get_capture_volume"

{#fun snd_mixer_selem_get_playback_dB as getPlaybackDb
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , alloca- `Integer' cToIntegral* } -> `Int' checkPlaybackDb*- #}

checkPlaybackDb = checkResult_ "snd_mixer_selem_get_playback_dB"

{#fun snd_mixer_selem_get_capture_dB as getCaptureDb
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , alloca- `Integer' cToIntegral* } -> `Int' checkCaptureDb*- #}

checkCaptureDb = checkResult_ "snd_mixer_selem_get_capture_dB"

peekBool = (>>= return . cToBool) . peek

{#fun snd_mixer_selem_get_playback_switch as getPlaybackSwitch
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , alloca- `Bool' peekBool* } -> `Int' checkPlaybackSwitch*- #}

checkPlaybackSwitch = checkResult_ "snd_mixer_selem_get_playback_switch"

{#fun snd_mixer_selem_get_capture_switch as getCaptureSwitch
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , alloca- `Bool' peekBool* } -> `Int' checkCaptureSwitch*- #}

checkCaptureSwitch = checkResult_ "snd_mixer_selem_get_capture_switch"

{#fun snd_mixer_selem_get_playback_volume_range as getPlaybackVolumeRange
    { withSimpleElement* `SimpleElement'
    , alloca- `Integer' cToIntegral*
    , alloca- `Integer' cToIntegral* } -> `Int' checkGetPlaybackVolumeRange*- #}

checkGetPlaybackVolumeRange = checkResult_ "snd_mixer_selem_get_playback_volume_range"

{#fun snd_mixer_selem_get_capture_volume_range as getCaptureVolumeRange
    { withSimpleElement* `SimpleElement'
    , alloca- `Integer' cToIntegral*
    , alloca- `Integer' cToIntegral* } -> `Int' checkGetCaptureVolumeRange*- #}

checkGetCaptureVolumeRange = checkResult_ "snd_mixer_selem_get_capture_volume_range"

{#fun snd_mixer_selem_get_playback_dB_range as getPlaybackDbRange
    { withSimpleElement* `SimpleElement'
    , alloca- `Integer' cToIntegral*
    , alloca- `Integer' cToIntegral* } -> `Int' checkGetPlaybackDbRange*- #}

checkGetPlaybackDbRange = checkResult_ "snd_mixer_selem_get_playback_dB_range"

{#fun snd_mixer_selem_get_capture_dB_range as getCaptureDbRange
    { withSimpleElement* `SimpleElement'
    , alloca- `Integer' cToIntegral*
    , alloca- `Integer' cToIntegral* } -> `Int' checkGetCaptureDbRange*- #}

checkGetCaptureDbRange = checkResult_ "snd_mixer_selem_get_capture_dB_range"

-----------------------------------------------------------------------
-- set
-- --------------------------------------------------------------------

{#fun snd_mixer_selem_set_playback_volume as setPlaybackVolume
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , fromIntegral `Integer' } -> `Int' checkSetPlaybackVolume*- #}

checkSetPlaybackVolume = checkResult_ "snd_mixer_selem_set_playback_volume"

{#fun snd_mixer_selem_set_capture_volume as setCaptureVolume
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , fromIntegral `Integer' } -> `Int' checkSetCaptureVolume*- #}

checkSetCaptureVolume = checkResult_ "snd_mixer_selem_set_capture_volume"

{#fun snd_mixer_selem_set_playback_dB as setPlaybackDb
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , fromIntegral `Integer'
    , negOne- `Int' } -> `Int' checkSetPlaybackDb*- #}

checkSetPlaybackDb = checkResult_ "snd_mixer_selem_set_playback_dB"

{#fun snd_mixer_selem_set_capture_dB as setCaptureDb
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , fromIntegral `Integer'
    , negOne- `Int' } -> `Int' checkSetCaptureDb*- #}

checkSetCaptureDb = checkResult_ "snd_mixer_selem_set_capture_dB"

{#fun snd_mixer_selem_set_playback_volume_all as setPlaybackVolumeAll
    { withSimpleElement* `SimpleElement'
    , fromIntegral `Integer' } -> `Int' checkSetPlaybackVolumeAll*- #}

checkSetPlaybackVolumeAll = checkResult_ "snd_mixer_selem_set_playback_volume_all"

{#fun snd_mixer_selem_set_capture_volume_all as setCaptureVolumeAll
    { withSimpleElement* `SimpleElement'
    , fromIntegral `Integer' } -> `Int' checkSetCaptureVolumeAll*- #}

checkSetCaptureVolumeAll = checkResult_ "snd_mixer_selem_set_capture_volume_all"

{#fun snd_mixer_selem_set_playback_dB_all as setPlaybackDbAll
    { withSimpleElement* `SimpleElement'
    , fromIntegral `Integer'
    , negOne- `Int' } -> `Int' checkSetPlaybackDbAll*- #}

checkSetPlaybackDbAll = checkResult_ "snd_mixer_selem_set_playback_dB_all"

{#fun snd_mixer_selem_set_capture_dB_all as setCaptureDbAll
    { withSimpleElement* `SimpleElement'
    , fromIntegral `Integer'
    , negOne- `Int' } -> `Int' checkSetCaptureDbAll*- #}

checkSetCaptureDbAll = checkResult_ "snd_mixer_selem_set_capture_dB_all"

{#fun snd_mixer_selem_set_playback_switch as setPlaybackSwitch
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , `Bool' } -> `Int' checkSetPlaybackSwitch*- #}

checkSetPlaybackSwitch = checkResult_ "snd_mixer_selem_set_playback_switch"

{#fun snd_mixer_selem_set_capture_switch as setCaptureSwitch
    { withSimpleElement* `SimpleElement'
    , channelToC `Channel'
    , `Bool' } -> `Int' checkSetCaptureSwitch*- #}

checkSetCaptureSwitch = checkResult_ "snd_mixer_selem_set_capture_switch"

{#fun snd_mixer_selem_set_playback_switch_all as setPlaybackSwitchAll
    { withSimpleElement* `SimpleElement'
    , `Bool' } -> `Int' checkSetPlaybackSwitchAll*- #}

checkSetPlaybackSwitchAll = checkResult_ "snd_mixer_selem_set_playback_switch_all"

{#fun snd_mixer_selem_set_capture_switch_all as setCaptureSwitchAll
    { withSimpleElement* `SimpleElement'
    , `Bool' } -> `Int' checkSetCaptureSwitchAll*- #}

checkSetCaptureSwitchAll = checkResult_ "snd_mixer_selem_set_capture_switch_all"

{#fun snd_mixer_selem_set_playback_volume_range as setPlaybackVolumeRange'
    { withSimpleElement* `SimpleElement'
    , fromIntegral `Integer'
    , fromIntegral `Integer' } -> `Int' checkSetPlaybackVolumeRange*- #}

checkSetPlaybackVolumeRange = checkResult_ "snd_mixer_selem_set_playback_volume_range"

{#fun snd_mixer_selem_set_capture_volume_range as setCaptureVolumeRange'
    { withSimpleElement* `SimpleElement'
    , fromIntegral `Integer'
    , fromIntegral `Integer' } -> `Int' checkSetCaptureVolumeRange*- #}

checkSetCaptureVolumeRange = checkResult_ "snd_mixer_selem_set_capture_volume_range"

setPlaybackVolumeRange m = uncurry (setPlaybackVolumeRange' m)
setCaptureVolumeRange m = uncurry (setCaptureVolumeRange' m)
