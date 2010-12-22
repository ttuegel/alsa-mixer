{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, TemplateHaskell #-} 
module Sound.ALSA.Mixer.Internal 
    ( Mixer()
    , SimpleElement()
    , SimpleElementId()
    , Channel(..)
    , mono
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

import qualified Data.ByteString.Char8 as B
import Foreign
import Foreign.C.Error ( eNOENT )
import Foreign.C.String ( CString )
import Foreign.C.Types
import Sound.ALSA.Exception ( checkResult_, throw )
import Sound.ALSA.Mixer.Templates

mono :: Channel
mono = FrontLeft

foreign import ccall "alsa/asoundlib.h snd_mixer_open"
  snd_mixer_open :: Ptr (Ptr MixerT) -> IO CInt

-- Static address import for finalizer. Suppression of the return type may not
-- be a good idea, but the workaround involves lots of C.
foreign import ccall "alsa/asoundlib.h &snd_mixer_close"
  snd_mixer_close :: FunPtr (Ptr MixerT -> IO ()) 

foreign import ccall "alsa/asoundlib.h snd_mixer_attach"
  snd_mixer_attach :: Ptr MixerT -> CString -> IO CInt

foreign import ccall "alsa/asoundlib.h snd_mixer_selem_id_malloc"
  snd_mixer_selem_id_malloc :: Ptr (Ptr SimpleElementIdT) -> IO ()

foreign import ccall "alsa/asoundlib.h snd_mixer_selem_register"
  snd_mixer_selem_register :: Ptr MixerT -> Ptr () -> Ptr () -> IO CInt

foreign import ccall "alsa/asoundlib.h snd_mixer_load"
  snd_mixer_load :: Ptr MixerT -> IO CInt

foreign import ccall "alsa/asoundlib.h snd_mixer_first_elem"
  snd_mixer_first_elem :: Ptr MixerT -> IO Element

foreign import ccall "alsa/asoundlib.h snd_mixer_last_elem"
  snd_mixer_last_elem :: Ptr MixerT -> IO Element

foreign import ccall "alsa/asoundlib.h snd_mixer_elem_next"
  snd_mixer_elem_next :: Element -> IO Element

foreign import ccall "alsa/asoundlib.h snd_mixer_find_selem"
  snd_mixer_find_selem :: Ptr MixerT -> Ptr SimpleElementIdT -> IO SimpleElement

foreign import ccall "alsa/asoundlib.h snd_mixer_selem_get_id"
  snd_mixer_selem_get_id :: Element -> Ptr SimpleElementIdT -> IO ()

open :: IO Mixer
open = do
    ppmix <- malloc
    pmix <- do
        snd_mixer_open ppmix >>= checkResult_ "snd_mixer_open"
        peek ppmix
    newForeignPtr snd_mixer_close pmix

attach :: Mixer -> String -> IO ()
attach fmix name = do
    B.useAsCString (B.pack name) $ \pName -> do
        withForeignPtr fmix $ \pmix -> do
            snd_mixer_attach pmix pName >>= checkResult_ "snd_mixer_attach"

load :: Mixer -> IO ()
load fmix = do
    withForeignPtr fmix $ \pmix -> do
        snd_mixer_selem_register pmix nullPtr nullPtr
            >>= checkResult_ "snd_mixer_selem_register"
        snd_mixer_load pmix >>= checkResult_ "snd_mixer_load"

getMixerByName :: String -> IO Mixer
getMixerByName name = do
    mix <- open
    attach mix name
    load mix
    return mix

elements :: Mixer -> IO [(SimpleElementId, SimpleElement)]
elements fMix = do
    withForeignPtr fMix $ \pMix -> do
        pFirst <- snd_mixer_first_elem pMix
        pLast <- snd_mixer_last_elem pMix
        es <- elements' pFirst [] pLast
        mapM (simpleElement fMix) es
  where elements' pThis xs pLast =
            case pThis == pLast of
                True -> return $ pThis : xs
                False -> do
                    pNext <- snd_mixer_elem_next pThis
                    elements' pNext (pThis : xs) pLast

simpleElement :: Mixer -> Element -> IO (SimpleElementId, SimpleElement)
simpleElement fMix pElem = do
    withForeignPtr fMix $ \pMix -> do
        fId <- getId pElem
        pSElem <- withForeignPtr fId $ snd_mixer_find_selem pMix
        if pSElem == nullPtr
            then throw "snd_mixer_find_selem" eNOENT
            else return (fId, pSElem)

foreign import ccall "alsa/asoundlib.h &snd_mixer_selem_id_free"
  snd_mixer_selem_id_free :: FunPtr (Ptr SimpleElementIdT -> IO ())

getId :: Element -> IO SimpleElementId
getId pElem = do
    pId <- alloca $ \ppId -> do
        snd_mixer_selem_id_malloc ppId
        peek ppId
    snd_mixer_selem_get_id pElem pId
    newForeignPtr snd_mixer_selem_id_free pId

foreign import ccall "alsa/asoundlib.h snd_mixer_selem_id_get_name"
  snd_mixer_selem_id_get_name :: Ptr SimpleElementIdT -> IO (Ptr CString)

getName :: SimpleElementId -> IO String
getName fId = do
    withForeignPtr fId $ \pId -> do
        pStr <- snd_mixer_selem_id_get_name pId
        cStr <- peek pStr
        bStr <- B.packCString cStr
        return $ B.unpack bStr

foreign import ccall "alsa/asoundlib.h snd_mixer_selem_id_get_index"
  snd_mixer_selem_id_get_index :: Ptr SimpleElementIdT -> IO CInt

getIndex :: SimpleElementId -> IO Integer
getIndex fId = do
    withForeignPtr fId $ \pId -> do
        cIndex <- snd_mixer_selem_id_get_index pId
        return $! fromIntegral cIndex

$(has "snd_mixer_selem_is_playback_mono" "isPlaybackMono")
$(has "snd_mixer_selem_is_capture_mono" "isCaptureMono")
$(has "snd_mixer_selem_has_common_volume" "hasCommonVolume")
$(has "snd_mixer_selem_has_playback_volume" "hasPlaybackVolume")
$(has "snd_mixer_selem_has_playback_volume_joined" "hasPlaybackVolumeJoined")
$(has "snd_mixer_selem_has_capture_volume" "hasCaptureVolume")
$(has "snd_mixer_selem_has_capture_volume_joined" "hasCaptureVolumeJoined")
$(has "snd_mixer_selem_has_common_switch" "hasCommonSwitch")
$(has "snd_mixer_selem_has_playback_switch" "hasPlaybackSwitch")
$(has "snd_mixer_selem_has_playback_switch_joined" "hasPlaybackSwitchJoined")
$(has "snd_mixer_selem_has_capture_switch" "hasCaptureSwitch")
$(has "snd_mixer_selem_has_capture_switch_joined" "hasCaptureSwitchJoined")

has2 :: (a -> b -> IO CInt) -> a -> b -> IO Bool
has2 f x y = do
    h <- f x y
    return $! h == 1

hasChannel :: (a -> CInt -> IO CInt) -> a -> Channel -> IO Bool
hasChannel f x chan = has2 f x $ fromIntegral $ fromEnum chan

foreign import ccall "alsa/asoundlib.h snd_mixer_selem_has_playback_channel"
  snd_mixer_selem_has_playback_channel :: Ptr SimpleElementT -> CInt -> IO CInt

hasPlaybackChannel :: SimpleElement -> Channel -> IO Bool
hasPlaybackChannel = hasChannel snd_mixer_selem_has_playback_channel

foreign import ccall "alsa/asoundlib.h snd_mixer_selem_has_capture_channel"
  snd_mixer_selem_has_capture_channel :: Ptr SimpleElementT -> CInt -> IO CInt

hasCaptureChannel :: SimpleElement -> Channel -> IO Bool
hasCaptureChannel = hasChannel snd_mixer_selem_has_capture_channel

$(getVol "snd_mixer_selem_get_playback_volume" "getPlaybackVolume")
$(getVol "snd_mixer_selem_get_capture_volume" "getCaptureVolume")
$(getVol "snd_mixer_selem_get_playback_dB" "getPlaybackDb")
$(getVol "snd_mixer_selem_get_capture_dB" "getCaptureDb")

$(getSwitch "snd_mixer_selem_get_playback_switch" "getPlaybackSwitch")
$(getSwitch "snd_mixer_selem_get_capture_switch" "getCaptureSwitch")

$(setVol "snd_mixer_selem_set_playback_volume" "setPlaybackVolume")
$(setVol "snd_mixer_selem_set_capture_volume" "setCaptureVolume")

$(setDb "snd_mixer_selem_set_playback_dB" "setPlaybackDb")
$(setDb "snd_mixer_selem_set_capture_dB" "setCaptureDb")

$(setVolAll "snd_mixer_selem_set_playback_volume_all" "setPlaybackVolumeAll")
$(setVolAll "snd_mixer_selem_set_capture_volume_all" "setCaptureVolumeAll")

$(setDbAll "snd_mixer_selem_set_playback_dB_all" "setPlaybackDbAll")
$(setDbAll "snd_mixer_selem_set_capture_dB_all" "setCaptureDbAll")

$(setSwitch "snd_mixer_selem_set_playback_switch" "setPlaybackSwitch")
$(setSwitch "snd_mixer_selem_set_capture_switch" "setCaptureSwitch")

$(setSwitchAll "snd_mixer_selem_set_playback_switch_all" "setPlaybackSwitchAll")
$(setSwitchAll "snd_mixer_selem_set_capture_switch_all" "setCaptureSwitchAll")

$(getRange "snd_mixer_selem_get_playback_volume_range" "getPlaybackVolumeRange")
$(getRange "snd_mixer_selem_get_playback_dB_range" "getPlaybackDbRange")
$(getRange "snd_mixer_selem_get_capture_volume_range" "getCaptureVolumeRange")
$(getRange "snd_mixer_selem_get_capture_dB_range" "getCaptureDbRange")

$(setRange "snd_mixer_selem_set_playback_volume_range" "setPlaybackVolumeRange")
$(setRange "snd_mixer_selem_set_capture_volume_range" "setCaptureVolumeRange")
