{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}
module Sound.ALSA.Mixer.Templates where

import Foreign
import Foreign.C.Types
import Language.Haskell.TH
import Sound.ALSA.Exception ( checkResult_ )

data MixerT
data ElemT
data SimpleElementIdT
data SimpleElementT

type Mixer = ForeignPtr MixerT
type Element = Ptr ElemT
type SimpleElementId = ForeignPtr SimpleElementIdT
type SimpleElement = Ptr SimpleElementT

data Channel = Unknown
             | FrontLeft
             | FrontRight
             | RearLeft
             | RearRight
             | FrontCenter
             | Woofer
             | SideLeft
             | SideRight
             | RearCenter

instance Enum Channel where
    fromEnum Unknown = -1
    fromEnum FrontLeft = 0
    fromEnum FrontRight = 1
    fromEnum RearLeft = 2
    fromEnum RearRight = 3
    fromEnum FrontCenter = 4
    fromEnum Woofer = 5
    fromEnum SideLeft = 6
    fromEnum SideRight = 7
    fromEnum RearCenter = 8

    toEnum (-1) = Unknown
    toEnum 0 = FrontLeft
    toEnum 1 = FrontRight
    toEnum 2 = RearLeft
    toEnum 3 = RearRight
    toEnum 4 = FrontCenter
    toEnum 5 = Woofer
    toEnum 6 = SideLeft
    toEnum 7 = SideRight
    toEnum 8 = RearCenter
    toEnum _ = error "Sound.ALSA.Mixer.Channel.toEnum: integer out of bounds."

has :: String -> String -> Q [Dec]
has = template frgn hask body
  where frgn = [t| Ptr SimpleElementT -> IO CInt |]
        hask = [t| Ptr SimpleElementT -> IO Bool |]
        body frgnName = [| \pElem -> do ret <- $(varE frgnName) pElem
                                        return $ 1 == ret
                         |]



getVol :: String -> String -> Q [Dec]
getVol frgnStr = template frgn hask body frgnStr
  where frgn = [t| Ptr SimpleElementT -> CInt -> Ptr CLong -> IO CInt |]
        hask = [t| Ptr SimpleElementT -> Channel -> IO Integer |]
        body frgnName = [| \pElem chan -> do
                               let iChan = fromIntegral $! fromEnum chan
                               vol <- alloca $ \pVol -> do
                                   ret <- $(varE frgnName) pElem iChan pVol
                                   checkResult_ frgnStr ret
                                   peek pVol
                               return $! fromIntegral vol
                         |]

getSwitch :: String -> String -> Q [Dec]
getSwitch frgnStr = template frgn hask body frgnStr
  where frgn = [t| Ptr SimpleElementT -> CInt -> Ptr CInt -> IO CInt |]
        hask = [t| Ptr SimpleElementT -> Channel -> IO Bool |]
        body frgnName = [| \pElem chan -> do
                               let iChan = fromIntegral $! fromEnum chan
                               iBool <- alloca $ \pBool -> do
                                   ret <- $(varE frgnName) pElem iChan pBool
                                   checkResult_ frgnStr ret
                                   peek pBool
                               return $! iBool == 1 
                         |]

setVol :: String -> String -> Q [Dec]
setVol frgnStr = template frgn hask body frgnStr
  where frgn = [t| Ptr SimpleElementT -> CInt -> CLong -> IO CInt |]
        hask = [t| Ptr SimpleElementT -> Channel -> Integer -> IO () |]
        body frgnName = [| \pElem chan vol -> do
                               let iChan = fromIntegral $! fromEnum chan
                                   iVol = fromIntegral $! vol
                               ret <- $(varE frgnName) pElem iChan iVol
                               checkResult_ frgnStr ret
                         |]

setDb :: String -> String -> Q [Dec]
setDb frgnStr = template frgn hask body frgnStr
  where frgn = [t| Ptr SimpleElementT -> CInt -> CLong -> CInt -> IO CInt |]
        hask = [t| Ptr SimpleElementT -> Channel -> Integer -> IO () |]
        body frgnName = [| \pElem chan vol -> do
                               let iChan = fromIntegral $! fromEnum chan
                                   iVol = fromIntegral $! vol
                               ret <- $(varE frgnName) pElem iChan iVol (-1)
                               checkResult_ frgnStr ret
                         |]

setVolAll :: String -> String -> Q [Dec]
setVolAll frgnStr = template frgn hask body frgnStr
  where frgn = [t| Ptr SimpleElementT -> CLong -> IO CInt |]
        hask = [t| Ptr SimpleElementT -> Integer -> IO () |]
        body frgnName = [| \pElem vol -> do
                               let iVol = fromIntegral $! vol
                               ret <- $(varE frgnName) pElem iVol
                               checkResult_ frgnStr ret
                         |]

setDbAll :: String -> String -> Q [Dec]
setDbAll frgnStr = template frgn hask body frgnStr
  where frgn = [t| Ptr SimpleElementT -> CLong -> CInt -> IO CInt |]
        hask = [t| Ptr SimpleElementT -> Integer -> IO () |]
        body frgnName = [| \pElem vol -> do
                               let iVol = fromIntegral $! vol
                               ret <- $(varE frgnName) pElem iVol (-1)
                               checkResult_ frgnStr ret
                         |]

setSwitch :: String -> String -> Q [Dec]
setSwitch frgnStr = template frgn hask body frgnStr
  where frgn = [t| Ptr SimpleElementT -> CInt -> CInt -> IO CInt |]
        hask = [t| Ptr SimpleElementT -> Channel -> Bool -> IO () |]
        body frgnName = [| \pElem chan bool -> do
                               let iChan = fromIntegral $! fromEnum chan 
                                   iBool = if bool then 1 else 0
                               ret <- $(varE frgnName) pElem iChan iBool
                               checkResult_ frgnStr ret
                         |]

setSwitchAll :: String -> String -> Q [Dec]
setSwitchAll frgnStr = template frgn hask body frgnStr
  where frgn = [t| Ptr SimpleElementT -> CInt -> IO CInt |]
        hask = [t| Ptr SimpleElementT -> Bool -> IO () |]
        body frgnName = [| \pElem bool -> do
                               let iBool = if bool then 1 else 0
                               ret <- $(varE frgnName) pElem iBool
                               checkResult_ frgnStr ret
                         |]

getRange :: String -> String -> Q [Dec]
getRange frgnStr = template frgn hask body frgnStr
  where frgn = [t| Ptr SimpleElementT -> Ptr CLong -> Ptr CLong -> IO CInt |]
        hask = [t| Ptr SimpleElementT -> IO (Integer, Integer) |]
        body frgnName = [| \pElem -> do
                               alloca $ \pMin ->
                                 alloca $ \pMax -> do
                                   ret <- $(varE frgnName) pElem pMin pMax
                                   checkResult_ frgnStr ret
                                   cMin <- peek pMin
                                   cMax <- peek pMax
                                   return $!
                                       (fromIntegral cMin, fromIntegral cMax)
                         |]

setRange :: String -> String -> Q [Dec]
setRange frgnStr = template frgn hask body frgnStr
  where frgn = [t| Ptr SimpleElementT -> CLong -> CLong -> IO CInt |]
        hask = [t| Ptr SimpleElementT -> (Integer, Integer) -> IO () |]
        body frgnName = [| \pElem (cMin, cMax) -> do
                               let iMin = fromIntegral $! cMin
                                   iMax = fromIntegral $! cMax
                               ret <- $(varE frgnName) pElem iMin iMax
                               checkResult_ frgnStr ret
                         |]

template :: Q Type -> Q Type -> (Name -> Q Exp) -> String -> String -> Q [Dec]
template frgnType haskType body frgn hask = do
    let frgnImp = "alsa/asoundlib.h " ++ frgn
        frgnName = mkName frgn
        haskName = mkName hask
    frgnDec <- forImpD cCall safe frgnImp frgnName frgnType
    let haskBody = normalB $ body frgnName
        haskClause = clause [] haskBody []
    haskDec <- funD haskName [ haskClause ]
    haskSig <- sigD haskName haskType
    return [ frgnDec, haskSig, haskDec ]
