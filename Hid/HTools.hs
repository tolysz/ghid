{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hid.HTools where

import Hid.HTypes
import Text.Printf (printf)

--import Hid.Hex



-- Reversed Bit to unsigned int
--rb2uint:: RBitString -> Int
--rb2uint (RBS a) = bval a


bin2hex :: [Char] -> [Char]
bin2hex  = concatMap (\c -> printf "%02X "((toEnum (fromEnum c))::Int ))

---------------OK-----------------
b2uint:: BitString -> UInt
b2uint (BS a) = mkUInt ( bval $ reverse a) (length a)

---------------OK-----------------
uint2b ::UInt -> BitString
uint2b = fromStr . toStr . uint2rb


-- Text to/from Hex
--RHEX is totaly different endianess

---------------OK-----------------
xtexthex2rhex :: String -> RHexString
xtexthex2rhex = hex2rhex . texthex2hex

---------------OK-----------------
textrhex2rhex :: String -> RHexString
textrhex2rhex = RHS . hd .uc

---------------OK-----------------
xtextrhex2hex :: String -> HexString
xtextrhex2hex = rhex2hex . textrhex2rhex


---------------OK-----------------
texthex2rbin :: String -> RBitString
texthex2rbin = hex2rbin . texthex2hex
---------------OK-----------------
texthex2bin :: String -> BitString
texthex2bin = hex2bin . texthex2hex

---------------OK-----------------
textrbin2rbin :: String -> RBitString
textrbin2rbin = RBS . hd

---------------OK-----------------
textbin2rbin :: String -> RBitString
textbin2rbin = fromStr . hd


-- Hex Strings
---------------OK-----------------
hex2rhex :: HexString -> RHexString
hex2rhex = fromStr . reverse . twoswap . reverse . toStr

rhex2hex :: RHexString -> HexString
rhex2hex = fromStr . reverse . twoswap . reverse . toStr


-----------------OK----------------
xrhex2rbin ::RHexString -> RBitString
xrhex2rbin a = conv (concatMap h2b) a


hex2bin :: HexString -> BitString
hex2bin = conv ((concatMap h2b))

fhex2bin :: String -> String
fhex2bin a = concatMap h2rb (fourswap a) 




-----------------OK----------------
rbin2rhex :: RBitString -> RHexString
rbin2rhex  = RHS . reverse .(map b2h) . four . toStr

-----------------OK----------------
rbin2hex :: RBitString -> HexString
rbin2hex  = rhex2hex . rbin2rhex

-- Item conversions
item2sitem :: Item -> SItem
item2sitem (IT (a,b,c))
               | (length $ toStr b) == 4 = SI ( toEnum ((fromEnum . rb2uint) (b+++a)):: MType, c)
               | otherwise = SI (toEnum (64 + (fromEnum $ rb2uint b)):: MType, c)

sitem2item :: SItem -> Item
sitem2item (SI (b,c))
        | eb < 64 = IT (a1,b1,c)
        | otherwise = IT (RBS "11",(uint2rb . toEnum) (eb - 64),c)
            where
               eb = fromEnum b
               b1 = conv (take 4) $ uint2rb $ toEnum eb
               a1 = conv ((take 2).(drop 4)) $ uint2rb $ toEnum  eb

removeCLineComments :: String -> String
removeCLineComments = unlines . (map removeLineComment) .lines
  where
    removeLineComment "" = ""
    removeLineComment ('/':'/':_) = ""
    removeLineComment  (a:as) = a : removeLineComment as

removeHLineComments :: String -> String
removeHLineComments = unlines . (map removeLineComment) .lines
  where
    removeLineComment "" = ""
    removeLineComment ('-':'-':_) = ""
    removeLineComment  (a:as) = a : removeLineComment as

removeCBlockComents :: String -> String
removeCBlockComents = startRemove
  where
    startRemove "" = ""
    startRemove ('/':'*':as) = stopRemove as
    startRemove (a:as) = a : startRemove as

    stopRemove "" = ""
    stopRemove ('*':'/':as) = startRemove as
    stopRemove (_:as) = stopRemove as

removeHBlockComents :: String -> String
removeHBlockComents = startRemove
  where
    startRemove "" = ""
    startRemove ('{':'-':as) = stopRemove as
    startRemove (a:as) = a : startRemove as

    stopRemove "" = ""
    stopRemove ('-':'}':as) = startRemove as
    stopRemove (_:as) = stopRemove as

