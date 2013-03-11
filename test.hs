
import System.IO (hSetBinaryMode, stdout, stdin)
import System.Environment (getArgs)

import Hid.HTypes
import Hid.HTools
import Hid.HParse

import Hid.Export
import Hid.Import

import qualified Data.ByteString.Lazy.Char8 as L8


-- how to use
-- read https://code.google.com/p/ghid/wiki/gHid

main :: IO ()
main = do
        args <- getArgs
        hSetBinaryMode stdin True
        hSetBinaryMode stdout True
        y1 <- L8.getContents
        let y = L8.unpack y1
        let preprocessed = case (head (args++[""])) of
             "h" -> fromCArray1
             "dat" -> fromDat
             "short" ->  fromShortHand
             "smart" ->  fromShortHand
             "txt" -> hsparse . hex2rbin . texthex2hex
             "bin" -> fromDat . bin2hex
             "hid" -> fromDat . fromHid . filter (' '/=) . bin2hex
             _ -> fromShortHand
        let postprocessor = case ((head . tail) (args++[""]++[""])) of
             "h" -> toCArray ((head . tail . tail) (args ++ ["rdesc"])) -- nicely tabed C array with // comments
             "hc" -> toCArrayC ((head . tail . tail) (args ++ ["rdesc"])) -- nicely tabed C array with /* */ comments
             "hd" -> toCArrayDirty
             "dat" ->  toDat              -- Text hex spaced format as in kernel 
             "bin" ->  datToBin . toDat
             "sitem" -> show
             "short" -> toShort
             "smart" -> toShort
             "simple" -> toSmartSimple
             "id"     -> show              -- internal parse format
             "thid"    -> toHid            -- Descriptor Tool Format (as text)
             "hid"    -> datToBin . toHid  -- Descriptor Tool Format
             _ -> toShort
        (putStr . postprocessor . preprocessed) y

