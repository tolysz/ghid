
module Hid.HToName where

import Hid.HTypes
import Hid.HTools
import Hid.UPG

repa :: Item -> HState -> (MType, [String])
repa i s = srepa (item2sitem i,s)

srepa :: (SItem, HState) -> (MType, [String])
srepa ((SI (b,c)),s) = (b, toName2 b c s)

srepa2 :: (SItem, HState) -> (MType, RBitString, HState, [String])
srepa2 ((SI (b,c)),s) = (b, c , s,toName2 b c s)


walk :: HState -> [SItem] -> [(SItem,HState)]
walk s l  = zip l $ scanl updateState s l

{--
uS s (a,b) x = s {a = z}
          where
            y = a s
            z = y{b=x}
--}
updateState :: HState -> SItem -> HState
updateState s (SI(GUsagePage, a)) = s {sCurrent = (sCurrent s){sUsagePage = a}}
updateState s (SI(GUnit, a)) = s {sCurrent = (sCurrent s){sUnit = a}}
updateState s (SI(GUnitExponent, a)) = s {sCurrent = (sCurrent s){sUnitExponent = a}}
updateState s (SI(GReportID, a)) = s {sCurrent = (sCurrent s){sReportID = a}}
updateState s (SI(GReportCount, a)) = s {sCurrent = (sCurrent s){sReportCount = a}}
updateState s (SI(GReportSize, a)) = s {sCurrent = (sCurrent s){sReportSize = a}}
updateState s (SI(GLogicalMinimum, a)) = s {sCurrent = (sCurrent s){sLogicalMinimum = a}}
updateState s (SI(GLogicalMaximum, a)) = s {sCurrent = (sCurrent s){sLogicalMaximum = a}}
updateState s (SI(GPhysicalMinimum, a)) = s {sCurrent = (sCurrent s){sPhysicalMinimum = a}}
updateState s (SI(GPhysicalMaximum, a)) = s {sCurrent = (sCurrent s){sPhysicalMaximum = a}}
updateState s (SI(GPop, _)) = s {sCurrent = head (sPushPop s), sPushPop = tail(sPushPop s) }
updateState s (SI(GPush, _)) = s {sPushPop = (sCurrent s):(sPushPop s)}

updateState s (SI(MCollection, _)) = s {sStack = (sCurrent s):(sStack s),sCurrent = (sCurrent s){sCollectionUsage=head $ (sUsage $ sLocal s)++[sCollectionUsage $ sCurrent s], sCollectionPage=sUsagePage $ sCurrent s}}
updateState s (SI(MEndCollection, _)) = s {sCurrent = head (sStack s), sStack = tail(sStack s) }

updateState s (SI(MInput, _)) = s {sLocal = emptyLocalState}
updateState s (SI(MFeature, _)) = s {sLocal = emptyLocalState}
updateState s (SI(MOutput, _)) = s {sLocal = emptyLocalState}

updateState s (SI(LUsageMinimum, a)) = s {sLocal = (sLocal s){sUsageMinimum = a,sLocalUsagePage=sUsagePage $sCurrent s}}
updateState s (SI(LUsageMaximum, a)) = s {sLocal = (sLocal s){sUsageMaximum = a,sLocalUsagePage=sUsagePage $sCurrent s}}

updateState s (SI(LUsage, a)) = s {sLocal = ls {sUsage = a: (sUsage ls),sLocalUsagePage=sUsagePage $sCurrent s}}
               where
                 ls = (sLocal s)
updateState s _ = s

toName2 :: MType -> RBitString -> HState -> [String]
toName2 MCollection c _ = [coll (rb2uint2int c)]
         where
          coll a
            | a == 0x00 = "Physical (group of axes)"
            | a == 0x01 = "Application (mouse, keyboard)"
            | a == 0x02 = "Logical (interrelated data)"
            | a == 0x03 = "Report"
            | a == 0x04 = "Named Array"
            | a == 0x05 = "Usage Switch"
            | a == 0x06 = "Usage Modifier"
            | (a >= 0x07) && (a <= 0x7F) =  "Collection Reserved " ++ showihex a
            | (a >= 0x80) && (a <= 0xFF) = "Collection Vendor-defined " ++ showihex a
            | otherwise = error ("Collection out of bounds " ++ (show a))
toName2 GUsagePage a _ = [toPageName (rb2uint2int a)]
toName2 LUsage a s = wrr a  $ toUsageName' ((sUsagePage . sCurrent) s)  a
toName2 LUsageMinimum a s = wrr a  $ toUsageName' ((sUsagePage . sCurrent) s)  a
toName2 LUsageMaximum a s = wrr a  $ toUsageName' ((sUsagePage . sCurrent) s) a
toName2 MInput a _ = wnl a (biz inputBitsS a )
toName2 MOutput a _ = wnl a (biz outputBitsS a )
toName2 MFeature a _ = wnl a (biz featureBitsS a )
toName2 MEndCollection a s = wrrnl a $  toUsageName' (( sCollectionPage . sCurrent) s)((sCollectionUsage . sCurrent) s) 
toName2 GLogicalMinimum a _ = wrr a  $ toSigned a
toName2 GLogicalMaximum a _ = wrr a  $ toSigned a
toName2 GPhysicalMinimum a _ = wrr a $ toSigned a
toName2 GPhysicalMaximum a _ = wrr a $ toSigned a
toName2 GUnitExponent a _ = wrr a $ toSigned a
toName2 GUnit a _ = wrr a $ toUnit a
toName2 LDelimiter a _ = case (rb2uint2int a) of
              0 ->  ["end delimiter"]
              1 ->  ["start delimiter"]
toName2  _ a _ = puint a

puint :: RBitString -> [String]
puint ra@(RBS a)
    | length a == 0 = []
    | otherwise = [(bval' ra)++" [" ++ (rbin2textuint ra) ++"]"]

toUsageName' :: RBitString -> RBitString -> String
toUsageName' j i
    | (length.toStr) i <= 16 = toUsageName (rb2uint2int j) (rb2uint2int i)
    | otherwise =(toPageName up)++" : " ++(toUsageName up u)
        where
          up = (div (rb2uint2int i) 65536 )
          u = (mod (rb2uint2int i) 65536)

wrr :: RBitString -> String -> [String]
wrr a s = [(bval' a ) ++ " [" ++ s ++"]"]

wrrnl :: RBitString -> String -> [String]
--wrrnl a s = [(bval' a ) ++ " [" ++ s ++"]\n"] --
wrrnl a s = [(bval' a ) ++ " [" ++ s ++"]"] --

wnl :: RBitString -> [String] -> [String]
wnl a s= [(bval' a )]++s 

bval' :: RBitString -> String
bval' (RBS "") = ""
bval' a = rbin2textrhex a

toSigned :: RBitString -> String
toSigned = rbin2textsint

toUnit :: RBitString -> String
toUnit a = (g 0) ++" ("++ concatMap (\(x,j) -> case x of {0->"";1->" "++g j;_->" "++(g j)++"^"++(sig x)} ) (zip expq [1..]) ++" )"
     where
       s = toStr a
       e = (four s)
       syst = (rb2uint2int . textrbin2rbin . head) e
       expq = map (rb2sint2int . textrbin2rbin) ( tail e)
       g j = getIJ basicUnitsS j syst
       sig d 
          | d < 0 = "("++(show d) ++")"
          | otherwise = show d

--ssint = show . fromEnum . rb2sint
ssint :: RBitString -> String
ssint = show . rb2sint2int

biz :: [[String]] -> RBitString -> [String]
biz a s = [biz' a (toStr s) 0]
  where 
    biz'::[[String]] -> String -> Int -> String
    biz' d (b:bs) i 
        | b=='1' || i < 3 = (getIJ d i (sbit b)) ++"["++show i++"] "++ (biz' d bs (i+1))
        | otherwise = biz' d bs (i+1)
--    biz' _ [] _ = "\n"
    biz' _ [] _ = ""

getIJ :: [[String]] -> Int -> Int -> String
getIJ a i j = (a !! i1) !! j1
             where
               i1 = min i (-1 + length a)
               j1 = min j (-1 + length (a!!i1))

basicUnits :: [[String]]
basicUnits = [["None", "SI Linear",  "SI Rotation", "English Linear", "English Rotation", "Reserved"],
              ["None", "Centimeter", "Radians",     "Inch",           "Degrees",          "Reserved"],
              ["None", "Gram",       "Gram",        "Slug",           "Slug",             "Reserved"],
              ["None", "Seconds",    "Seconds",     "Seconds",        "Seconds",          "Reserved"],
              ["None", "Kelvin",     "Kelvin",      "Fahrenheit",     "Fahrenheit",       "Reserved"],
              ["None", "Ampere",     "Ampere",      "Ampere",         "Ampere",           "Reserved"],
              ["None", "Candela",    "Candela",     "Candela",        "Candela",          "Reserved"],
              ["None", "None",       "None",        "None",           "None",             "Reserved"]]

basicUnitsS :: [[String]]
basicUnitsS = [["", "SI Lin", "SI Rot", "Eng Lin", "Eng Rot", "Reserved"],
              ["",  "cm",     "rad",    "in",      "deg",     "Reserved"],
              ["",  "g",      "g",      "slug",    "slug",    "Reserved"],
              ["",  "s",      "s",      "s",       "s",       "Reserved"],
              ["",  "K",      "K",      "F",       "F",       "Reserved"],
              ["",  "A",      "A",      "A",       "A",       "Reserved"],
              ["",  "cd",     "cd",     "cd",      "cd",      "Reserved"],
              ["",  "",       "",       "",        "",        "Reserved"]]

inputBits :: [[String]]
inputBits = [ ["Data (0)",    "Constant (1)"],
              ["Array (0)",   "Variable (1)"],
              ["Absolute (0)","Relative (1)"],
              ["No Wrap (0)", "Wrap (1)"],
              ["Linear (0)",  "Non Linear (1)"],
              ["Preferred State (0)", "No Preferred (1)"],
              ["No Null position (0)","Null state(1)"],
              ["Reserved (0)"],
              ["Bit Field (0)"], ["Buffered Bytes (1)"],
              ["Reserved (0)"]] :: [[String]]

inputBitsS :: [[String]]
inputBitsS = [ ["Data",    "Const"],
              ["Arr",   "Var"],
              ["Abs","Rel"],
              ["NoWrap", "Wrap"],
              ["Lin",  "NLin"],
              ["Prf", "NPrf"],
              ["NotNull","Null"],
              ["Reserv"],
              ["BitField"], ["Buff Bytes"],
              ["Reserv"]] :: [[String]]

outputBitsS :: [[String]]
outputBitsS =  [["Data", "Const"],
              ["Arr", "Var"],
              ["Abs", "Rel"],
              ["NoWrap", "Wrap"],
              ["Lin", "NLin"],
              ["Prf", "NPref"],
              ["NoNull", "Null"],
              ["NonVolatile", "Volatile"],
              ["BitField", "Buff Bytes"],
              ["Reserv"]]:: [[String]]

outputBits :: [[String]]
outputBits =  [["Data (0)", "Constant (1)"],
              ["Array (0)", "Variable (1)"],
              ["Absolute (0)", "Relative (1)"],
              ["No Wrap (0)", "Wrap (1)"],
              ["Linear (0)", "Non Linear (1)"],
              ["Preferred State (0)", "No Preferred (1)"],
              ["No Null position (0)", "Null state(1)"],
              ["Non Volatile (0)", "Volatile (1)"],
              ["Bit Field (0)", "Buffered Bytes (1)"],
              ["Reserved (0)"]]:: [[String]]

featureBits :: [[String]]
featureBits = [["Data (0)", "Constant (1)"],
              ["Array (0)", "Variable (1)"],
              ["Absolute (0)", "Relative (1)"],
              ["No Wrap (0)", "Wrap (1)"],
              ["Linear (0)", "Non Linear (1)"],
              ["Preferred State (0)", "No Preferred (1)"],
              ["No Null position (0)", "Null state(1)"],
              ["Non Volatile (0)", "Volatile (1)"],
              ["Bit Field (0)", "Buffered Bytes (1)"],
              ["Reserved (0)"]]:: [[String]]

featureBitsS :: [[String]]
featureBitsS = [["Data", "Const"],
              ["Arr", "Var"],
              ["Abs", "Rel"],
              ["NoWrap", "Wrap"],
              ["Lin", "NLin"],
              ["Prf", "NPrf"],
              ["No Null position (0)", "Null"],
              ["NonVolatile", "Volatile"],
              ["BitField", "Buff Bytes"],
              ["Reserv"]]:: [[String]]
