{-# LANGUAGE GeneralizedNewtypeDeriving
            ,MultiParamTypeClasses
            ,OverloadedStrings
            ,UndecidableInstances
            ,TypeSynonymInstances
            ,FlexibleInstances #-}

module Hid.HTypes where
import Data.String( IsString(..) )
import Data.List
import Data.Char

hex :: [Char]
hex = "0123456789ABCDEF"

-- Text Strings
uc, hd :: String -> String
uc =  map toUpper
hd = filter (`elem` hex)

hexdigit :: Int -> String
hexdigit i | i < 10 = show i
hexdigit i = [(toEnum (i + 55) :: Char)]

writebase :: Int -> Int -> String
writebase b h | h < b = hexdigit h
writebase b h = (writebase b (h `div` b)) ++ ( hexdigit (h `mod` b))

showihex :: Int -> [Char]
showihex h = padZeros z s
          where 
            s = writebase 16 h
            z = 2 * ((1 + length s ) `div` 2)

padZeros :: Int -> [Char] -> [Char]
padZeros i s = (take (i - l) (repeat '0')) ++ s
              where 
                l = length s

tabhb :: [(Char, [Char])]
tabhb = [ ('0', "0000")
        , ('1', "0001")
        , ('2', "0010")
        , ('3', "0011")
        , ('4', "0100")
        , ('5', "0101")
        , ('6', "0110")
        , ('7', "0111")
        , ('8', "1000")
        , ('9', "1001")
        , ('A', "1010")
        , ('B', "1011")
        , ('C', "1100")
        , ('D', "1101")
        , ('E', "1110")
        , ('F', "1111")] :: [(Char,[Char])]


h2b :: Char -> String
h2b c =  snd $ head $ (filter (\x -> c == fst x) tabhb)  ++ [('X', "XXXX")]

h2rb :: Char -> String
h2rb a = reverse (h2b a) 

--i2rb i = 

b2h :: String -> Char
b2h s = fst $ head $  (filter (\x -> s == snd x) tabhb) ++ [('X', "XXXX")]

rb2h :: String -> Char
rb2h s = fst $ head  $ (filter (\x -> (reverse s) == snd x) tabhb) ++ [('X', "XXXX")]  

data HGlobal = HG {
                  sUsagePage :: RBitString, --1
                  sLogicalMinimum :: RBitString, --2
                  sLogicalMaximum :: RBitString, --3
                  sPhysicalMinimum :: RBitString, --4
                  sPhysicalMaximum :: RBitString, --5
                  sUnitExponent :: RBitString, --6
                  sUnit :: RBitString, --7
                  sReportSize :: RBitString, --8
                  sReportID :: RBitString,  --9
                  sReportCount :: RBitString, --10
                  sCollectionPage :: RBitString, --11
                  sCollectionUsage :: RBitString --11
                  } deriving (Show, Eq)

emptyGlobalState :: HGlobal
emptyGlobalState = HG elbs elbs elbs elbs elbs elbs elbs elbs elbs elbs elbs elbs

data HLocal = HL {
                 sLocalUsagePage :: RBitString,
                 sUsage :: [RBitString], --1
                 sUsageMinimum :: RBitString, 
                 sUsageMaximum :: RBitString,
                 sDesignatorIndex :: RBitString,
                 sDesignatorMinimum :: RBitString,
                 sDesignatorMaximum :: RBitString,
                 sStringIndex :: RBitString, --7
                 sStringMinimum :: RBitString,
                 sStringMaximum :: RBitString,
                 sDelimier :: RBitString --10
                 } deriving (Show, Eq)

emptyLocalState :: HLocal
emptyLocalState = HL elbs [] elbs elbs elbs elbs elbs elbs elbs elbs elbs


data HState = HState {
                     sCurrent :: HGlobal,
                     sPushPop :: [HGlobal],
                     sStack :: [HGlobal],
                     sLocal :: HLocal,
                     sOffsetI :: [Offset],
                     sOffsetO :: [Offset],
                     sOffsetF :: [Offset]
                    } deriving (Show, Eq)

emptyState :: HState
emptyState = HState emptyGlobalState [] [] emptyLocalState [] [] []

data UInt = UInt Int Int deriving (Eq , Show)

mkUInt :: Int -> Int -> UInt
mkUInt a l
     | a < 0 = error "UInt cann't be negative"
     | l < 0 = error "Negative length!"
     | l == 0 && a /= 0 = error "You lose information losing 'a'."
     | otherwise = UInt a l

instance Enum UInt where
  fromEnum (UInt n _) = n
  toEnum n = mkUInt n 32

data SInt = SInt Int Int deriving (Eq, Show)

mkSInt :: Int -> Int -> SInt
mkSInt a l
     | l < 0 = error "Negative length!"
     | l == 0 && a /= 0 = error "You lose information losing 'a'."
     | otherwise = SInt a l

instance Enum SInt where
  fromEnum (SInt n _) = n
  toEnum n = mkSInt n 32


elbs :: RBitString
elbs = RBS ""

elb0 :: RBitString
elb0 = RBS ""

newtype Offset = OF (Maybe Int, Maybe Int) deriving (Show, Eq)

nullOffset :: Offset
nullOffset = OF (Nothing, Nothing)

data Item = IT(RBitString, RBitString, RBitString) deriving (Show, Eq, Ord)
data SItem = SI(MType, RBitString) deriving (Show, Eq, Ord)

type Parsed = [SItem]

class Conv0 a where
  toStr :: a -> String
  fromStr :: String -> a
  (+++):: a -> a -> a
  a +++ b = fromStr $ (toStr a) ++ (toStr b)


instance Conv0 [Char] where
  toStr = id
  fromStr = id

class Conv a b where
  conv :: (String->String) -> a -> b

instance (Conv0 a, Conv0 b) => Conv a b where
   conv f = fromStr . f . toStr

--instance Conv0 a => IsString a where
--  fromString = fromStr
instance IsString BitString where
  fromString = fromStr

instance IsString RBitString where
  fromString = fromStr

instance IsString HexString where
  fromString = fromStr

instance IsString RHexString where
  fromString = fromStr


newtype BitString = BS String deriving (Show, Eq, Ord, Read)
instance Conv0 BitString where
  toStr (BS a) = a
  fromStr a = BS a

newtype RBitString = RBS String deriving (Show, Eq, Ord, Read)
instance Conv0 RBitString where
  toStr (RBS a) = a
  fromStr a = RBS a

newtype HexString = HS String deriving (Show, Eq, Ord, Read)
instance Conv0 HexString where
  toStr (HS a) = a
  fromStr a = HS a

newtype RHexString = RHS String deriving (Show, Eq, Ord, Read)
instance Conv0 RHexString where
  toStr (RHS a) = reverse a
  fromStr a = RHS $ reverse a



data MType = MPadding   -- 0 0000
           | MReserved1   -- 1
           | MReserved2   -- 2
           | MReserved3   -- 3
           | MReserved4   -- 4
           | MReserved5   -- 5
           | MReserved6   -- 6
           | MReserved7   -- 7
           | MInput       -- 8 0001
           | MOutput      -- 9 1001
           | MCollection  -- 10 0101
           | MFeature     -- 11 1101
           | MEndCollection -- 12 0011
           | MReservedD   -- 13
           | MReservedE   -- 14
           | MReservedF   -- 15
           | GUsagePage -- 0 0000
           | GLogicalMinimum -- 1 1000
           | GLogicalMaximum -- 2 0100
           | GPhysicalMinimum -- 3 1100
           | GPhysicalMaximum -- 4 0010
           | GUnitExponent -- 5 1010
           | GUnit -- 6 0110
           | GReportSize -- 7 1110
           | GReportID -- 8 0001
           | GReportCount -- 9 1001
           | GPush -- 10 0101
           | GPop -- 11 1101
           | GReservedC   -- 12
           | GReservedD   -- 13
           | GReservedE   -- 14
           | GReservedF   -- 15
           | LUsage -- 0 0000
           | LUsageMinimum -- 1 1000
           | LUsageMaximum -- 2 0100
           | LDesignatorIndex -- 3 1100
           | LDesignatorMinimum -- 4 0010
           | LDesignatorMaximum -- 5 1010
           | LReserved6 -- 6
           | LStringIndex --  7 1110
           | LStringMinimum -- 8 0001
           | LStringMaximum -- 9 1001
           | LDelimiter -- 10 0101
           | LReservedB -- 11
           | LReservedC -- 12
           | LReservedD -- 13
           | LReservedE -- 14
           | LReservedF -- 15
           | RReserved0 -- 0
           | RReserved1 -- 1
           | RReserved2 -- 2
           | RReserved3 -- 3
           | RReserved4 -- 4
           | RReserved5 -- 5
           | RReserved6 -- 6
           | RReserved7 -- 7
           | RReserved8 -- 8
           | RReserved9 -- 9
           | RReservedA -- 10
           | RReservedB -- 11
           | RReservedC -- 12
           | RReservedD -- 13
           | RReservedE -- 14
           | RReservedF -- 15
           | TReserved00   -- 64
           | TReserved01   -- 65
           | TReserved02   -- 66
           | TReserved03   -- 67
           | TReserved04   -- 68
           | TReserved05   -- 69
           | TReserved06   -- 70
           | TReserved07   -- 71
           | TReserved08   -- 72
           | TReserved09   -- 73
           | TReserved0A   -- 74
           | TReserved0B   -- 75
           | TReserved0C   -- 76
           | TReserved0D   -- 77
           | TReserved0E   -- 78
           | TReserved0F   -- 79
           | TReserved10   -- 80
           | TReserved11   -- 81
           | TReserved12   -- 82
           | TReserved13   -- 83
           | TReserved14   -- 84
           | TReserved15   -- 85
           | TReserved16   -- 86
           | TReserved17   -- 87
           | TReserved18   -- 88
           | TReserved19   -- 89
           | TReserved1A   -- 90
           | TReserved1B   -- 91
           | TReserved1C   -- 92
           | TReserved1D   -- 93
           | TReserved1E   -- 94
           | TReserved1F   -- 95
           | TReserved20   -- 96
           | TReserved21   -- 97
           | TReserved22   -- 98
           | TReserved23   -- 99
           | TReserved24   -- 100
           | TReserved25   -- 101
           | TReserved26   -- 102
           | TReserved27   -- 103
           | TReserved28   -- 104
           | TReserved29   -- 105
           | TReserved2A   -- 106
           | TReserved2B   -- 107
           | TReserved2C   -- 108
           | TReserved2D   -- 109
           | TReserved2E   -- 110
           | TReserved2F   -- 111
           | TReserved30   -- 112
           | TReserved31   -- 113
           | TReserved32   -- 114
           | TReserved33   -- 115
           | TReserved34   -- 116
           | TReserved35   -- 117
           | TReserved36   -- 118
           | TReserved37   -- 119
           | TReserved38   -- 120
           | TReserved39   -- 121
           | TReserved3A   -- 122
           | TReserved3B   -- 123
           | TReserved3C   -- 124
           | TReserved3D   -- 125
           | TReserved3E   -- 126
           | TReserved3F   -- 127
           | TReserved40   -- 128
           | TReserved41   -- 129
           | TReserved42   -- 130
           | TReserved43   -- 131
           | TReserved44   -- 132
           | TReserved45   -- 133
           | TReserved46   -- 134
           | TReserved47   -- 135
           | TReserved48   -- 136
           | TReserved49   -- 137
           | TReserved4A   -- 138
           | TReserved4B   -- 139
           | TReserved4C   -- 140
           | TReserved4D   -- 141
           | TReserved4E   -- 142
           | TReserved4F   -- 143
           | TReserved50   -- 144
           | TReserved51   -- 145
           | TReserved52   -- 146
           | TReserved53   -- 147
           | TReserved54   -- 148
           | TReserved55   -- 149
           | TReserved56   -- 150
           | TReserved57   -- 151
           | TReserved58   -- 152
           | TReserved59   -- 153
           | TReserved5A   -- 154
           | TReserved5B   -- 155
           | TReserved5C   -- 156
           | TReserved5D   -- 157
           | TReserved5E   -- 158
           | TReserved5F   -- 159
           | TReserved60   -- 160
           | TReserved61   -- 161
           | TReserved62   -- 162
           | TReserved63   -- 163
           | TReserved64   -- 164
           | TReserved65   -- 165
           | TReserved66   -- 166
           | TReserved67   -- 167
           | TReserved68   -- 168
           | TReserved69   -- 169
           | TReserved6A   -- 170
           | TReserved6B   -- 171
           | TReserved6C   -- 172
           | TReserved6D   -- 173
           | TReserved6E   -- 174
           | TReserved6F   -- 175
           | TReserved70   -- 176
           | TReserved71   -- 177
           | TReserved72   -- 178
           | TReserved73   -- 179
           | TReserved74   -- 180
           | TReserved75   -- 181
           | TReserved76   -- 182
           | TReserved77   -- 183
           | TReserved78   -- 184
           | TReserved79   -- 185
           | TReserved7A   -- 186
           | TReserved7B   -- 187
           | TReserved7C   -- 188
           | TReserved7D   -- 189
           | TReserved7E   -- 190
           | TReserved7F   -- 191
           | TReserved80   -- 192
           | TReserved81   -- 193
           | TReserved82   -- 194
           | TReserved83   -- 195
           | TReserved84   -- 196
           | TReserved85   -- 197
           | TReserved86   -- 198
           | TReserved87   -- 199
           | TReserved88   -- 200
           | TReserved89   -- 201
           | TReserved8A   -- 202
           | TReserved8B   -- 203
           | TReserved8C   -- 204
           | TReserved8D   -- 205
           | TReserved8E   -- 206
           | TReserved8F   -- 207
           | TReserved90   -- 208
           | TReserved91   -- 209
           | TReserved92   -- 210
           | TReserved93   -- 211
           | TReserved94   -- 212
           | TReserved95   -- 213
           | TReserved96   -- 214
           | TReserved97   -- 215
           | TReserved98   -- 216
           | TReserved99   -- 217
           | TReserved9A   -- 218
           | TReserved9B   -- 219
           | TReserved9C   -- 220
           | TReserved9D   -- 221
           | TReserved9E   -- 222
           | TReserved9F   -- 223
           | TReservedA0   -- 224
           | TReservedA1   -- 225
           | TReservedA2   -- 226
           | TReservedA3   -- 227
           | TReservedA4   -- 228
           | TReservedA5   -- 229
           | TReservedA6   -- 230
           | TReservedA7   -- 231
           | TReservedA8   -- 232
           | TReservedA9   -- 233
           | TReservedAA   -- 234
           | TReservedAB   -- 235
           | TReservedAC   -- 236
           | TReservedAD   -- 237
           | TReservedAE   -- 238
           | TReservedAF   -- 239
           | TReservedB0   -- 240
           | TReservedB1   -- 241
           | TReservedB2   -- 242
           | TReservedB3   -- 243
           | TReservedB4   -- 244
           | TReservedB5   -- 245
           | TReservedB6   -- 246
           | TReservedB7   -- 247
           | TReservedB8   -- 248
           | TReservedB9   -- 249
           | TReservedBA   -- 250
           | TReservedBB   -- 251
           | TReservedBC   -- 252
           | TReservedBD   -- 253
           | TReservedBE   -- 254
           | TReservedBF   -- 255
           | TReservedC0   -- 256
           | TReservedC1   -- 257
           | TReservedC2   -- 258
           | TReservedC3   -- 259
           | TReservedC4   -- 260
           | TReservedC5   -- 261
           | TReservedC6   -- 262
           | TReservedC7   -- 263
           | TReservedC8   -- 264
           | TReservedC9   -- 265
           | TReservedCA   -- 266
           | TReservedCB   -- 267
           | TReservedCC   -- 268
           | TReservedCD   -- 269
           | TReservedCE   -- 270
           | TReservedCF   -- 271
           | TReservedD0   -- 272
           | TReservedD1   -- 273
           | TReservedD2   -- 274
           | TReservedD3   -- 275
           | TReservedD4   -- 276
           | TReservedD5   -- 277
           | TReservedD6   -- 278
           | TReservedD7   -- 279
           | TReservedD8   -- 280
           | TReservedD9   -- 281
           | TReservedDA   -- 282
           | TReservedDB   -- 283
           | TReservedDC   -- 284
           | TReservedDD   -- 285
           | TReservedDE   -- 286
           | TReservedDF   -- 287
           | TReservedE0   -- 288
           | TReservedE1   -- 289
           | TReservedE2   -- 290
           | TReservedE3   -- 291
           | TReservedE4   -- 292
           | TReservedE5   -- 293
           | TReservedE6   -- 294
           | TReservedE7   -- 295
           | TReservedE8   -- 296
           | TReservedE9   -- 297
           | TReservedEA   -- 298
           | TReservedEB   -- 299
           | TReservedEC   -- 300
           | TReservedED   -- 301
           | TReservedEE   -- 302
           | TReservedEF   -- 303
           | TReservedF0   -- 304
           | TReservedF1   -- 305
           | TReservedF2   -- 306
           | TReservedF3   -- 307
           | TReservedF4   -- 308
           | TReservedF5   -- 309
           | TReservedF6   -- 310
           | TReservedF7   -- 311
           | TReservedF8   -- 312
           | TReservedF9   -- 313
           | TReservedFA   -- 314
           | TReservedFB   -- 315
           | TReservedFC   -- 316
           | TReservedFD   -- 317
           | TReservedFE   -- 318
           | TReservedFF   -- 319
              deriving (Show, Read, Eq, Ord, Enum)

data KWords = KW {
                 kwTag :: String,
                 kwType :: MType,
                 kwArgs :: Int,
                 kwConv :: ([String]->RBitString),
                 kwShow :: (RBitString -> String)}

keyWords :: [KWords]
keyWords = [KW  "I"    MInput           1 smartu   rbin2textrhex,
            KW  "O"    MOutput          1 smartu   rbin2textrhex,
            KW  "F"    MFeature         1 smartu   rbin2textrhex,
            --KW  "{"    MCollection      1 smartu   rbin2textrhex,
            KW  "P"    MCollection       1 (smartc "0x00")   rbin2textrhex,
            KW  "A"    MCollection       1 (smartc "0x01")   rbin2textrhex,
            KW  "L"    MCollection       1 (smartc "0x02")   rbin2textrhex,
            KW  "R"    MCollection       1 (smartc "0x03")   rbin2textrhex,
            KW  "NA"   MCollection       1 (smartc "0x04")   rbin2textrhex,
            KW  "US"   MCollection       1 (smartc "0x05")   rbin2textrhex,
            KW  "UM"   MCollection       1 (smartc "0x06")   rbin2textrhex,
            KW  "}"    MEndCollection   0 (smartz "") nop,
            KW  "UP"   GUsagePage       1 smartu   rbin2textrhex,
            KW  "Lmin" GLogicalMinimum  1 smarts   rbin2textsint,
            KW  "Lmax" GLogicalMaximum  1 smarts   rbin2textsint,
            KW  "Pmin" GPhysicalMinimum 1 smarts   rbin2textsint,
            KW  "Pmax" GPhysicalMaximum 1 smarts   rbin2textsint,
            KW  "Unit" GUnit            1 smartu   rbin2textrhex,
            KW  "Exp"  GUnitExponent    1 smarts   rbin2textsint,
            KW  "Push" GPush            0 (smartz "") nop,
            KW  "Pop"  GPop             0 (smartz "") nop,
            KW  "ID"   GReportID        1 smartu   rbin2textrhex,
            KW  "RS"   GReportSize      1 smartu   rbin2textrhex,
            KW  "RC"   GReportCount     1 smartu   rbin2textrhex,
            KW  "pad"  MPadding         0 smartu   rbin2textrhex,
            KW  "U"    LUsage           1 smartu   rbin2textrhex,
            KW  "Umin" LUsageMinimum    1 smartu   rbin2textrhex,
            KW  "Umax" LUsageMaximum    1 smartu   rbin2textrhex,
            KW  "D"    LDesignatorIndex 1 smartu   rbin2textrhex,
            KW  "Dmin" LDesignatorMinimum 1 smartu rbin2textrhex,
            KW  "Dmax" LDesignatorMaximum 1 smartu rbin2textrhex,
            KW  "S"    LStringIndex     1 smartu   rbin2textrhex,
            KW  "Smin" LStringMinimum   1 smartu   rbin2textrhex,
            KW  "Smax" LStringMaximum   1 smartu   rbin2textrhex,
            KW  "["    LDelimiter       0 (smartz "1") rbin2textrhex,
            KW  "]"    LDelimiter       0 (smartz "0") rbin2textrhex] -- can change to "" after patching kernel

smartWords :: String -> [(String,[String])]
smartWords "" = []
smartWords a = process $ words $ fixBrackets a
         where
           process [] = []
           process (b:bs)
                | isSmartWord b = (uc b,collect bs):process (drop (length $ collect bs) bs)
                | otherwise = error ("Not a smartword: " ++ b)
           collect [] =[]
           collect (b:bs) 
                | isSmartWord b = []
                | otherwise = b: collect bs

fixBrackets :: String -> String
fixBrackets "" = ""
fixBrackets (a:as) 
         | a `elem` "{}[]-" = ' ':a:fixBrackets as
         | a `elem` ",;.:" = ' ':a:' ':fixBrackets as
         | otherwise = a:fixBrackets as

isSmartWord :: String -> Bool
isSmartWord a = (uc a) `elem` sortedWords 

sortedWords :: [String]
sortedWords = map uc $ reverse $ sortBy (\a b -> (length a) `compare` (length b)) $ map kwTag keyWords 

nop :: RBitString -> String
nop = \_-> ""

findKey :: [KWords] -> String -> KWords
findKey [] s = error ("Not not defined key:" ++ s)
findKey (a:as) s
         | uc (kwTag a) == uc s = a
         | otherwise = findKey as s

findType :: [KWords] -> MType -> KWords
findType [] t = KW ("RAW 0x" ++(showihex $ fromEnum t)) t 0 (\_->RBS "") rbin2texthex
findType (a:as) t
         | kwType a == t = a
         | otherwise = findType as t

rpadu :: RBitString -> RBitString
rpadu r@(RBS s) 
     | l==0 || l==8 || l == 16 || l ==32 = r
     | l < 8 = RBS (s++(z 8))
     | l < 16 = RBS (s++(z 16))
     | l < 32 = RBS (s++(z 32))
       where 
         l = length s
         z i = take (i-l) (repeat '0')

rpads :: RBitString -> RBitString
rpads r@(RBS s) 
     | l == 0 || l == 8 || l == 16 = r 
     | l < 8 = RBS (s++(z 8))
     | l < 16 = RBS (s++(z 16))
     | l < 32 = RBS (s++(z 32))
       where 
         l = length s
         msd = last s
         z i = take (i-l) (repeat (msd))

smartu :: [String] -> RBitString
smartu [] = RBS ""
smartu [s] = rpadu res
       where
        v = sma s
        sma z
           | (length z)  == 1 =  (read ("0x"++z)) :: Int
           | otherwise =  (read z) :: Int
        ui =  mkUInt v (guesssize v)
        res = leuint2rb ui
        guesssize w
             | w == 0 = 0
             | w <= 0xFF = 8
             | w <= 0xFFFF = 16
             | w <= 0xFFFFFFFF = 32
             | otherwise = error ("too long data")

smarts :: [String] -> RBitString
smarts [] = RBS ""
smarts [s] = rpads res
       where 
          v = (read s) :: Int
          si = mkSInt v (guesssize v)
          res = lesint2rb si
          guesssize w
               | w > 32767 = 32
               | w > 127 = 16
               | w > 0 = 8 
               | w == 0 = 0
               | w >= - 128 = 8
               | w >= - 32768 = 16
               | otherwise = 32 

smartz :: String -> [String] -> RBitString
smartz x = \_ -> (rpadu $ RBS x)

smartc ::  String -> [String] -> RBitString
smartc v = \_ -> smarts [v]

twoswap :: [a] -> [a]
twoswap [] = []
twoswap [a] = [a]
twoswap (a:b:cs) = b:a:twoswap cs

fourswap :: [a] -> [a]
fourswap [] = []
fourswap (a:b:[]) = b:a:[]
fourswap (a:b:c:d:cs) = d:c:b:a:twoswap cs

twospace :: String -> String
twospace "" = ""
twospace (a:b:c:cs) = a:b:' ':twospace (c:cs)
twospace [a,b] = a:b:[]

four :: String -> [String]
four = splitEvery 4

twos :: String -> [String]
twos = splitEvery 2

splitEvery :: Int -> String -> [String]
splitEvery _ [] = []
splitEvery m s = c:splitEvery m cs
      where
       (c,cs) = splitAt m s
----------------------------------------------------

leuint2rb ::UInt -> RBitString 
leuint2rb (UInt v b) =  vv +++ (RBS z)
       where 
         vv = con v
         con w
           | w == 0 = RBS ""
           | otherwise = (hex2rbin .texthex2hex . twoswap . reverse . showihex) w
         l = length $ toStr vv
         z = take (b-l) (repeat '0')

lesint2rb ::SInt -> RBitString 
lesint2rb (SInt v b)
         | v >= 0 = (leuint2rb $ mkUInt v b)
         | otherwise = conv twoNegate (leuint2rb $ mkUInt (-v) b) 
--         | otherwise = spec (-v)
--           where
--              spec v  
--                  | v < 127 = (conv twoNegate (leuint2rb $ mkUInt v (b)) ) +++ (RBS "1")
--                  | otherwise = (conv twoNegate (leuint2rb $ mkUInt v (b)) ) +++ (RBS "11111111")
---------------OK-----------------
uint2rb ::UInt -> RBitString 
uint2rb (UInt v b) =  vv +++ (RBS z)
       where 
         vv = (hex2rbin . texthex2hex . showihex) v
         l = length $ toStr vv
         z = take (b-l) (repeat '0')

---------------OK-----------------
texthex2hex :: String -> HexString
texthex2hex = conv (hd . uc)
---------------OK-----------------
hex2rbin ::HexString -> RBitString
hex2rbin = fromStr . (concatMap h2rb) . twoswap . toStr

twoNegate :: String -> String
twoNegate "" = ""
twoNegate (a:as) = case a of
                    '0' -> '0': twoNegate as 
                    '1' -> '1': flipz as
                       where
                          flipz s = map (\x -> if x=='1' then '0' else '1') s

-- BITS UNNSAFE!

sbit :: Char -> Int
sbit '0' = 0
sbit '1' = 1
sbit a = error ("No such bit [" ++[a] ++"]" )

bval :: String -> Int
bval (a:as) = (sbit a) +  2 * (bval as)
bval [] = 0

sval :: String -> Int
sval [] = 0
sval a
    | last a == '1' = - (2 ^ l) + (bval a)
    | otherwise = bval a
        where
          l =  length a

rb2uint :: RBitString -> UInt
rb2uint a = mkUInt (bval sa ) (length sa)
       where
         sa = toStr a

rb2uint2int :: RBitString -> Int
rb2uint2int a = fromEnum $ rb2uint a

rb2sint2int :: RBitString -> Int
rb2sint2int a = fromEnum $ rb2sint a

rb2sint :: RBitString -> SInt
rb2sint a = mkSInt (sval sa) (length sa)
       where
         sa = toStr a

-----------------OK----------------
rbin2texthex :: RBitString -> String
rbin2texthex a = twospace $ twoswap $ map rb2h $four $toStr a

-----------------OK----------------
rbin2textrhex :: RBitString -> String
rbin2textrhex (RBS "") = ""
rbin2textrhex a = "0x" ++ (concat $ reverse $ twos $ twoswap $ map rb2h $ four $toStr a)

rbin2textsint :: RBitString -> String
rbin2textsint = show . rb2sint2int 

rbin2textuint :: RBitString -> String
rbin2textuint = show . rb2uint2int
