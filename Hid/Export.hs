
module Hid.Export
  ( toCArray
  , toCArrayC
  , toCArrayDirty
  , toShort
  , datToBin
  , toDat
  , toSmartSimple
  , toHid
  )
  where

import Hid.HTypes
--import Hid.HTools
import Hid.HParse
import Hid.HToName


datToBin :: String -> String
datToBin = (map (\x -> ( toEnum (read ("0x"++x) :: Int) )::Char) ) . words

toDat :: [SItem] -> String
toDat = rbin2texthex . hsinter

spaces :: String
spaces = repeat ' '

sx :: Int -> String
sx i = take i spaces

pad :: Int -> String -> String
pad i a = take ct spaces
      where
       ct = max (i - (length a)) 2

pshow :: [(String, (MType, [String]), HState)] -> Int -> [Char]
pshow [] _ = "\n"
pshow [a] i= sone1 a i
pshow (a:as) i= (sone a i) ++"\n"++pshow as i

pshowC :: [(String, (MType, [String]), HState)] -> Int -> [Char]
pshowC [] _ = "" -- 
pshowC [a] i= "\t" ++ sone1C a i
pshowC (a:as) i = "\t" ++  (soneC a i) ++"\n" ++ pshowC as i


sone :: (String, (MType, [String]), HState) -> Int -> String
sone (te,(a,des),s) i = (sx l) ++ c ++(pad (i-l) c)++"//  "++ (show a) ++" "++ (putDes des i)
       where
          c =  coma(te++" ")
          l = length $ sStack s

soneC :: (String, (MType, [String]), HState) -> Int -> String
soneC (te,(a,des),s) i = (sx l ) ++ c ++(pad (i-l) c)++"/*  "++ (show a) ++ " " ++(putDesC des i)
       where
          c =  coma(te++" ")
          l = (length $ sStack s) 

putDes :: [String] -> Int -> String
putDes []  _ = ""
putDes (a:as) i = a ++ (desNext as i)
 where
  desNext [] _ = ""
  desNext (b:bs) ii = "\n"++(sx ii) ++ "//  " ++ b ++ desNext bs ii

putDesC :: [String] -> Int -> String
putDesC []  _ = " */"   -- only if description is empty to start with
putDesC (a:as) i = a ++ " " ++ (desNext as i) ++ " */"
 where
  desNext [] _ = ""
--  desNext [b] ii = " " ++ b ++ " )"
  desNext (b:bs) ii = "("++ b  ++ desNext bs ii ++ ")"


sone1 :: (String, (MType, [String]), HState) -> Int -> String
sone1 (te,(a,des),s) i = (sx l) ++c++(pad (i-l) c)++"//  "++ (show a) ++" "++ (putDes des i)
       where
          c = (init $ coma(te++" "))
          l = length $ sStack s

sone1C :: (String, (MType, [String]), HState) -> Int -> String
sone1C (te,(a,des),s) i = (sx l) ++c++(pad (i-l) c)++"/*  "++ (show a) ++" "++ (putDesC des i)
       where
          c = (init $ coma(te++" "))
          l = (length $ sStack s)


coma :: String -> String
coma (a:b:' ':[]) = "0x" ++ [a,b]
coma (a:b:' ':as) = "0x"++[a,b]++", " ++ coma as
coma [] = []

wrap :: String -> String -> String
wrap n a = n ++ " = {\n" ++ a ++"\n}\n"

toCArrayDirty :: [SItem] -> String
toCArrayDirty y = wrap "rdesc" $ pshow parsed 20
         where
           parsed = map (\x-> ((toStr . rbin2texthex . hsinter1 . fst) x, srepa x, snd x))$ walk emptyState y

--plainAsCArray a y = wrap a $ pshow parsed i
toCArray :: String -> [SItem] -> String
toCArray a y = wrap a $ pshow parsed i
         where
           parsed = map (\x-> ((toStr . rbin2texthex . hsinter1 . fst) x, srepa x, snd x))$ walk emptyState y
           i = 2 + maximum (map (\(te,_,s)-> (length $ sStack s) + (length $ coma (te++" "))) parsed)

toCArrayC :: String -> [SItem] -> String
toCArrayC a y = wrap a $ pshowC parsed i
         where
           parsed = map (\x-> ((toStr . rbin2texthex . hsinter1 . fst) x, srepa x, snd x))$ walk emptyState y
           i = 2 + maximum (map (\(te,_,s)-> (length $ sStack s) + (length $ coma (te++" "))) parsed)


smartShow ::  Int -> (String, (MType, RBitString, HState, [String])) -> String
smartShow i (_,(a,b,s,des))   = (sx l) ++c++((pad (i-l)) c)++" // "++ (concatMap (\x->" "++x) des) ++"\n"
        where
          l = 3 * (length $ sStack s)
          c = (smartRAW a b)

smartSimpleShow :: Int -> (String, (MType, RBitString, HState, [String])) -> String
smartSimpleShow _ (_,(a,b,s,_))   = (sx l) ++c++"\n"
        where
          l = 3 * (length $ sStack s)
          c = (smartRAW a b)

smartRAW :: MType -> RBitString -> String
smartRAW MCollection b = case (rb2uint2int b) of 
                  0 -> "P {"
                  1 -> "A {"
                  2 -> "L {"
                  3 -> "R {"
                  4 -> "NA {"
                  5 -> "UM {"
                  6 -> "US {"
                  _ -> "{ " ++ (rbin2textrhex b)
smartRAW LDelimiter b = case (rb2uint2int b) of 
                  0 -> "] "
                  _ -> "[ "

smartRAW a b = (kwTag t) ++ pad' ++ (kwShow t $ b)
          where
             t = findType keyWords a
             lt = length $ (kwTag t)
             ls = length $ (kwShow t $ b)
             pad' = pad ( max (10 -lt -ls) 1) ""
toShort :: [SItem] -> String
toShort y = concatMap (smartShow i) parsed
   where 
     parsed = map (\x-> ((show . fst) x, srepa2 x))$ walk emptyState y
     i = 2 + (maximum $ map (\(_,(a,b,s,_))-> 3*(length $ sStack s) + (length $ smartRAW a b)) parsed)

toSmartSimple :: [SItem] -> String
toSmartSimple y = concatMap (smartSimpleShow i) parsed
   where 
     parsed = map (\x-> ((show . fst) x, srepa2 x))$ walk emptyState y
     i = 2 + (maximum $ map (\(_,(a,b,s,_))-> 3*(length $ sStack s) + (length $ smartRAW a b)) parsed)


hidShow :: (String, (MType, RBitString, HState, [String])) -> String
hidShow (te, (a,b,s,_)) = let
                            ind i = take 15 ( (rbin2texthex $ rpadu $ leuint2rb $  mkUInt (i+(length $ sStack s)) 16) ++ zeroPad)
                            usa = (case length (toStr b) of
                                 32 -> take 16 (te ++ zeroPad)
                                 _  -> (take 9 (te ++ zeroPad)) ++ (take 7 $ ( rbin2texthex $ sUsagePage $ sCurrent s) ++ zeroPad )
                                 ) ++ (ind 0 )
                       in
                       case a of
                        LUsage -> usa
                        LUsageMinimum -> usa
                        LUsageMaximum -> usa
                        MEndCollection -> take 16  (te ++ zeroPad) 
                               ++ (ind (-1))
                        GUnit -> case length (toStr b)of
                             8 ->  take 31 (te ++ zeroPad)
                             _ -> (take 16 (te ++ zeroPad) ) ++ (ind 0)
                        _ -> take 16  (te ++ zeroPad)
                               ++ (ind 0)

zeroPad ::String
zeroPad = " 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 "

toHid :: [SItem] -> String
toHid y = "22 00 01 00 FE CA "
       ++ l
       ++ " 0A"++ zeroPad
       ++ concatMap hidShow parsed
   where
       l = rbin2texthex $ rpadu $ leuint2rb $  mkUInt (length y) 16
       parsed = map (\x-> ((toStr . rbin2texthex . hsinter1 . fst) x, srepa2 x))$ walk emptyState y

