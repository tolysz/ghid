
module Hid.Import
  ( fromCArray1
  , fromHid
  , fromShortHand
  , fromDat
  )
  where

import Hid.HTypes
import Hid.HTools
import Hid.HParse
--import Hid.HToName

addSpace :: String -> String
addSpace (a1:a2:as) = a1:a2:' ':addSpace as
addSpace a = a

fromHid :: String -> String
fromHid = addSpace . concatMap( hLen ) . (splitEvery 20) . (drop 68)

hLen :: String -> String
hLen s = take (2 + 2 * (shortLen r)) s
            where
                r = toStr $ texthex2rbin  s

fromDat :: String -> [SItem]
fromDat = hsparse . hex2rbin .texthex2hex  . head . lines

fromCArray1 :: String -> [SItem]
fromCArray1  = hsparse . texthex2rbin .(concatMap (\x->x++" ")) .(map showihex).(map (\x -> (read x) :: Int)) . words .(kill ','). grabData . removeCBlockComents . removeCLineComments


fromShortHand :: String -> [SItem]
fromShortHand =  shortHand . smartWords . removeCBlockComents . removeCLineComments


shortHand :: [(String,[String])] -> [SItem]
shortHand [] = []
shortHand ((a,args):as) = SI(kwType k, v): shortHand as
            where
               k = findKey keyWords a
               v = (kwConv k) args

kill :: Char -> [Char] -> [Char]
kill c (a:as)
      | a == c = ' ' : kill c as
      | otherwise = a : kill c as
kill _ "" = ""

grabData :: [Char] -> [Char]
grabData = startGrab
  where
    startGrab "" = ""
    startGrab (a:as) 
      | a == '{' = stopGrab as
      | otherwise = startGrab as

    stopGrab "" = ""
    stopGrab (a:as)
      | a == '}' = ""
      | otherwise = a : (stopGrab as)
