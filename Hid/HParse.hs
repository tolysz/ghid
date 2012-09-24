
module Hid.HParse where

import Hid.HTypes
import Hid.HTools

shortLen :: String -> Int
shortLen ('0':'0':_) = 0
shortLen ('1':'0':_) = 1
shortLen ('0':'1':_) = 2
shortLen ('1':'1':_) = 4

rlen ::  [a] -> RBitString
rlen c
     | lc == 0  = RBS "00"
     | lc == 8  = RBS "10"
     | lc == 16  = RBS "01"
     | lc == 32  = RBS "11"
         where 
          lc = length c

longLen :: String -> Int
longLen a = rb2uint2int $ textrbin2rbin $ take 8 a

hsparse :: RBitString -> [SItem]
hsparse = (map item2sitem) . hparse

hparse :: RBitString -> [Item]
hparse (RBS "") = []
hparse (RBS ('0':'1' :'1':'1' :'1':'1':'1':'1':as))  = (IT (RBS "11", RBS tag, RBS dat)) :hparse (RBS rest)
      where
        sl = longLen as
        (loc, rest) = splitAt ((sl * 8) + 8) (drop 8 as)
        (tag, dat) = splitAt 8 loc

hparse  (RBS s) = (IT (RBS ty, RBS tag, RBS dat)):hparse (RBS rest)
     where
       sl = shortLen s
       (ty,s4) = splitAt 2 (drop 2 s)
       (tag,s8) = splitAt 4 s4
       (dat, rest) = splitAt (8 * sl) s8

hsinter :: [SItem] -> RBitString
hsinter = hinter . (map sitem2item)

hinter :: [Item] -> RBitString
hinter [] = RBS ""
hinter (a:as) = (hinter1 a) +++ hinter as

hsinter1 :: SItem -> RBitString
hsinter1 = hinter1 . sitem2item

hinter1 :: Item -> RBitString
hinter1 (IT(a, b, c))
          | 8 == (length $ toStr b) = (RBS "01111111") +++ (uint2rb $ toEnum $ div (length $ toStr c) 8 ) +++ b +++ c
          | otherwise = (rlen $ toStr c) +++ a +++ b +++ c



{--
resolution h = (127-(-127)) / ((3175-(-3175)) * 10-4) = 400

nnan a b
     | a == NaN = b
     otherwise  = a
--}
