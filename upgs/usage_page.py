#!/usr/bin/env python

import os
import re

was = False

def upg():
   inFile  = open ("all.up", "r" )
   print "module Hid.UPG where"
   print "import Hid.HTypes\n"
   print "toPageName :: Int -> String"
   print "toPageName i" 
   line = inFile.readline().strip()

   while (line != ""):
      ll = line.split("\t")
      print "   | %s  = \"%s\"" % (rang( "i" ,ll[0]), subs(ll[1]))
      line = inFile.readline().strip()
   print 'toPageName i =  error ("Page Name out of bounds "++ (show i))'
def subs(a):
    return a.replace( "%X", '"++(showihex i)++"' ).replace( "%x", '"++(showihex j)++"' )

def rang(i,xx):
    dd = xx.split("-")
    if len(dd) >=2:
       return "0x%s <= %s && %s <=0x%s" % (dd[0],i,i,dd[1])
    else:
       return "0x%s == %s " % (dd[0],i)

def pars(lines,x):
   mewas = False
   for line in lines:
    if line != "":
      ll = line.strip().split("\t")
      tes = ll[0].replace("M","")
      if len(tes)==len(ll[0]):
         print "   | %s && %s = \"%s\"" % (rang("i",x),rang( "j" ,ll[0]), subs(ll[1]))
      else:
         mewas = True
         was = True
         print "   | %s && %s = \"%s\"" % (rang("i",x),rang( "j" ,tes), subs(ll[1]))
   if mewas:
     print "   | %s && j >=0x1000 = modify (j `div` 0x1000) (toUsageName i (j `mod` 0x1000))" % (rang("i",x))
def upg2():
   print "\n\n\ntoUsageName :: Int -> Int -> String\ntoUsageName i j"
   for subdir, dirs, files in  os.walk("."):
      files.sort()
      for file in files:
         if re.search("\.upg$",file):
            print "--  %s" % file
            f=open(file, 'r')
            lines=f.readlines()
            f.close()
            ll = lines.pop(0).strip().split("\t")
            lines.pop(0)
            pars(lines,ll[0])
   print 'toUsageName i j =  error ("Usage Name out of bounds "++ (show i) ++ "  " ++ (show j) )'
upg()
upg2()

if True:
 print ""
 print 'modify :: Int -> String -> String'
 print 'modify 0x0 s = s'
 print 'modify 0x1 s = "Change Sensitivity Absolute(" ++ s ++ ")"'
 print 'modify 0x2 s = "Maximum(" ++ s ++ ")"'
 print 'modify 0x3 s = "Minimum(" ++ s ++ ")"'
 print 'modify 0x4 s = "Accuracy(" ++ s ++ ")"'
 print 'modify 0x5 s = "Resolution(" ++ s ++ ")"'
 print 'modify 0x6 s = "Threshold High(" ++ s ++ ")"'
 print 'modify 0x7 s = "Threshold Low(" ++ s ++ ")"'
 print 'modify 0x8 s = "Calibration Offset(" ++ s ++ ")"'
 print 'modify 0x9 s = "Calibration Multiplier(" ++ s ++ ")"'
 print 'modify 0xA s = "Report Interval(" ++ s ++ ")"'
 print 'modify 0xB s = "Frequency Max(" ++ s ++ ")"'
 print 'modify 0xC s = "Period Max(" ++ s ++ ")"'
 print 'modify 0xD s = "Change Sensitivity Percent of Range(" ++ s ++ ")"'
 print 'modify 0xE s = "Change Sensitivity Percent Relative(" ++ s ++ ")"'
 print 'modify 0xF s = "Reserved for Vendors/OEMs(" ++ s ++ ")"'
 print 'modify _ _ = error "Can not mofify"'
