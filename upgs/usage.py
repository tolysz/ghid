#!/usr/bin/env python

inFile  = open ("all.up", "r" )

print "module Hid.UPG where"
print "import Hid.Hex\n"
print "toPageName i" 
line = inFile.readline().strip()

while (line != ""):
   ll = line.split("\t")
   dd = ll[0].split("-")
   if len(dd) >=2 :
       print "   | 0x%s <= i && i <=0x%s = \"%s\"" % (dd[0],dd[1], ll[1].replace( "%X", '"++(showihex i)++"' ))
   else:
       print "   | i == 0x%s = \"%s\"" % (ll[0], ll[1])
   line = inFile.readline().strip()