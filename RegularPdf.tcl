# RegularPDF
# Author: Abdullah Fatota
# Description: PDF Authoring Tool

package require Tk

wm title . {RegularPDF}
wm geometry . {700x400}

button .button -text {Button 1} -bg #123456 -fg white
place .button -relx 0.5 -rely 0.5 -anchor center

menu .aMenu
menu .aHelp 
.aHelp add command -command {puts Test} -label About
. config -menu .aMenu
.aMenu add cascade -label Help -menu .aHelp