# RegularPDF
# Author: Abdullah Fatota
# Description: PDF Authoring Tool

package require Tk

wm title . {RegularPDF}
wm geometry . {700x400}

# About Dialog Window
toplevel .mTop
wm withdraw .mTop
wm title .mTop About
wm protocol .mTop WM_DELETE_WINDOW {wm withdraw .mTop}
label .mTop.0Label -text [wm title .] -font {Tahoma 16 normal}
label .mTop.1Label -text {A PDF Authoring Tool}
label .mTop.2Label -text "\u00a9 2020 Abdullah Fatota" -font {TkDefaultFont 10 italic}
pack .mTop.0Label .mTop.1Label .mTop.2Label -side top -pady 10 -padx 2cm

# List box Frame
set a [frame .0frame -relief groove -borderwidth 2]

pack $a -side left -expand false -fill y
# Current Items Label
set b [label .0frame.0label -text {Items in current directory}]
# Reload Button
set c [button .0frame.0button -text Reload]
# Separator
set d [ttk::separator .0frame.0separator -orient horizontal]
set dd [ttk::separator .0frame.1separator -orient horizontal]
# Pack
pack $b $d -fill x
pack $c -anchor ne -pady 2
pack $dd -fill x
# List box
set e [listbox $a.0list -relief flat -highlightthickness 2 -highlightcolor black -cursor hand2]
pack $e -side left -fill y 
#$e config -highlightbackground [$e cget -highlightcolor] ; # highlight background -> When NOT in Focus
$e config -background [. cget -background]
#bind .mList <Visibility> {puts "Visibility event fired >%s<"}

button .mButton -text {Button 1} -bg #123456 -fg white
place .mButton -relx 0.5 -rely 0.5 -anchor center

# Root Menu
menu .mMenu
. config -menu .mMenu

# Help->About Menu
menu .mMenu.mHelp
.mMenu.mHelp add command -command {wm deicon .mTop} -label About
.mMenu add cascade -label Help -menu .mMenu.mHelp