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

set Font TkDefaultFont
set UnicodeFolder "\ud83d\udcc2"
set UnicodeBack "\u2190"
set UnicodeReload "\u21ba"
set boldfont {-font {-weight bold}}

# List box Frame
set a [labelframe .0frame -relief groove -borderwidth 2 -text "Items in current directory" -relief solid]
pack $a -side left -expand false -fill y -padx 5 -pady 5
# Current Items Label
#set b [label .0frame.0label ]
# Reload Button
set c [button .0frame.0button -text $UnicodeReload]
#puts [$c configure]
# Separator
set d [ttk::separator .0frame.0separator -orient horizontal]
set dd [ttk::separator .0frame.1separator -orient horizontal]
# Pack
pack $d -fill x
pack $c -anchor ne
pack $dd -fill x
# List box
set e [listbox $a.0list -relief flat -highlightthickness 2 -highlightcolor blue -cursor hand2 -activestyle dotbox -selectmode single]
set f [listbox $a.1list -relief flat -highlightthickness 2 -highlightcolor red -cursor hand2 -activestyle dotbox -bg [. cget -bg]]
pack $e -side right -expand 1 -fill both
pack $f -side left -expand 1 -fill y
proc Adjustf {} {
	
	set p [font measure TkDefaultFont tes]
	$::f config -width 3; #"${p}p"
	puts "yyyyyyyyyyyy $p"
}
#$e config -highlightbackground [$e cget -highlightcolor] ; # highlight background -> When NOT in Focus
$e config -background [. cget -background]

# Bind Button
proc get_files {{path ""}} { ; # gets directories too.
	
	if {$path == ""} {
		set path [pwd]
	}
	$::e delete 0 end
	set files [glob -directory $path *]
	#puts $files
	$::e insert 0 $::UnicodeBack
	$::f insert 0 ""
	foreach f $files {
		$::e insert end [lindex [file split $f] end] 
		$::f insert end $::UnicodeFolder 
	}
	if {[string is alpha %s] == 1} {
		bind $e <Visibility> ""
	}
}


$c config -command get_files
bind $e <Visibility> {get_files ; Adjustf}

set eHover {}
proc hover {e y} {
	set i [$e nearest $y]
	
	if {$::eHover != $i} {
		if {$::eHover != ""} {$e itemconfig $::eHover -background [$e cget -background]}
		set ::eHover $i
	}
	
	if { [$e curselection] != $i } {
		$e itemconfigure $i -bg yellow
	}
}
bind $e <Motion> {
	
	hover $::e %y
	
	
}


bind $e <Leave> {
	set i [$::e nearest %y]
	if {[$::e curselection] != $i} {
		$::e itemconfig $i -background [$::e cget -background]
	}
	#puts $item
}

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

console show