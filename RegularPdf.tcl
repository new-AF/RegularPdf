# RegularPDF
# Author: Abdullah Fatota
# Description: PDF Authoring Tool

package require Tk

wm title . {RegularPDF}
wm geometry . {700x400}

# About Dialog Window
toplevel .1top
wm withdraw .1top
wm title .1top About
wm protocol .1top WM_DELETE_WINDOW {wm withdraw .1top}
set z .1top
label $z.0label -text [wm title .] -font {Tahoma 16 normal}
label $z.1label -text {A PDF Authoring Tool}
label $z.2label -text "\u00a9 2020 Abdullah Fatota" -font {TkDefaultFont 10 italic}
foreach v {0 1 2} {
pack $z.${v}label -side top -pady 10 -padx 2cm
}
variable Font {TkDefaultFont} IconFolder "\ud83d\udcc2" IconBack "\u2190" IconReload "\u21ba" boldfont {-font {-weight bold}} eVar {} fVar {} eHover {}

# List box Frame
set a [labelframe .0frame -borderwidth 5 -text "Items in current directory" -relief ridge]
set buttonsBar [frame $a.0frame]

# Change directory Button
set b [button $buttonsBar.1button -text "$IconFolder .."]
# Reload Button
set c [button $buttonsBar.0button -text $IconReload]
#puts [$c configure]
# Separator
set d [ttk::separator .0frame.0separator -orient horizontal]
set dd [ttk::separator .0frame.1separator -orient horizontal]

# List box
set e [listbox $a.0list -relief flat -highlightthickness 2 -highlightcolor blue -cursor hand2 -activestyle dotbox -selectmode single -listvar eVar]
set f [listbox $a.1list -relief flat -highlightthickness 2 -highlightcolor red -cursor hand2 -activestyle dotbox -bg [. cget -bg] -listvar fVar]


variable g [scrollbar $a.0scroll -orient vertical -command "$e yview"] h [scrollbar $a.1scroll -orient horizontal -command "$e xview"]


$e config -xscrollcommand "$h set" -yscrollcommand  "$g set"

# Pack "Items in current directory"


pack $a -side left -expand false -fill y -padx 5 -pady 5
pack $d -fill x
pack $buttonsBar -fill x
pack $b $c -side right -anchor ne -padx 10
pack $h  -side bottom  -fill x
pack $dd -fill x
pack $f -side left -expand 1 -fill y
pack $e -side left -expand 1 -fill both
pack $g -side left -expand 1 -fill y
pack config $b -padx 0

proc Adjustf {} {
	set test tes
	set p [font measure $::Font $test]
	$::f config -width 3; #"${p}p"
	puts "Font measure >$test< >$p<"
}
#$e config -highlightbackground [$e cget -highlightcolor] ; # highlight background -> When NOT in Focus
$e config -background [. cget -background]

# Bind Button
proc get_items {{path ""}} {
	
	if {[string compare path ""]==0 } {
		set path [pwd]
	}
	
	
	set files [concat [glob -directory $path -nocomplain  -types {f} *] [glob -directory $path -nocomplain  -types {f hidden} *] ]
	set dirs [concat [glob -directory $path -nocomplain  -types {d} *] [glob -directory $path -nocomplain  -types {d hidden} *] ] 
	
	#set filenames [lmap v $files { lindex [file split $v] end  }] ; #Not Available in Tcl8.6
	
	
	puts "****[llength $dirs]*****"
	variable filenames "" dirnames "" iconnames [lrepeat [llength $dirs] $::IconFolder]
	
	foreach v "$dirs" {  lappend dirnames [lindex [file split $v] end] }
	foreach v "$files" {  lappend filenames [lindex [file split $v] end] }
	
	variable ::eVar {} ::fVar {}
	set ::eVar [lsort -nocase  $dirnames] ;# $filenames]
	lappend ::eVar {*}[lsort -nocase  $filenames]
	#set ::fVar $iconnames

	puts ************
	puts $dirnames ; puts ---------------
	puts $filenames ; puts -------------
	puts $::eVar ; puts -------------
	
	$::e insert 0 $::IconBack
	$::f insert 0 ""
	

	if {[string is alpha %s] == 1} { # unbind Visibility
		bind $::e <Visibility> ""
	}
}


$c config -command get_items
bind $e <Visibility> {$c invoke ; Adjustf}

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


button .mButton -text {Button 1} -bg #123456 -fg white
place .mButton -relx 0.5 -rely 0.5 -anchor center

# Root Menu
menu .mMenu
. config -menu .mMenu

# Help->About Menu
menu .mMenu.mHelp
.mMenu.mHelp add command -command "wm deicon $z" -label About
.mMenu add cascade -label Help -menu .mMenu.mHelp

console show