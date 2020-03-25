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
variable Font {TkDefaultFont} IconFolder "\ud83d\udcc2" IconBack "\u2190" IconReload "\u21ba" boldfont {-font {-weight bold}} eVar {} eDirCount {0} ePath {} fVar {} eHover {} sVar {}

# Status Bar
set s [label .0label -relief sunken -borderwidth 2 -text ""]

#Checkbutton
proc Reliefbutton {name args} {
	
	puts stdout "--- >$name< >$args<"
	set operation [lindex $args 0]
	
	switch $operation {
		ison {
			return [expr "[$name cget -relief] eq sunken"]
		}
		isoff {
			return [expr "[$name cget -relief] eq raised"]
		}
		default {
			set a [button $name {*}$args -relief raised]
	
			bindtags $a {$a ReliefButton Button . all}
	
			return $a
		}
	}
	
}
proc bind_Reliefbutton {} {
	bind ReliefButton <ButtonRelease> { puts Relief }
}


# Tabs
namespace eval Tab {
	set a [labelframe .1frame -text {Open Tabs} -width 20 -bd 5]
	pack $a -side right -fill y
}

# List box Frame
set a [labelframe .0frame -borderwidth 5 -text "Items in current directory" -relief ridge]

set buttonsBar [frame $a.0frame]
# Filter PDF button
set j [Reliefbutton $buttonsBar.3button -text {Filter PDF files}]
# Change directory Button
set b [button $buttonsBar.1button -text "$IconFolder"]
# Reload Button
set c [button $buttonsBar.0button -text $IconReload]
#puts [$c configure]
# Separator
set d [ttk::separator .0frame.0separator -orient horizontal]
set dd [ttk::separator .0frame.1separator -orient horizontal]

# List box
set e [listbox $a.0list -relief flat -highlightthickness 2 -highlightcolor blue -cursor hand2 -activestyle dotbox -selectmode single -listvar eVar]
set f [listbox $a.1list -relief flat -highlightthickness 2 -highlightcolor red -cursor hand2 -activestyle dotbox -bg [. cget -bg] -listvar fVar]

proc distribute_scroll {things args} { foreach v $things { $v {*}$args } }
variable g [scrollbar $a.0scroll -orient vertical -command "distribute_scroll {$e $f} yview"] h [scrollbar $a.1scroll -orient horizontal -command "$e xview"]


$e config -xscrollcommand "$h set" -yscrollcommand  "$g set"
$f config -xscrollcommand "$h set" -yscrollcommand  "$g set"
# Pack "Items in current directory"

#pack $s -side top -fill x
pack $a -side left -expand false -fill y -padx 5 -pady 5
pack $d -fill x
pack $buttonsBar -fill x
pack $j $b $c -side right -anchor ne -padx 10
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


proc get_items {{path ""}} {
	
	if {[string compare $path ""]==0 } {
		set path [pwd]
	}
	
	variable ::eVar {} ::fVar {}
	
	set files [concat [glob -directory $path -nocomplain  -types {f} *] [glob -directory $path -nocomplain  -types {f hidden} *] ]
	set dirs [concat [glob -directory $path -nocomplain  -types {d} *] [glob -directory $path -nocomplain  -types {d hidden} *] ] 
	
	#set filenames [lmap v $files { lindex [file split $v] end  }] ; #Not Available in Tcl8.5
	
	
				; puts "*****files([llength $files])*******"
	puts $files; puts "*****directories([llength $dirs])*******"
	puts $dirs ;
	puts ---------------
	
	#puts $::eVar ; puts -------------
	
	set ::eDirCount [expr {[llength $dirs]+1}]
	variable filenames "" dirnames "" iconnames [lrepeat $::eDirCount $::IconFolder]
	
	foreach v "$dirs" {  lappend dirnames [lindex [file split $v] end] }
	foreach v "$files" {  lappend filenames [lindex [file split $v] end] }
	
	set ::ePath [concat [lindex $dirs 0] [lindex $files 0]] ;# puts "%%%%%%%%%%%%%%%"; puts ">$::ePath<"
	set ::ePath [file dirname [lindex $::ePath 0]]; #puts "====Current Path>$::ePath====="
	
	set ::eVar [lsort -nocase  $dirnames] ;# $filenames]
	lappend ::eVar {*}[lsort -nocase  $filenames] ;
	
	$::e insert 0 $::IconBack
	set ::fVar [concat $iconnames [expr { [llength $files] ? [lrepeat [llength $files] {}] : {} }]]

	# unbind Visibility
	if {[string is alpha %s] == 1} { 
		bind $::e <Visibility> ""
	}
}

proc change_dir {} {
	set dir [tk_chooseDirectory -title {Choose a directory to list its contents}]
	get_items $dir
}

proc list_select {widget} {
	
	set from $::ePath
	
	set i [$widget cursel]
	if {$i == {}} {
		return
	} elseif {$i == 0} {
		
		set to [file dirname $from]
		
	} else {
		set to [file join $from [$widget get $i]]
		
	}
	puts "from {$from} to {$to}"
	set ::eHover {} ; get_items $to
	
}

#Tooltips "database"
namespace eval Tooltip {
	set $::b "Choose a different directory to list its contents"
	set $::c "Reload and relist items (as Folders and Files) in current directory"
}

proc from_ns {N name} {
	
	return [set ::[concat $N]::$name]
}

proc set_statusbar {what} {
	$::s config -text $what
}
	

# Bind Things
$c config -command get_items
$b config -command change_dir
bind $e <Visibility> {$c invoke ; Adjustf ; bind_Reliefbutton}
bind $e <<ListboxSelect>> {list_select %W}
bind $b <Motion> { set_statusbar [from_ns Tooltip %W] }
bind $c <Motion> { set_statusbar [from_ns Tooltip %W] }
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
bind $e <Motion> { hover %W %y }


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