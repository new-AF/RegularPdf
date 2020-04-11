# RegularPDF
# Author: Abdullah Fatota
# Description: PDF Authoring Tool

package require Tk
package require TclOO
wm title . {RegularPDF}
wm geometry . "700x400+[expr [winfo vrootwidth .]/2]+[expr [winfo vrootheight .]/2]"

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
variable Font {TkDefaultFont} IconFolder "\ud83d\udcc2" IconBack "\u2190" IconReload "\u21bb" boldfont {-font {-weight bold}} eVar {} eDirCount {0} ePath {} fVar {} eHover {} sVar {} jVar {} pdfWS [dict create null \0 htab \9 lfeed \a formfeed \c creturn \d s \20]

# Status Bar
set s [label .0label -relief sunken -borderwidth 2 -text ""]

#Checkbutton
proc Reliefbutton {name args} {
	
	puts stdout "(Reliefbutton) >$name< >$args<"
	set operation [lindex $args 0]
	
	switch $operation {
		ison {
			return [expr {"[$name cget -relief]" eq "sunken"}]
		}
		isoff {
			return [expr {"[$name cget -relief]" eq "groove"}]
		}
		default {
			set a [button $name {*}$args]
			$a config -command "do_Reliefbutton $a"
			#bindtags $a {$a ReliefButton  . all} ;# Button deleted
	
			return $a
		}
	}
	
}
proc bind_Reliefbutton {W to} {
	set command [$W cget -command]
	$W config -command "$command ; $to $W" ;# only appends , clear/remove the command when to == {}
}

proc do_Reliefbutton {W} {
		puts "ReliefButton Defualt Binding"
		set was [$W cget -relief]
		set will [switch $was groove { concat sunken} sunken {concat groove} ]
		$W config -relief $will
}

proc filter_pdf {W} {
	
	#puts "filter pdf ison [Reliefbutton $W ison]"
	if [Reliefbutton $W ison] {
	set a [lrange $::eVar 0 $::eDirCount-1]
	lappend a {*}[lsearch -nocase -inline -all -glob [lrange $::eVar $::eDirCount end] *.pdf];
	#puts "PDFs -> $b \n Others -> $a"
	set ::jVar $a
	$::e config -listvariable ::jVar
	} else {
	$::e config -listvariable ::eVar
	}
	
}
proc iseol {thing} {
	string match *\d\a*
}
variable Pdf
proc pdfparse {str} {
	
	set l [string length $str]
	set b 0
	puts [lrepeat 10 *]
	while {[set v [string range $str $b [incr b 7]]] != {} } {
		puts "\nCounter->$b String\{${v}\}\n"
		foreach vv [split $v ""] {puts -nonewline "[format %X [scan $vv %c]] "}
		foreach vv [split $v ""] {puts -nonewline "$vv "}
		puts {}
	}
	puts [lrepeat 10 *]
}
oo::class create Sanvas {
	
	variable main item scrollx scrolly
	
	constructor {name args} {
		
		array set res $[my parse $args]
		
		
		set main [labelframe $name -relief groove -bd 5 ]
		set item [canvas $name.canvas ]
		# Scrolling
		set scrollx [scrollbar $name.scrollx -orient horizontal -relief groove -command "$item xview" ]
		set scrolly [scrollbar $name.scrolly -orient vertical -relief groove -command "$item yview" ]
		$item configure -xscrollcommand "$scrollx set" -yscrollcommand "$scrolly set"
		
		pack $item -side right -expand 1 -fill both
		pack $scrolly -side right -fill y
		pack $scrollx -fill x
		
		return $main
	}
	
	
	method config {what args} {
		[susbt $what] config {*}$args
	}
	method parse {args} {
		set tmp(none) {}
		foreach v {frame canvas scrollx scrolly} {
			set i [lsearch $args -$v] ; if ![llength $i] { continue }
			set end [lsearch -start $i $args \}*] ; if [expr {$end == -1} ] { set end end }
			set tmp($v) [lrange $args $i $end]
		}
		return [array get tmp]
	}
	method get {what} {
		if [string equal $args {}] {my config main $args} 
		return $main
	}
	
	}
oo::class create SingleTab { 
	
	variable txt str path fh b 
	
	constructor {tempcount {temptxt ""}} {

		set txt $temptxt
		
		set com "[self] clicked"
		set b [button .tabs.canvas.button$tempcount -text [expr { $temptxt eq {} ? "Blank Document Text" : $temptxt }] -relief groove -cursor hand2 -command $com]
		if ![string equal $txt ""] {

		set path [file join $::ePath $txt]
		set fh [open $path r] ; fconfigure $fh -encoding binary ;# -translation binary -eofchar {}
		set str [read -nonewline $fh]
		#puts $str
		} else {
		set str {Blank Document}
		}
		
		my clicked
	}
	 method get {} {
	 	return $b
	 }
	 method close {} {
	 	close fh
	 }
	 method clicked {} {
	 	.main.canvas itemconfigure TEXT -text $str
	 	pdfparse $str
	 }

}
oo::class create Tabs { 
	
	variable fcount newcount lobj sobj
	
	constructor {} {
	
	my create_main
	#puts "---->>> [Sanvas new .main] <<<"
	
	labelframe .tabs -text {Current Tabs} -relief ridge -bd 5
	canvas .tabs.canvas
	set com "[self] create {}"
	button .tabs.canvas.add -text "\ud83d\uddcb Create New Document" -relief groove -command $com
	set fcount 0
	set newcount 0
	pack .tabs.canvas -fill both
	pack .tabs.canvas.add -fill x -pady 0.05in
	
	place .tabs -relx 0.66 -y 0 -relwidth 0.3 -relheight 1
	
	
	}
	
	method create_main {} {
		labelframe .main -relief groove -bd 5
		canvas .main.canvas -highlightbackground blue
		place .main -relx 0.34 -y 0 -relwidth 0.3 -relheight 1
		pack .main.canvas -expand 1 -fill both
		set pad [.main.canvas cget -highlightthickness]
		puts "[.main.canvas create text [expr 0+$pad] [expr 0+$pad] -text {	INITIAL TEXT} -tag TEXT -anchor nw]"
		set com "[self] width_changed %W"
		bind .main.canvas <Configure> $com
		#create_scrolls .canvas
		
	}
	
	method create {txt} {
		
		set new [SingleTab new [expr $fcount+$newcount] $txt]
		lappend lobj $new
		pack [$new get] -fill x -pady 0.05in
		switch $txt {} {incr newcount} default {incr fcount}
		
	}
	method width_changed {w} {
		set old [.main.canvas itemcget TEXT -width] 
		set new [winfo width .main.canvas]
		#puts "<configure event> old canas TEXT width $old new $new"
		.main.canvas itemconfigure TEXT -width $new
		
	}
	
}
Tabs create tabs
# List box Frame
set a [labelframe .0frame  -text "Items in current directory" -relief ridge -bd 5]

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
set e [listbox $a.0list -relief flat -highlightthickness 2 -highlightcolor [. cget -bg] -cursor hand2 -activestyle none -selectmode single -listvar eVar]
set f [listbox $a.1list -relief flat -highlightthickness 2 -highlightcolor red -cursor hand2 -activestyle dotbox -bg [. cget -bg] -listvar fVar]

proc distribute_scroll {things args} { foreach v $things { $v {*}$args } }
variable g [scrollbar $a.0scroll -orient vertical -command "distribute_scroll {$e $f} yview"] h [scrollbar $a.1scroll -orient horizontal -command "$e xview"]


$e config -xscrollcommand "$h set" -yscrollcommand  "$g set"
$f config -xscrollcommand "$h set" -yscrollcommand  "$g set"
# Pack "Items in current directory"

#pack $s -side top -fill x
#pack $a -side left -expand false -fill y
place $a -relx 0.02 -y 0 -relwidth 0.3 -relheight 1
pack $d -fill x
pack $buttonsBar -fill x
pack $b $c $j  -side right -anchor ne -padx 5
foreach v {$b $c $j} {[subst $v] config -relief groove}
pack $h  -side bottom  -fill x
pack $dd -fill x
pack $f -side left -expand 1 -fill y
pack $e -side left -expand 1 -fill both
pack $g -side left -expand 1 -fill y
#pack config $j -padx 0

proc Adjustf {} {
	set test tes
	set p [font measure $::Font $test]
	$::f config -width 3; #"${p}p"
	puts "Font measure >$test< >$p<"
}
#$e config -highlightbackground [$e cget -highlightcolor] ; # highlight background -> When NOT in Focus
$e config -background [. cget -background]

proc get_items {{path ""}} {
	
	if [string equal $path ""] {
		set path [pwd]
	}
	
	$::e delete 0 end
	variable ::eVar {} ::fVar {} ::ePath $path filenames {} dirnames {} 
	
	set files [concat [glob -directory $path -nocomplain  -types {f} *] [glob -directory $path -nocomplain  -types {f hidden} *] ]
	set dirs [concat [glob -directory $path -nocomplain  -types {d} *] [glob -directory $path -nocomplain  -types {d hidden} *] ] 
	
	#set filenames [lmap v $files { lindex [file split $v] end  }] ; #Not Available in Tcl8.5
	
	puts "*****path($path)*******
	*****files\[[llength $files]\]*******
	[join $files \n]
	*****directories\[[llength $dirs]\]*******
	[join $dirs \n]
	---------------"
	
	#puts $::eVar ; puts -------------
	
	variable ::eDirCount [expr {[llength $dirs]+1}] iconnames [lrepeat $::eDirCount $::IconFolder]
	
	foreach v $dirs {  lappend dirnames [lindex [file split $v] end] }
	foreach v $files {  lappend filenames [lindex [file split $v] end] }
	
	#set ::ePath [concat [lindex $dirs 0] [lindex $files 0]] ;# puts "%%%%%%%%%%%%%%%"; puts ">$::ePath<"
	#set ::ePath [file dirname [lindex $::ePath 0]]; #puts "====Current Path>$::ePath====="
	
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

proc list_select {w} {
	
	set from $::ePath
	
	set i [$w curselection]
	set txt [$w get $i]
	if {$i == {}} {
		return
	} elseif {$i == 0} {
		
		set to [file dirname $from]
		
	} elseif [expr {$i >= $::eDirCount}] {
		tabs create $txt
		return
	} else {
		set to [file join $from $txt]
		
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
bind $e <Visibility> { "[$c cget -command]" [file normalize ~/TestPDF]  ; Adjustf}
bind $e <<ListboxSelect>> {list_select %W}
bind $b <Motion> { set_statusbar [from_ns Tooltip %W] }
bind $c <Motion> { set_statusbar [from_ns Tooltip %W] }
#bind $j <Button> +{filter_pdf %W}
bind_Reliefbutton $j filter_pdf
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

proc get_center {win {before 1}} {
	set w [expr [winfo vrootwidth $win]/2]
	set h [expr [winfo vrootheight $win]/2]
	

	return "+$w+$h[switch $before 1 {} 0 {}]"
}
#button .mButton -text {Button 1} -bg #123456 -fg white
#place .mButton -relx 0.5 -rely 0.5 -anchor center

# Root Menu
menu .mMenu
. config -menu .mMenu

# Help->About Menu
menu .mMenu.mHelp
.mMenu.mHelp add command -command "wm deicon $z; wm geometry $z [get_center $z]" -label About
.mMenu add cascade -label Help -menu .mMenu.mHelp
.mMenu add command -label Console -command {console show}