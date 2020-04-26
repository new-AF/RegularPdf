# RegularPDF
# Author: Abdullah Fatota
# Description: PDF Authoring Tool

package require Tk
package require TclOO
wm title . {RegularPDF}
wm geometry . "700x400+[expr [winfo vrootwidth .]/2]+[expr [winfo vrootheight .]/2]"
panedwindow .pane -showhandle 1 -sashwidth 10 -sashpad 20 -sashrelief raised -handlepad 50
place .pane -relx 0 -rely 0.1 -relwidth 1 -relheight 1
# About Dialog Window
proc buttonhover {w} {
	$w config -relief ridge
	
}
proc buttonleave {w} {
	$w config -relief flat
	
}

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
variable Font {TkDefaultFont} IconFolder "\ud83d\udcc2" IconBack "\u2190" IconReload "\u21bb" Cursor "\u25a0" boldfont {-font {-weight bold}} eVar {} eDirCount {0} ePath {} fVar {} eHover {} sVar {} jVar {} pdff {} misc [dict create] cFont {} cSize {} cDim {} bCursor {} tIndex {} bIndex {}

proc grand_annoucement {args} {
	puts "*-*-*-*$args*-*-*-*-*"
}
proc get_args {List args} {
	set r [list]
	
	foreach v $args {
		set test [lsearch $List $v]
		if {$test != -1} {
			lappend r [lindex $List $test+1]
		}
		}
	return $r
	}
proc change_font {args} {
	if {$::cFont eq {}} {
		set r [.pane.main.canvas itemconfig TEXT -font]
		if {[lsearch $r {*-size*}] != -1} {grand_annoucement canvas font returned size! $r}
		set ::cFont [font actual [lindex $r 3]]
		set ::cSize [get_args $::cFont -size]
	} else {
		#grand_annoucement cSize $::cSize
		incr ::cSize [lindex $args end]1
	}
	set ::cDim [.pane.main.canvas bbox TEXT]
	.pane.main.canvas itemconfig TEXT -font "-size $::cSize"
	return $::cSize
}
# **Stack** #
button .left -text \ud83e\udc44 -font {-size 16}
button .rght -text \ud83e\udc46 -font {-size 16}

dict append misc  stack 1
set stackVar {}
proc StackThings {args} {
	if [dict get $::misc stack] {
		
		#foreach v "$::a .pane.main .pane.tabs" {
		#	bind $v <Enter> "StackThings move $v"
		#}
		
	}
	
	place config .pane -rely 0.4
}

proc aphoto {args} { ; # alpha photo
	# ?create? ?zoom factorx factory? ?alpha newValue?
	
	set pho [lindex $args 0]
	
	if [string equal $pho create] { set pho  [image create photo [lindex $args 1]]
		set args [lreplace $args 0 1] } else { set args [lreplace $args 0 0]  }
	
	set args2 [dict create]
	
	for {set v [llength $args]} {[incr v -1] >= 0} {}  {
		switch [lindex $args $v] {
			zoom {dict set args2 zoom [list $v [expr $v+2]] }
			alpha {dict set args2 alpha [list $v [expr $v+1]]}
		}
	}
	
	foreach v [dict keys $args2] {
		set value [dict get $args2 $v]
		dict set args2 $v [lrange $args [lindex $value 0]+1 [lindex $value 1] ]
		set args [lreplace $args {*}$value]
	}
	
	$pho configure {*}$args
	foreach v [dict keys $args2] {
		set value [dict get $args2 $v]
		switch $v {
		zoom {pho copy pho {*}$value }
		alpha { set x [$pho cget -width] ; lappend x [$pho cget -height] ; set x [lsort $x]
			set y [lindex $x 1]
			set x [lindex $x 0] ;
			while {[incr x -1] >= 0} {
				set tempy $y
				while {[incr tempy -1]>=0} {$pho transparency set $x $tempy $value -alpha }
				
			} 
		}
	} 
		
	}
	return $pho
}
####

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

proc pdf {com args} {

	variable Info [dict create]	 null \x0 	htab \x9 	nextline \xa	nextpage \xc	cr \xd		space \x20
	set delimeter [dict create leftparan \x28 rightparan \x29 leftangle	\x3c rightangle	\x3e lefsquare	\x5b rightsquare \x5d leftcurly	\x7b rightcurly \x7d unixslash	\x2f percent \x25]
	set ar {-all -indices -inline}
	if {{-is} in $args} { lset $ar [lsearch $ar -inline ] "" ; lset $args [lsearch $args -is ] "" }
	
	set target [expr { [llength $args] ? [concat $args] : $::pdff }] 
	
	set new [list]	; set result [switch $com {
		
		wspace {
			regexp {*}$ar "(?:$null)|(?:$htab)|(?:$nextpage)|(?:$space)" $target
		}
		
		eol { ; # EOL MARKERS
			regexp {*}$ar "(?:$cr$nextline)|(?:$cr)|(?:$nextline)" $target
		}
		delimeter {
			regexp {*}$ar [join [lmap v [dict values $delimeter *] {concat (?:$v)}] |] $target
		}
		default {
			puts "::pdf -> Unrecognized Command"
			return
		}
	
	}]
	
	set len [llength $result]
	if {{-indices} in $ar} { for {set index 0} {$index < $len} {incr index 1} {lappend new [lindex [lindex $result $index] 0]} }
	return $new
}

proc pdfparse {objpath} { #object is ::oo::objxxx it is result of [self] from the calling Object. 
	
	set str [subst $[subst $objpath]::str] ; puts "**pdf file Legnth: [string length $str] Bytes**"
	
	#set calc [expr 8*4+4*4-1] ; #12 ab cd 32 ....
	puts [lrepeat 10 *]
	
	set ::pdff $str

}

proc TEXThover {args} {
	grand_annoucement TEXThover $args $::cDim
	variable x [lindex $args 0] y [lindex $args 1]
	if { $y+5 >= [lindex $::cDim 3] || $x >= [lindex $::cDim 2] } {grand_annoucement Nope ; return}
	set ::tIndex [.pane.main.canvas index TEXT @$x,$y]
	set i $::tIndex
	
	if {$::bCursor == {}} {
		.pane.main.canvas insert TEXT $i $::Cursor
		set ::bIndex $i
		set ::bCursor Set
		return
	}
	.pane.main.canvas imove TEXT $::bIndex $x $y
	.pane.main.canvas dchars TEXT $::bIndex
	.pane.main.canvas insert TEXT $i $::Cursor
	
	set ::bIndex $i
	puts $::tIndex
}
proc create_scrolls {name} {
	
	set main [winfo parent $name]
	
		# Scrolling
		scrollbar ${main}.scrollx -orient horizontal -relief groove -command "$name xview"
		scrollbar ${main}.scrolly -orient vertical -relief groove -command "$name yview"
		$name configure -xscrollcommand "${main}.scrollx set" -yscrollcommand "${main}.scrolly set"
		
		pack forget $name 
		pack ${main}.scrollx -side bottom  -fill x
		pack $name -side left  -fill y
		pack ${main}.scrolly -side right -expand 1 -fill y
		#return $main
	
	}
oo::class create SingleTab { 
	
	variable txt str path fh b 
	
	constructor {tempcount {temptxt ""}} {

		set txt $temptxt ; incr tempcount
		
		set com "[self] clicked"
		set b [button .pane.tabs.button$tempcount -text [expr { $temptxt eq {} ? "Blank Document #$tempcount" : $temptxt }] -relief groove -cursor hand2 -command $com]
		if ![string equal $txt ""] {

		set path [file join $::ePath $txt]
		set fh [open $path]
		fconfigure $fh -translation binary
		set str  [read $fh]
		
		} else {
		set str {Blank Document}
		}
		
		#after 1000 "[self] clicked"
		my clicked
	}
	 method get {} {
	 	return $b
	 }
	 method close {} {
	 	close fh
	 }
	 method clicked {} {
	 	.pane.main.canvas itemconfigure TEXT -text [string range $str 0 100]
	 	pdfparse [self]
	 }

}
oo::class create Tabs { 
	
	variable fcount newcount lobj sobj m mc 		t tc
	
	constructor {} {
	
	my create_main
	
	
	set t [labelframe .pane.tabs -text {Current Tabs} -relief ridge -bd 5]
	
	set tc [canvas $t.canvas]
	set com "[self] create {}"
	
	
	button $tc.add -text "\ud83d\uddcb New Document" -relief groove -command $com
	ttk::separator $tc.end -orient horizontal
	
	set fcount 0
	set newcount 0
	pack $tc -fill both
	
	
	pack $tc.add -fill x -pady 0.05in -side right
	pack $tc.end -fill x -pady 0.05in -side bottom
	
	#place .tabs -relx 0.66 -y 0.4in -relwidth 0.3 -relheight 1
	
	
	}
	
	method create_main {} {
		set m [labelframe .pane.main -relief groove -bd 5]
		set tbar [frame .pane.main.toolbar -relief groove -bd 2]
		set mc [canvas $m.canvas -highlightbackground green]
		#place .main -relx 0.34 -y 0.4in -relwidth 0.3 -relheight 1
		pack $mc -expand 1 -fill both
		pack $tbar -expand 1 -fill both
		set pad [$m.canvas cget -highlightthickness]
		$mc create text [expr 0+$pad] [expr 0+$pad] -text {INITIAL TEXT} -tag TEXT -anchor nw
		set com "[self] width_changed %W"
		bind $mc <Configure> $com
		$mc bind TEXT <Motion> "TEXThover %x %y %h"
		create_scrolls $mc
		my fill_canvas_toolbar
		
	}
	
	method create {txt} {
		
		set new [SingleTab new [expr $fcount+$newcount] $txt]
		lappend lobj $new
		pack [$new get] -fill x -pady 0.05in
		switch $txt {} {incr newcount} default {incr fcount}
		
	}
	method width_changed {w} {
		set old [$mc itemcget TEXT -width] 
		set new [winfo width $mc]
		#puts "<configure event> old canas TEXT width $old new $new"
		$mc itemconfigure TEXT -width $new
		$w config -scrollregion [$w bbox all]
		
	}
	method fill_canvas_toolbar {} {
		set tbar $m.toolbar
		variable zoomin [button $tbar.zoomin -text "Enalrge Text" -relief groove -command "change_font +"]
		variable zoomout [button $tbar.zoomout -text "Ensmall Text" -relief groove -command "change_font -"]
		
		pack $zoomin -side left -expand 0
		pack $zoomout -side left -expand 0
	}
	
}
Tabs create tabs
# List box Frame
set a [labelframe .pane.file  -text "Items in current directory" -relief ridge -bd 5]

set buttonsBar [frame $a.buttonbar]
# Filter PDF button
set j [Reliefbutton $buttonsBar.3button -text {Filter PDF files}]
# Change directory Button
set b [button $buttonsBar.1button -text "$IconFolder"]
# Reload Button
set c [button $buttonsBar.0button -text $IconReload]
#puts [$c configure]
# Separator
set d [ttk::separator $a.0separator -orient horizontal]
set dd [ttk::separator $a.1separator -orient horizontal]

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
#place $a -relx 0.02 -y 0.4in -relwidth 0.3 -relheight 1 
pack $d -fill x 				; 
pack $buttonsBar -fill x			; 
pack $b $c $j  -side right -anchor ne -padx 5 	
foreach v {$b $c $j} {[subst $v] config -relief groove}
pack $h  -side bottom  -fill x
pack $dd -fill x
pack $f -side left -expand 1 -fill y
pack $e -side left -expand 1 -fill both
pack $g -side left -fill y
#pack config $j -padx 0

# ***Toolbar Buttons*** #
proc ToolbarButton {args} {
	puts "** args are -> $args"
	set result [button {*}$args -relief flat]
	bind [lindex $args 0] <Enter> {buttonhover %W}
	bind [lindex $args 0] <Leave> {buttonleave %W}
	return $result
}

# ***Toolbar*** #
frame .toolbar -relief flat -bd 5 ; pack [ttk::separator .toolbar.endseparator -orient horizontal] -side bottom -expand 1 -fill x -pady 1
pack [button .toolbar.first -text {} -relief flat -state disabled] -side left -expand 0 -fill none
place .toolbar -x 0 -y 0 -relwidth 1 -height 0.4in
ToolbarButton .toolbar.stack -text "\u2b94 Stack Things" -command StackThings
pack .toolbar.stack -side left
####

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

# Pane additions
foreach v "$a .pane.main .pane.tabs" {.pane add $v -sticky nswe -stretch always
puts "**$v**"
}

# Root Menu
menu .mMenu -tearoff 0
proc setmenu {{what .mMenu}} {. config -menu $what}
proc debug {} {
	puts yess
	
}
# Help->About Menu
menu .mMenu.mHelp -tearoff 0
.mMenu.mHelp add command -command "wm deicon $z; wm geometry $z [get_center $z]" -label About
.mMenu add cascade -label Help -menu .mMenu.mHelp
.mMenu add command -label Console -command {console show} ; proc postmenu {name menu} { 
			$menu post [winfo rootx .toolbar.menu$name ]  [expr [winfo rooty .toolbar.menu$name ]+[winfo height .toolbar.menu$name ]] }
.mMenu add command -label Debug -command debug ; 

proc lin {target supplied args} {
	set result 1
	if {{-missing} in $args} {set result [llength $args]; # result is how many missing elements of $supplied are in $target.
		foreach v $supplied {incr result -[expr {"$v" in $target}]}
	} else {
	
        	foreach v $supplied {
        		if {$v ni $target} {set result 0; break}
        	}
	}
	return $result;
}

dict append misc switchmenu 1

proc ToolbarMenu {args}  {
	
	foreach command $args {
	switch $command {
		put {
			foreach x {{Help .mMenu.mHelp} {Console } {Debug }} {
				set v [lindex $x 0] 
				set m [lindex $x 1]
				set w [string tolower $v] 
				
				button .toolbar.menu$w  -text $v -relief flat
												; #.toolbar.$w configure -font [concat [.mMenu config -font]]
				bind .toolbar.menu$w <Enter> {buttonhover %W}
				bind .toolbar.menu$w <Leave> {buttonleave %W} ; puts **$w**
				
				.toolbar.menu$w config -command "if {[string equal {} $m]} {.mMenu invoke $v} else {postmenu $w $m}"
				
			}
			ToolbarButton .toolbar.switchmenu -text "\u2b9d Up Menu" -command {ToolbarMenu swap}
			ttk::separator .toolbar.switchmenu_separator -orient vertical
		}
		unpack {
			foreach v [lsearch -all -inline [winfo children .toolbar] .toolbar.menu*] {
				try {pack forget $v } on error {} {}
			}
		}
		pack {
			foreach v [lsearch -all -inline [winfo children .toolbar] .toolbar.menu*] {
				 try {pack $v -side left -after .toolbar.first} on error {} {}
			}
			
			if [dict get $::misc switchmenu] { 
				pack .toolbar.switchmenu -side left -before .toolbar.stack
				pack .toolbar.switchmenu_separator -side left -padx 1 -fill y -after .toolbar.switchmenu
				dict set ::misc switchmenu 0  }
		}
		swap { ; # like a on/off switch which to show first and hide the second
			set str [.toolbar.switchmenu cget -text]
			set to [lindex $str 1]
			if {"$to" eq "Up"} {
				ToolbarMenu unpack ; setmenu ; 
				.toolbar.switchmenu configure -text [string map {Up Down \u2b9d \u2b9f} $str] 
				
			} else {
				ToolbarMenu pack ; setmenu {} 
				.toolbar.switchmenu configure -text [string map {Down Up \u2b9f \u2b9d} $str] }
		}
		
	}
	

	
} }
ToolbarMenu put pack ; setmenu {} 
bind . <Visibility> {

	set cDim [.pane.main.canvas bbox TEXT]
	bind . <Visibility>
}