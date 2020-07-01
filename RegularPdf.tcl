# RegularPDF
# Author: Abdullah Fatota
# Description: PDF Authoring Tool

package require Tk
package require TclOO

wm title . RegularPDF
wm geometry . "700x400+[expr [winfo vrootwidth .]/2-350]+[expr [winfo vrootheight .]/2-200]"
place [panedwindow .pane -showhandle 1 -sashwidth 10 -sashpad 20 -sashrelief raised -handlepad 0] -x 0 -y 0 -relwidth 1 -relheight 0.9
frame .resize -bd 5 -relief groove
# About Dialog Window
proc buttonhover {w} {
	$w config -relief ridge
	
}

proc buttonleave {w} {
	$w config -relief flat
	
}
proc Args {lst args} {
	#Args $args -angles 60deg -points 0,0 +10,0 -10,-10 -anchor 1 -sides
	set x [lsearch -nocase -all $args -\[a-z\]* ] ;#option_indexes
	set d [dict create] ; #vairables_dictionary
	
	foreach name {args lst} {
		upvar 0 $name var
		foreach i [list {*}[lrange $x 1 end] [llength $var]] j $x {
			#puts "i-j  [expr {$i - $j} ] =[lrange $var $j+1 $i-1]="
			dict set d [lindex $var $j] [expr { ($i - $j) > 1 ? [lrange $var $j+1 $i-1] : {} } ]
		}
		set x [lsearch -all $lst -?* ]
		if {$x eq {}} then {break}
	}
	#set con "$d"
	#concat
	set com [string cat {dict for {key value} } "{$d}" { {set [string range $key 1 end] $value}} ]
	
	
	uplevel 1 $com
	return x
}
proc ParsePoints {lst} {
	#puts =[ParsePoints {10,10 +1,+1 -2,+2 3,3}]=
	set xcat [list] ; # 
	set x [lmap i $lst {subst [split $i ,]}]
	set last [list x x]
	foreach l $x  { ;#i [range 0 [llength $lst]-1]
		set w 0
		
		foreach p $l {
			if {[string index $p 0] eq {#}} {set p [split #] ; lset p 0 [lindex $xcat [lindex $p 1] $w ]}
			if {[string first + $p] != {-1} || [string first - $p] != {-1}} {set p [expr [lindex $last $w] $p]}
			lappend xcat $p
			lset last $w $p
			set w 1
		}
		
	}
	return $xcat
}
proc Angle2 {a args} {
	#Angle2 -rad 50 40
	set a [list {*}$a {*}$args]
	set a [join $a]
	lassign $a w
	if [string match -?* $w] {set a [lrange $a 1 end] ; set w [string range $w 1 end]} else {set w {}; return}
	
	lassign [ list [expr { 2 * $::pi / 360 }] [expr { 360 / (2 * $::pi) }] ] radUnit degUnit
	set unit [set ${w}Unit]
	set a [lmap i $a {subst [expr $i]}]
	set r [lmap i $a {subst [expr {$i * $unit} ]  }]
	return $r
}
proc Angle {a args} {
	#Angle -rad +99rad 7deg 1deg 1rad
	set a [list {*}$a {*}$args]
	set w [string trimleft [lindex $a 0] -]
	set a [lrange $a 1 end]
	
	foreach name {deg rad} {
		set $name [lsearch -all -glob $a ?*$name]
	}
	if { ([llength $rad] + [llength $deg]) != [llength $a] } {error "Malformatted arguments: =$a="}

	
	set unit [switch $w { rad {subst [expr 2*$::pi/360]} deg {subst [expr 360/(2*$::pi)]}  }]
	
	foreach name {deg rad} {
		set tmp [ lmap i [set $name] {subst "$i [string trimright [lindex $a $i] $name]"} ]
		set $name [dict create {*}[join $tmp]]
	}
	concat
	
	set w2 [switch $w deg {subst rad} rad {subst deg}]
	set $w2 [dict map {key val}  [set $w2]  {
		set val [expr $val]
		expr { $val * $unit }
	}]
	set l [lrepeat [llength $a] x]
	set mega [dict merge $deg $rad]
	
	dict for {key val} $mega {
		lset l $key $val
	}
	
	#set $name [lmap i [set $name] {set tmp [string trimright $i $name] ; expr {$tmp * $unit} }]
	#set d [lsearch -all -glob $a ?*rad]
	#set rad [lsearch -all -regexp $a {([0-9])+\.?rad}]
	#set deg [lsearch -all -regexp $a {([0-9])+\.?deg}]
	#set rad [lmap i $rad {}]
	return $l
}
proc PolarToRect2 {a args} {
	set a [list {*}$a {*}$args]
	set a [join $a]
	#[list 0 0 $p1x -$p1y $p2x -$p2y $p2x 0]
	set a [lmap i $a {subst [expr $i]}]
	
	set result [list]
	for {set len [expr {[llength $a] / 2}] ; set count 0} {$count < $len} {incr count 1} {
		set r [lindex $a $count]
		set th [lindex $a $count+1]
		lappend result "[expr {$r * cos($th)}] [expr {$r * sin($th)}]"
	}
	return $result
	
}
proc PolarToRect {a  args} {
	#PolarToRect [Angle -deg 90deg] 1
	#PolarToRect pi 1 2*pi 10
	lassign $a w
	if {$w eq {-zip}} {set a [lrange $a 1 end]}
	set a [list {*}$a {*}$args]
	#set w [string trimleft [lindex $a 0] -]
	#set a [lrange $a 1 end]
	set len [llength $a]
	if [expr $len%2] {error {Count of Arguments: count is not even}}
	
	if {$w ne {-zip}} {
		for {set i 0} {$i < $len} {incr i [expr {$i ? $i : 2}]} {
			set r [lindex $a $i+1]
			set theta [lindex $a $i]
			lset a $i "[expr {$r * cos($theta)} ] [expr {$r * sin($theta)} ]"
		}
		set a [lreplace $a $i-1 end]
		} else {
		set a [lmap theta [lindex $a 0] r [lindex $a 1] { subst "[expr {$r * cos($theta)} ] [expr {$r * sin($theta)} ]" }]
	}
	return $a
}


proc Shape {what args} {
	#Args $args -angles 60deg -points 0,0 +10,0 -10,-10 -anchor 1 -sides
	switch $what {
		triangle {
			Args $args -angles -points -anchor -sides -height
			
			#if {!($points ne {} || ($angles ne {} && $sides ne {})) } {
			#	error { Missing Arguments: Either -points or (-angles and -sides) must be supplied. } }
			if {$points ne {}} {
				set points [ParsePoints $points]
			} elseif {$height ne {}} {
				#angle is taken care of in Angle2
				set angles [expr { $angles eq {} ? 60 : $angles}]
				set height [expr $height] 
				set angle [Angle2 -rad $angles]
				set s9 [expr {tan($angle) / $height}]
				
				set s1 [expr { sqrt($s9**2 + $height**2) }]
				set result [Shape triangle -angles $angles $angles -sides $s1]
			} else {
				set len [llength $angles]
				if {$len != 2} {error "Shape: Triangle: Got $len Angle(s) =$angles= But 2 are nedded.  "}
				lassign $angles a1 a2 	; #only 2 angles needed: (left-side) and (right-side)
				set a2orig $a2
				lassign [Angle2 -rad $a1 [expr {$a2 > 90 ? $a2 : 180-$a2}]] a1 a2
				lassign $sides s1  		;# only 1 side's length needed from the user, for now (becasue the alogrith assumes a horizontaol and flat third side)
				lassign [PolarToRect2 $s1 $a1] p1 ; lassign $p1 p1x p1y
				set a9 $a2
				set s9 [expr { abs($p1y / tan($a9)) }]
				#if {$a2orig > 90} { set s9 0}
				set s2 [expr {sqrt($s9**2 + $p1y**2)}]
				lassign [PolarToRect2  $s2 $a2] p2 ; lassign $p2 p2x p2y
				# the 3rd side is not needed since [canvas polygon] will automaticallyly return to (p1x 0) thereby creating a (horizontal line).
				#set p2x [expr {$p2x + $p2x + $p1x}]
				#set result [list 0 0 $p1x -$p1y ]  ; # Minus the y-axis on the tkinter canvas is inverted.
				#set result [list 0 0 $p2x [expr -$p2y] ]
				set result [list 0 0 $p1x [expr -$p1y] [expr ($p1x  + abs($p2x))] 0]
				if {$a2orig > 90} { set result [list 0 0 $p1x [expr -$p1y] [expr ($p1x  + $p2x)] 0] }
				
			}
			
		}
		parallel {
			Args $args -angle -width -height
			if {$angle eq {}} {error {Shape-> parallel: Argument Missing: -angle not specified.}} elseif {
				$width eq {} || $height eq {}} {error "Shape-> parallel: Argument Missing: [expr {$width eq {} ? {-width} : {}}] [expr {$width eq {} ? {-height} : {}}] not specified."}
			
			set over90 [expr {$angle > 90 ? 1 : 0}]
			
			lassign [Angle2 -rad $angle] a1
			set s1 [expr { $height / sin($a1) }]
			lassign [PolarToRect2 $s1 $a1 ] p ; lassign $p x y
			
			
			set result [list 0 0 $x [expr -$y] [expr {$x + $width}]  [expr -$y] $width 0  ]
		}
		
	}
	return $result
}
proc operation {what a args} {
 set args [list {*}$a {*}$args]
 set d [dict create plus + minus - product * divide / raise **] ;#Symbols
 if { $what eq {vectorize} || $what eq {vectorise} } {
	set vec 1
	lassign $args what
	set args [lrange $args 1 end] 
	}
 set s [dict get $d $what] ;#Decode the symbol
 
 if {[llength $args] < 2} { if {$args eq {} } { return {} } else { return [expr "$s $args"] } } 
 
 set str {}
 
 if [info exists vec] {
 
	set s "$s[lindex $args end]"
	set args [lrange $args 0 end-1]
	set str [lmap v $args {subst [expr $v$s]}]
 } else {

	set str [join $args $s]
	set str [expr $str]
 }
 
 return $str
}

proc range {from to {by 1}} {
	set from [expr { $from }] 
	set to [expr  $to ] 
	set str [lrepeat [expr { $to - $from - $by + 1}] x ]
	incr from -$by
	set lst [lmap v $str { subst [incr from $by] }]
	return $lst
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

variable cc .pane.main.canvas
variable Font {TkDefaultFont} IconSave "\ud83d\udcbe" IconFolder "\ud83d\udcc2" IconBack "\u2190" IconReload "\u21bb" BlockCursor "\u25a0" IconFontIncrease "\ud83d\uddda" IconFontDecrease "\ud83d\udddb" IconPlus "\uff0b" IconMinus "\u2212" IconZoom "\ud83d\udd0e" IconSeethrough "\u239a" boldfont {-font {-weight bold}} eVar {} eDirCount {0} ePath {} fVar {} eHover {} sVar {} jVar {} pdff {} misc [dict create] cFont {} cSize {} cDim {} bCursor {} tIndex {} bIndex {} pi [expr asin(1)*2] paneY {} stackVar 0 lVar {} lId {} lPos {} lY {} tVar {} IconEnd "\ud83d\udccc"

set Cursor $BlockCursor

namespace eval mark {
	variable once 1 x 0 y 0 a 0 b 0

        proc moveonce {x y a b} {
        	
        	if {$mark::once} {
        	;#puts "uuuuuuuuuuu $x $y $a $b"
        	mark::move $x $y $a $b
        	set mark::once 0}
        }
        proc move {{x 0} {y 0} {a 0} {b 0}} {
        	;#puts "MOOOOOOOOOOOOOOVED $x $y $a $b"
        	${::cc}top move all $x $y
        	${::cc}left move all $a $b
		
        }
	proc fill {{x 0} {y 0} {a 0} {b 0}} {
		set mark::x $x
		set mark::y $y
		set mark::a $a
		set mark::b $b}
}

proc deg_to_rad {input {opposite ""}} {
	set unit [expr 2*$::pi/360]
	
	if {$opposite ne {}} {set unit [expr 1/$unit]}
	
	return [expr $input*$unit]
}
proc polar_to_rect {angle_deg_input {r 1} {origin {0 0}} {dontConvertToRad false}} {
	
	set input [expr {$dontConvertToRad ? $angle_deg_input : [deg_to_rad $angle_deg_input] }]
	
	set x [expr $r*cos($input)+[lindex $origin 0]]
	set y [expr $r*sin($input)+[lindex $origin 1]]
	
	return [list $x $y]
	
}

proc put1 {args} {
	puts =$args=
}
proc get_args {List args} {
	set r [list]
	
	foreach v $args {
		set i [lsearch $List $v]
		if {$i != -1} {
			lappend r [lindex $List $i+1]
		}
	}
	return $r
}
proc get_args2 {List args} {
	set r [list]
	set count 0
	foreach v $List {
		
		set len [string length $v]
		foreach newv $args { incr count ; if [string equal -length $len $v $newv] {  lappend r [lindex $List $count]  }  }
		}
	return $r
}
proc new_args {List args} {
	
	set toreturn [list]
;#puts "List->$List<-
;#ARGS->$args<-"
	if {[lindex $List end-1] eq {-XDefaults}} {set def [lindex $List end] ; set List [lreplace $List end end] ; set List [lreplace $List end end] } else {set def ""}
;#	puts ""
;#	puts "List->$List<-
;#ARGS->$args<-
;#DEF->$def<-"
	set count 0
	foreach v $args {
		set found [lsearch $List $v ]
		if {$found != {-1}} { lappend toreturn [lindex $List $found+1]  } elseif {$def ne ""} { lappend toreturn [lindex $def $count] }
	
		incr count
	}
	
;#puts "
;#TORETURN->$toreturn<-"
	return $toreturn
}
proc new_args_count {List args} {
	
	set toreturn [list]
	;#puts "$List // $args"
	if {[lindex $List end-1] eq {-XDefaults}} {set def [lindex $List end] ; set List [lreplace $List end end] ; set List [lreplace $List end end] } else {set def ""}
	;#puts "$List // $args // $def"
	set count 0
	foreach v $args {
		set found [lsearch $List $v ]
		if {$found != {-1}} { lappend toreturn [lindex $List $found+1] [plus $found 1]  } elseif {$def ne ""} { lappend toreturn [lindex $def $count] $found }
	
		incr count
	}
	
	return $toreturn
}

proc polar_to_rect2 {args} {
	
;#angle_deg_input {r 1} {origin {0 0}} {dontConvertToRad false}
	
	lappend args -XDefaults {0 1 {0 0} false}
	
	set r [new_args $args -angle -radius -shift -dontConvertToRad  ]
;#put $r
	
	lassign $r angle radius shift dontConvertToRad ; set r $radius
	
;#puts "ANGLE>$angle<
;#RADIUS>$radius<
;#SHIFT>$shift<
;#DONT>$dontConvertToRad<
;#"
	
	lassign $shift xshift yshift
;#put $xshift $yshift
	
	set input [expr {$dontConvertToRad ? $angle : [deg_to_rad $angle] }]
	
	set x [expr $r*cos($input)+$xshift]
	set y [expr $r*sin($input)+$yshift] ; set y -$y
	
	return [list $x $y]
	
}

proc polygon {args} {
	;#put ARGS $args
	set output [list]
	
	set count 0
	foreach i  $args {
		incr count ; set from 0
		foreach v [split $i ,] {
			puts >$v<
			set t [list [string first + $v] [string first - $v]]
			set sign [expr max([string first + $v], [string first - $v])]
			set end [expr {$sign == {-1} ? [string length $v] :  $sign }]
			
			
			set number {}
			set hash [string first # $v]
			if {$hash != {-1}} {
			set number [lindex $output "[expr [string range $v $hash+1 $end-1]]" ] } elseif {$sign != {-1}} { set number [lindex $output "end-1" ] } else {set number $v}

			set FSIGN +
			set add 0
			if {$sign != {-1}} { set add [string range $v $sign+1 end] ; set FSIGN [string index $v $sign] }  
			
			;#put -no $number $add ### "$number $FSIGN $add" ### [expr "$number $FSIGN $add"]
			lappend output [expr "$number $FSIGN $add"]
			
			
			
			
			set from 2;
		}
		puts ""
	
	}
	return $output
	}
proc parallel {args} {

	set dangle [get_args2 $args -dangle]
	set h [get_args2 $args -horizontal]
	set v [get_args2 $args -vertical]
	set origin [get_args2 $args -origin]
	
	if {[string length $origin] < 2} {set origin 0,0}
	
	set origin [split $origin ,]
	
	set r [lrange $origin 0 end ]
	lappend r {*}[polar_to_rect $dangle $v]
	
	lset r 2 [expr "[lindex $r 2]+[lindex $r 0]"]
	lset r 3 [expr "[lindex $r 3]+[lindex $r 1]"]
	
	set rr  [lrange $r 0 end]
	
	lset rr 0 [expr "[lindex $rr 0]+$h"]
	;#lset rr 1 [expr "[lindex $rr 1]+$h"]
	lset rr 2 [expr "[lindex $rr 2]+$h"]
	;#lset rr 3 [expr "[lindex $rr 3]+$h"]
	
	set upper [lrange $rr 0 1]
	set lower [lrange $rr 2 3]
	set rr "$lower $upper"
	
	
	
	;#put $r || $rr
	
	return "$r $rr"
}
#proc parallel {args} {parallelogram {*}$args}
proc change_font {args} {
	if {$::cFont eq {}} {
		
		set ::cFont [font actual TkDefaultFont -displayof .pane.main.canvas] ; ;#put $::cFont
		set ::cSize [get_args $::cFont -size]
	} else {
		#put cSize $::cSize
		set ::cSize [expr int($::cSize[lindex $args end]) ]
	}
	set ::cDim [.pane.main.canvas bbox TEXT]
	.pane.main.canvas itemconfig TEXT -font "-size $::cSize"
	.pane.main.canvas itemconfig BLOCK_CURSOR -font "-size $::cSize"
	update_hoverline
	return $::cSize
}
# **Stack** #
button .left -text \ud83e\udc44 -font {-size 16}
button .rght -text \ud83e\udc46 -font {-size 16}


proc stack_things {args} {
	
	
	if [set ::stackVar [expr !$::stackVar]] {
		place configure .pane -y 100 
		
	} else {
		place configure .pane -y $::paneY
	}
}


# Status Bar
set s [label .0label -relief sunken -borderwidth 2 -text ""]

#Checkbutton
proc Reliefbutton {name args} {
	
	#puts stdout "(Reliefbutton) >$name< >$args<"
	set operation [lindex $args 0]
	
	switch $operation {
		ison {
			return [expr {[$name cget -relief] eq {sunken}}]
		}
		isoff {
			return [expr {[$name cget -relief] eq {groove}}]
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

proc TEXThover0 {args} {
	#put TEXThover $args $::cDim
	variable x [lindex $args 0] y [lindex $args 1]
	if { $y+5 >= [lindex $::cDim 3] || $x >= [lindex $::cDim 2] } {put Nope ; return}
	set ::tIndex [.pane.main.canvas index TEXT @$x,$y]
	set i $::tIndex
	
	if {$::bCursor == {}} {
		.pane.main.canvas insert TEXT $i $::Cursor
		set ::bIndex $i
		set ::bCursor Set
		return
	}
	#.pane.main.canvas imove TEXT $::bIndex $x $y
	.pane.main.canvas dchars TEXT $::bIndex
	.pane.main.canvas insert TEXT $i $::Cursor
	
	set ::bIndex $i
	#puts $::tIndex
}
proc TEXThover {c x y h} {
	
	set now [$c type current]
	set info [$c bbox current] ; if { $info eq {} } { return } 
	lassign $info bx by bw bh 
	put $x $y <> $info 
	if { $y+5 >= $bh || $x >= $bw } {put Nope ; 

	if {$::bCursor ne {}} { $c dchars current $::bIndex } ; return}

	set ::tIndex [$c index current @$x,$y] 
	set i $::tIndex
	
	if {$::bCursor eq {}} {
		$c insert current $i $::Cursor
		set ::bIndex $i
		set ::bCursor Set
		return
	}
	#.pane.main.canvas imove TEXT $::bIndex $x $y
	$c dchars current $::bIndex
	$c insert current $i $::Cursor
	
	set ::bIndex $i
	#puts $::tIndex
}
proc opaque_canvas {W} {
	.pane.main.canvas itemconfig B -fill [lindex "white [.pane.main.canvas cget -bg]" [Reliefbutton $W ison]]
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
proc put_scrolls {args} {
	
	set control [get_args2 $args -control]
	set put [get_args2 $args -put]
	set xargs [get_args2 $args -xargs]
	set yargs [get_args2 $args -yargs]
	set Controlargs [get_args2 $args -Controlargs]
	set xplace [get_args2 $args -xplace]
	set yplace [get_args2 $args -yplace]
	
	scrollbar $put.scrollx -orient horiz -relief sunken -command "$control xview" {*}$xargs
	scrollbar $put.scrolly -orient vert -relief sunken -command "$control yview" {*}$yargs
	$control config -xscrollcommand "$put.scrollx set" -yscrollcommand "$put.scrolly set" {*}$Controlargs
	eval {*}$xplace
	eval {*}$yplace
	
}
proc separator {name {type ""} args} {
	set config {-state disabled -text {} -relief flat}
	switch -glob $type {
		
		button { button $name {*}$config }
		horiz* { ttk:separator $name -orient horizontal } 
		vert* { ttk:separator $name -orient vertical }
		default { label $name {*}$config }
	}
	set c 0 
	switch -glob [lindex $args 0] {
		-pack {incr c ; pack $name -expand 0 -padx 1 -pady 1 -side left}
		-* { incr c ; eval [lrange $args c-1 c] }
	}

}
set bbVar 0
proc create_text {c x y k a} {
	set g [$c index BLOCK_CURSOR insert]
	switch $k {
		
		BackSpace {
			if {$g} {
			$c dchars BLOCK_CURSOR [expr $g - 1]
			incr ::bbVar -1
			}
		}
		Left {
			if {$g} {
			put [expr $g - 1]
			
			$c icursor BLOCK_CURSOR [incr ::bbVar -1]
			;#$c dchars BLOCK_CURSOR [expr $::bbVar + 1]
			;#$c insert BLOCK_CURSOR insert $::Cursor
			
			}
		}
		Return {
			set id [$c find withtag BLOCK_CURSOR]
			;#$c dchars BLOCK_CURSOR $::bbVar
			;#$c icursor BLOCK_CURSOR end
			$c insert BLOCK_CURSOR insert $::IconEnd
			;#$c rchars BLOCK_CURSOR $::bbVar [expr $::bbVar + 1] $::IconEnd
			$c itemconfig $id -tag TEXT
			$c create text $x $y -anchor w  -tag BLOCK_CURSOR
		}
		default {
			$c insert BLOCK_CURSOR end $a
			incr ::bbVar
			
		}
	}
	
	return
	set r [ $c find overlapping $x $y [expr $x + [font measure TkDefaultFont -displayof $c $::Cursor] ] $y ]
	;#put $r
	foreach v $r { if { [$c type $v] eq {text} } { set r $v ; break } }
	;#put $r
	if { $r eq {} } {
	$c create text $x $y -tag TEXT -anchor w -text $k
	}
	
}

set whocalled {}
set whoobject {}
set whataction {}
oo::class create SingleTab { 
	
	variable txt myframe doc dot pages parent Position ;#...
	
	constructor {tempcount passed_parent {temptxt ""}} {

		set pages 0
		set txt $temptxt ;
		
		set parent $passed_parent
		set myframe [frame $parent.myframe$tempcount -relief groove]
		set com "[self] clicked"
		#Document
		set doc [button $myframe.doc -text [expr { $temptxt eq {} ? "Blank Document #$tempcount" : $temptxt }] -relief groove -cursor hand2 -command $com]
		set dot [button $myframe.dot -relief flat -text {...} -cursor hand2]
		
		
		grid $doc -column 0 -row $pages -sticky nsew
		grid $dot -column 1 -row $pages -sticky nsew
		
		
		my newpage
		
		grid $myframe -column 0 -row $tempcount -sticky nsew {*}[if {$tempcount > 1 } { subst {-pady 0.5cm}}]
		
		#after 1000 "[self] clicked"
		#my clicked
	}
	method newpage { {at {} } { textPassed {}}} {
		
		incr pages
		
		set p [button $myframe.page$pages -text [expr {$textPassed eq {} ? "Page $pages" : $textPassed}] -relief groove]
		set pdot [button $myframe.dot$pages -relief flat -text {...} -cursor hand2];# -command "postgeneric $myframe.dot$pages .mPage"]
		
		set sub ""
		set sub1 ""
		bind $pdot <ButtonRelease> " set ::whocalled $p ; set ::whoobject [self] ; postgeneric $pdot .mPage" 
		
		if {$at eq {}} {
			set at $pages
		}
		grid $p -column 0 -row $at -padx 0.2cm
		grid $pdot -column 1 -row $at
	}
	 method get {} {
	 	return [list $doc $dot]
	 }
	 method remove {} {
	 	
	 }
	 method info {who args} {
		puts who=>$who
		set det [grid info $who] ; #details
		return [lmap i $args {subst [dict get $det $i]}]
	 }
	 method rename {who} {
		
		set name [string trimleft $who [dict get $det -in]] ; puts Name|$name
		set row [dict get $det -row ]
		set col [dict get $det -column]
		set textW [entry $myframe.textW -bg [. cget -bg] -text $name]
		grid forget $who
		grid $textW -row $row -column $col -sticky nswe
		return
	 }
	 method clone {who} { ; #whocalled #.pane.tabs.myframe1.dot1
		lassign [my info $who -row -column] row col
		puts who=>$who
		
		foreach x [grid slaves $myframe] {set oldRow  [dict get [grid info $x] -row]  ; if {$oldRow <= $row} {continue} ;  grid configure $x -row [expr 1+$oldRow] ; puts U }
		my newpage [expr {1 + $row}] "Clone of ([$who cget -text])"
	 }
	 method up {who} {
		lassign [my info $who -row -column] row col
		foreach x [grid slaves $myframe] {set oldRow  [dict get [grid info $x] -row] ; puts "oldElement $x row $oldRow Reference $row";  if {$oldRow < $row} {continue} ; grid configure $x -row [expr 1+$oldRow] }
			my newpage [expr {$row}] 
	 }
	 method down {who} {
		lassign [my info $who -row -column] row col
		foreach x [grid slaves $myframe] {set oldRow  [dict get [grid info $x] -row] ; puts "oldElement $x row $oldRow Reference $row";  if {$oldRow <= $row} {continue} ; grid configure $x -row [expr 1+$oldRow] }
			my newpage [expr {1 + $row}] 
	 }
	 method clicked {} {
	 	
	 	#
	 }

}
proc rotate {args} { 
	lappend args -XDefaults {"" 0 all}
	set got [new_args $args -c -angle -id] ;#tagOrID
	lassign $got c angle id
	;#put GOT $got
	set send [list]
	
	set target [$c coords $id]
	for {set i 0 ; set len [llength $target] } {$i < $len} {incr i 2} {
		
		set x [lindex $target $i]
		set y [lindex $target $i+1]
		lappend send [expr { $x * cos($angle)} - {$y * sin($angle) }]
		lappend send [expr { $x * sin($angle)} + {$y * cos($angle) }]
	}
	$c coords $id {*}$send
}
proc triadd {args} { 
	lappend args -XDefaults {"" {0 0} all}
	set got [new_args $args -c -shift -id] ; set send [list] ;#tagOrID
	lassign $got c shift id
	lassign $shift addx addy
	set target [$c coords $id]
	for {set i 0 ; set len [llength $target] } {$i < $len} {incr i 2} {
		set x [lindex $target $i]
		set y [lindex $target $i+1]
		lappend send [expr {$x} + {$addx}]
		lappend send [expr {$y} + {$addy}]
	}
	$c coords $id {*}$send
}
proc triangle {args} {
	lappend args -XDefaults {{0 0} {60 60} 50 TRIANGLE .pane.main.canvas}
	set temp [new_args $args -xy -angles -radius -tag -c]
	;#put $temp
	lassign $temp ox langle radius tag c
	lassign $ox ox oy
	lassign $langle langle rangle
	;#put tag $tag
	set langle [minus 180 $langle] 	;#radius
	
	lassign [polar_to_rect2 -angle $langle -radius $radius -shift {0 0}] a b
	lassign [polar_to_rect2 -angle $rangle -radius $radius -shift {0 0}] x y
	
	set adj2 [expr 2*cos([deg_to_rad $rangle])*[expr sqrt($x**2+$y**2)]]
	
	;#$c create line   -fill red -width 5 -tag TRIANGLE
	;#$c create line 0 0 $x $y $adj2 0 [plus $adj2 $a] $b -fill blue -width 5 -tag TRIANGLE
	set tor [$c create line $ox $oy $adj2 $oy $x $y $ox $oy -fill blue -width 2 -tag $tag]
	;#B $c create line 0 0 $adj2 0 -fill blue -width 5 -tag TRIANGLE
	return $tor
}

proc update_hoverline {} {
	set s $::cSize ; set ::lVar $s 
	set all [$::cc find withtag LINE]
	set f [lindex $all 0] 
	
	
	lassign [$::cc bbox B] bx by bw bh ; incr bx 2
	
	set many [expr {$bh / $s}] ;
	for {set i 0 ; set count 1} {$count < $many} {incr i $s; incr count 1} {
		$::cc moveto [lindex $all $count] $bx $i
	}
	
}

proc hoverline {c {x ""} {y ""} args} {
	
	if {$x eq "" } {
		set f [font metrics TkDefaultFont] ; set s [lsearch -glob $f -linespace] ; set s [lindex $f $s+1]
		incr s 5
		set ::lVar $s ; lassign [$c bbox B] bx by bw bh ; #put ABA [$c bbox B]
		incr bx 2
		
		set many [expr {$bh} / $s] ; set count -1 ; set i $by ; incr i $s ; while {[incr count] < $many} { 
			#put 
			$c create line $bx $i $bw $i -width 2 -dash _ -fill {} -tag LINE ; incr i $s }
		;#$c bind B <Motion> {hoverline %W %x %y}
		$c create text $bx $by -text {} -anchor w -fill {} -tag BLOCK_CURSOR
		;#$c bind B <Enter> {%W config -cursor none}
		;#$c bind B <Leave> {%W config -cursor arrow}
		return
	}
	;#set x [$c canvasx $_x] ; set y [$c canvasy $_y] 
	;#$put many $many
	$c itemconfig LINE -fill {}
	;#set next [$c find closest $x $y $::lVar B]
	;#set next [$c find closest $x $y $::lVar ]
	;#set next [$c index LINE @$x,$y]
	set next [$c find overlapping $x $y [expr $x+1] [expr $y+$::lVar] ]
	foreach v $next { if {[$c type $v] eq {line}} {set next $v ; break} }
	;#put next $next
	
	if {[$c type $next] eq {line}} {$c itemconfig $next -fill gray ;
		set ::lId $next ; set ::lPos [$c bbox $next] ; set ::lY [lindex $::lPos 1]
		$c config -cursor hand1
		$c itemconfig BLOCK_CURSOR -fill black
		$c moveto BLOCK_CURSOR $x [expr $::lY - $::lVar + 5]
		$c focus BLOCK_CURSOR
		
	}
}


proc ruler {top left} {
	set ct .pane.main.canvastop
	set c .pane.main.canvas
	set cl .pane.main.canvasleft
	
	lassign $top startx  w 
	lassign $left starty  h 
	
	set pad [$c cget -highlightthickness]
	
	set xpad $pad
	set ypad [expr $pad+5]
	
	set tickLength [expr ($pad+1)*2]
	
	$ct create line 0 0 400c 0 -width 5 -fill black -tag topline
	$ct create text 15 [expr $ypad+11] -text cm -anchor n
	$cl create text [expr $ypad+11+10] 0 -text cm -anchor w
	$cl create line 0 0 0 400c -width 5 -fill black -tag leftline ;# rename zeroline
	
	
	$c create line 0 0 1cm 0 -fill {} -tag cm
	set cm [lindex [$c bbox cm] 2] ;#length
	set tenth [expr $cm/10.0]
	

	
	for {set x $startx; set end 400} {$x < $end} {incr x} {
		#puts "x=$x end=$end" 
		$ct create line ${x}c $xpad ${x}c [expr $ypad+11]
		$ct create text ${x}c  [expr $ypad+11] -anchor n -text $x
		
		set count 0
		while {[incr count] < 10} {
			;#puts "count=$count x=$x should be less than 10" 
			$ct create line ${x}.${count}c $xpad ${x}.${count}c [expr $ypad+$count] }
	}
	for {set x $startx; set end 400} {$x < $end} {incr x} {
		#puts "x=$x end=$end" 
		$cl create line 0 ${x}c [expr $ypad+11] ${x}c
		$cl create text  [expr $ypad+11] ${x}c -anchor w -text $x
		
		set count 0
		while {[incr count] < 10} {
			;#puts "count=$count x=$x should be less than 10" 
			$cl create line 0 ${x}.${count}c [expr $ypad+$count] ${x}.${count}c  }
	}
	

}

proc ruler2 {startx w starty h} {
	
	set ct .pane.main.canvastop
	set c .pane.main.canvas
	set cl .pane.main.canvasleft
	
	set padtop [$ct cget -highlightthickness]
	set padleft [$cl cget -highlightthickness]
	
	set pad1 [expr { $padtop+11 }]
	set pad2 [expr { $padleft+11+5 }] ;#+5
	
	$c create line 0 0 1cm 0 -fill {} -tag cm
	set cm [lindex [$c bbox cm] 2] ;#length
	set tenth [expr $cm/10.0]
	
	set w [expr {int(ceil(2*$w / $cm))}]
	set h [expr {int(ceil(2*$h / $cm))}]
	set fill gray
	
	#set bunch [ list ] ; while {[incr count] < 10} {lappend bunch $count} ;# The Dot before count is essential.
	#set bunch [range 0 10] ; set ex [range -$w $w] ; lset ex [lsearch $ex 0] {-0 0} ; set ex [join $ex]
	
	set count 	10
	set i 		0
	set bunch1 [list] ; while {$i < $count} { lappend bunch1 $i ; incr i}
	set bunch2  [lmap i [list $count {*}[lrange $bunch1 1 end]] {subst {[expr $padtop+$i+4]}}]
	
	set bunch1 [lmap i $bunch1 {subst .${i}c}]

	$ct create line 0  $padtop ${w}c $padtop -width 2 -fill black -tag {topline toplineRight}
	$ct create line -${w}c  $padtop 0 $padtop -width 2 -fill black -tag {topline toplineLeft} -fill $fill
	$cl create line $padleft 0 $padleft ${h}c  -width 2 -fill black -tag {leftline leftlineRight}
	$cl create line $padleft -${h}c $padleft 0 -width 2 -fill black -tag {leftline leftlineLeft} -fill $fill
	
	for {set i 0} {$i < $w} {incr i} {
		
		foreach j $bunch1 k [list [lindex $bunch2 0] {*}[lreverse [lrange $bunch2 1 end ]]] {
			
			$ct create line -$i$j $padtop -$i$j $k -fill $fill -tag "-$j -$i$j"
			$cl create line $padleft  -$i$j  $k -$i$j -fill $fill -tag "-$j -$i$j"
		}
		$ct create text -${i}c  $pad1 -anchor n -text -$i -fill $fill -tag -$i
		$cl create text  $pad2 -${i}c  -anchor w -text -$i -fill $fill -tag -$i
	}
	
	for {set i 0} {$i < $w} {incr i} {
		
		foreach j $bunch1 k $bunch2 {
			
			$ct create line $i$j $padtop $i$j $k -tag "$j $i$j"
			$cl create line $padleft  $i$j  $k $i$j -tag "$j $i$j"
		}
		$ct create text ${i}c  $pad1 -anchor n -text $i -tag +$i
		$cl create text  $pad2 ${i}c  -anchor w -text $i -tag +$i
	}
	
	$ct delete -0
	$cl delete -0
}
namespace eval small_triangle {
	set avalh 0 ; #avialable height (with a small amount sybtracted from for the text below)
	set yzero 0 ; #y coord of 0 |  (tick)
	set ylabvar {}
proc create {} {
	set ct .pane.main.canvastop
	set c .pane.main.canvas
	set cl .pane.main.canvasleft
	
	#Unused
	set wct [$ct cget -width]
	set hct [$ct cget -height]
	
	set wcl [$cl cget -width]
	set hcl [$cl cget -height]
	
	#set zero [$ct bbox +0] ; # NOT REliable +3 Difference 
	set zero [$ct coords .0c] ; #ycoord of |
	
	set yzero [expr [lindex $zero 3]]
	set xzero [lindex $zero 0]
	
	set h [expr $hct-$yzero]
	set small_triangle::yzero $yzero 
	set small_triangle::avalh [expr $h] ; #space for text beneath
	
	set dim [Shape triangle -height $h ]
	$ct create text $xzero [expr $yzero+15] -anchor n -tag small_traingle_text
	$ct moveto [$ct create polygon $dim -tag small_triangle_tag] $xzero $yzero
	
}
proc update {gotx} {
	set c .pane.main.canvas
	set ct .pane.main.canvastop
	set cl .pane.main.canvasleft
	set dx [expr [winfo x $c]-[winfo x $ct]]
	set dy [expr [winfo y $c]-[winfo y $cl]]
	
	
	set x [$ct canvasx $gotx]
	set div [expr $x/49.0]
	#puts =$x=$div=
	set origpart [string range $div 0[string first . $div] end]
	puts ORIGPART=>$origpart
	puts H=>[set h $small_triangle::avalh]
	
	set mh [expr {$h * $origpart}]
	set part [expr 1-$origpart]
	puts PART=>$part
	puts ANGLE=>[set a [expr $part*90]] ; #puts "$a Deg"
	
	
	
	
	puts MH=>$mh
	#Duplicate code here. from Shape->Update->some if
	puts RADANGLE=>[set angle [Angle2 -rad    $a]]
	puts S9=>[set s9 [expr {abs(tan($a) / $mh)}]]
	
	puts S1=>[set s1 [expr { sqrt($s9**2 + $h**2) }]]
	
	#puts NEWY=>[set newy [expr $::small_triangle::avalh+$::small_triangle::yzero]]
	#set newy [expr -$newy]
	
	#set box [list [expr $x-$s9] $newy $p1x $p1y [expr $x+$s9] $newy ]
	set box [Shape triangle -angles $a $a -sides $s1]
	$ct coords small_triangle_tag $box
	#puts BOX=>$box
	lassign [$ct bbox small_triangle_tag] nx ny nw nh
	
	puts "$nx $ny $nw $nh"
	if {$nw eq {}} {set nw 0; set nh 0}
	$ct itemconfig small_traingle_text -text [string range $origpart 0 2]
	$ct moveto small_triangle_tag [expr $x+$dx-[expr {$nw / 2.0}]] [expr -15+$::small_triangle::yzero+$mh]
	$ct moveto small_traingle_text [expr $x+$dx] 30
}	
}
oo::class create Tabs { 
	
	variable fcount newcount lobj sobj m mc 		t tc
	set cm {}
	
	constructor {} {
	
	my create_main
	#Document create document$fcount
	
	set t [labelframe .pane.tabs -text {Current Tabs} -relief ridge -bd 5]
	
	#set tc [canvas $t.canvas]
	set com "[self] create {}"
	#set com {Document create document$dcoun}
	
	button $t.add -text "\ud83d\uddcb Create New Document" -relief groove -command $com
	ttk::separator $t.end -orient horizontal
	
	set fcount 0
	set newcount 0
	#pack $tc -fill both -side top
	
	#pack $t.add -fill x -pady 0.05in -side top
	#pack $t.end -fill x -pady 0.05in -side bottom
	
	
	grid $t.add -column 0 -row $newcount -columnspan 2 -sticky nsew 
	#place .tabs -relx 0.66 -y 0.4in -relwidth 0.3 -relheight 1
	
	
	}
	
	method create_main {} {
		set m [labelframe .pane.main -relief groove -bd 5]
		set tbar [frame .pane.main.toolbar -relief groove -bd 2]
		set mc [canvas $m.canvas -highlightbackground green]
		#place .main -relx 0.34 -y 0.4in -relwidth 0.3 -relheight 1
		
		set com "$mc configure -scrollregion  [$mc bbox all]"
		bind $mc <Gravity> "puts 123"
		set pad [$m.canvas cget -highlightthickness]
		;#$mc create text [expr 0+$pad] [expr 0+$pad] -text {INITIAL TEXT} -tag TEXT -anchor nw
		
		bind $m <Configure> $com
		;#$mc bind TEXT <Motion> {TEXThover %W [%W canvasx %x] [%W canvasy %y] %h}
		put_scrolls -control $mc -put .pane.main -xplace {pack $put.scrollx -side bottom -fill x } -yplace {pack $put.scrolly -side right -fill y }
		pack $tbar -side top -expand 0 -fill x -pady 5
		pack [canvas .pane.main.canvastop -highlightthickness 2  -height 1c] -side top -after $tbar -fill x
		 
		pack [canvas .pane.main.canvasleft -highlightthickness 2 -width 1c] -side left -fill y
		pack [frame .pane.main.tools ] -side left -fill y
		pack $mc -expand 1 -fill both -side bottom
		my fill_canvas_toolbar
		my draw_document
		my make_ruler
		hoverline $mc ;# put $ox $oy $w $h <> $x $y; ; put OUT
		bind $mc <Motion> { lassign [%W bbox B] ox oy w h ; set x [%W canvasx %x] ; set y [%W canvasy %y] ; if { $x  >= $ox && $x <= [expr $ox + $w] && $y >= $oy && $y <= [expr $oy + $h]} { hoverline %W $x $y } else {%W config -cursor arrow ; %W itemconfig LINE -fill {} ; %W focus ""} }
		$mc bind BLOCK_CURSOR <Key> { set x [%W canvasx %x] ; set y [%W canvasy %y] ; create_text %W $x $y %K %A}
		;#my triangle_tick
		focus $mc
		
		;#pack [label .pane.main.tools.title -text Tools -relief groove] -pady 5 -padx 5 -expand 0 -fill x
		;#pack [Reliefbutton .pane.main.tools.textd -text {Text Directed} -relief groove] -pady 5 -padx 5 -expand 1 -fill both
		;#pack [Reliefbutton .pane.main.tools.lineh -text {Horizontal Line} -relief groove] -pady 5 -padx 5 -expand 1 -fill both
		;#bind_Reliefbutton .pane.main.tools.lineh lineh_pdf
	}
	
	method create {txt} {
		
		incr newcount
		set new [SingleTab new [expr $fcount+$newcount] $t $txt]
		#lappend lobj $new
		lassign [$new get] b dot
		#pack [$b get] -fill x -pady 0.05in
		
		
		
		
		#switch $txt {} {incr newcount} default {incr fcount}
		
	}
	
	method width_changed {w} {
		set old [$mc itemcget TEXT -width] 
		set new [winfo width $mc]
		#puts "<configure event> old canvas TEXT width $old new $new"
		$mc itemconfigure TEXT -width $new
		$w config -scrollregion [$w bbox all]
		
	}
	method fill_canvas_toolbar {} {
		set c .pane.main.canvas
		
		set tbar $m.toolbar
		pack [Reliefbutton $tbar.seethrough -text "$::IconSeethrough Darken Document" -relief groove] -side left -expand 0 -padx 1
		bind_Reliefbutton $tbar.seethrough opaque_canvas
		
		separator $tbar.separator1 label -pack
		
		pack [button $tbar.enlarge -text "$::IconFontIncrease Enalrge Text" -relief groove -command "change_font +1"] -side left -expand 0 -padx 1
		pack [button $tbar.ensmall -text "$::IconFontDecrease Ensmall Text" -relief groove -command "change_font -1"] -side left -expand 0 -padx 1
		
		separator $tbar.separator2 label -pack
		
		pack [button $tbar.zoomin -text "$::IconPlus $::IconZoom Zoom In" -relief groove -command {.pane.main.canvas scale all [expr [.pane.main.canvas cget -width]/2] [expr [.pane.main.canvas cget -height]/2] 2 2 ; 
			.pane.main.toolbar.enlarge invoke
		; change_font *2 }] -side left -expand 0 -padx 1
		pack [button $tbar.zoomout -text "$::IconMinus $::IconZoom Zoom Out" -relief groove -command { .pane.main.canvas scale all [expr [.pane.main.canvas cget -width]/2] [expr [.pane.main.canvas cget -height]/2] .5 .5 ;
			.pane.main.toolbar.ensmall invoke
		; change_font *0.5 }] -side left -expand 0 -padx 1
		
		separator $tbar.separator3 label -pack
		
		bind $c <ButtonPress> "[self] mark %W %x %y"
		bind $c <B1-Motion> "[self] dragto %W %x %y"
		bind $c <MouseWheel> "[self] mouse_wheel %x %y %D"
	}
	method mark {c x y} {
		$c scan mark $x $y
		set ct [subst $c]top
		set cl [subst $c]left
		$ct scan mark $x 0
		$cl scan mark 0 $y
		;#mark::from $x $y
	}
	method dragto {c x y} {
		$c scan dragto $x $y 1
		
		set ct [subst $c]top
		set cl [subst $c]left
		$ct scan dragto $x 0 1
		$cl scan dragto 0 $y 1
		;#mark::to $x $y
		;#puts "FINAL ->   $x,$y"
		;#
	}
	method draw_document {} {
		set c .pane.main.canvas
		#put [join [.pane.main.canvas config] \n]
		#put [join [winfo reqheight .pane.main.canvas ] \n]
		
		set w [$c cget -width]
		set h [$c cget -height]
		set pad 15
		
		$c create polygon [parallel -dangle [expr 180-45] -h 250 -v 10 -orig 20,17] -outline black -fill black -tag BOX
		;#$c create polygon [parallel -dangle [expr 90] -h 250 -v 10 -orig 20,17] -outline black -fill black
		
		$c create rectangle [polygon 250,18 +20,+402] -outline black -fill black -tag BOX
		$c create rectangle [polygon 10,20 +250,+400] -outline black -fill white -tag {BOX B}
		$c move BOX -10 -17
		;#bind $m <Configure> {+
		;#	set w [winfo width %W]
		;#	%W.canvas moveto BOX [expr ($w-[lindex [%W.canvas bbox BOX] 2])/2+40 ] 40}
			
	}
	method mouse_wheel {x y d} {
		puts "MouseWheel $x $y $d"
		set s [string index $d 0]
		if {$s eq {+}} {.pane.main.canvas scale all $x $y 2 2 ; puts ++++ } else {.pane.main.canvas scale all $x $y 0.5 0.5 ; puts --------}
		
	}
	
	method make_ruler {} {
		
		#ruler "0 [$::cc cget -width]" "0 [$::cc cget -height]"
		ruler2 0 [$::cc cget -width] 0 [$::cc cget -height]
		small_triangle::create
	}
	method triangle_tick {} {
		set c .pane.main.canvas
		set ct .pane.main.canvastop
		
		set t [triangle -angles {60 60} -radius 1 -tag tri -c $ct -xy "50 [$ct cget -height]"]
		$ct itemconfig $t -width 1
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
set f [listbox $a.1list -relief flat -highlightthickness 2 -highlightcolor red -cursor hand2 -activestyle dotbox -bg [. cget -bg] -listvar fVar -justify center ]

proc distribute_scroll {things args} { foreach v $things { $v {*}$args } }
variable g [scrollbar $a.0scroll -orient vertical -command "distribute_scroll {$e $f} yview"] h [scrollbar $a.1scroll -orient horizontal -command "$e xview"]


$e config -xscrollcommand "$h set" -yscrollcommand  "$g set"
$f config -xscrollcommand "$h set" -yscrollcommand  "$g set"
# Pack "Items in current directory"

#pack $s -side bottom -fill x
#pack $a -side left -expand false -fill y
#place $a -relx 0.02 -y 0.4in -relwidth 0.3 -relheight 1 
pack $d -fill x 				; 
pack $buttonsBar -fill x			; 
pack $b $c $j  -side right -anchor ne -padx 5 	
foreach v {$b $c $j} {[subst $v] config -relief groove}
pack $h  -side bottom  -fill x
pack $dd -fill x
pack $f -side left -expand 0 -fill y
pack $e -side left -expand 1 -fill both
pack $g -side left -fill y
#pack config $j -padx 0

# ***Toolbar Button*** #
proc ToolbarButton {args} {
	# puts "** args are -> $args"
	set result [button {*}$args -relief flat]
	bind [lindex $args 0] <Enter> {buttonhover %W}
	bind [lindex $args 0] <Leave> {buttonleave %W}
	return $result
}



set OBJ 0
set OBJTABLE [dict create]


proc incrobj {{by 1}} {
 return [incr ::OBJ $by]
} 
proc dictlincr {d key index  {by 1} } {
 upvar $d x
 set l [dict get $x $key]
 dict set x $key [lreplace $l $index $index [expr [lindex $l $index]+$by ]]
 concat
}

namespace eval save {
	set filter {}

	set exts [list [list {All Files} *] [list {PostScript Files} *.ps] [list {Portable Document Files} *.pdf]]

	proc pdf {args} {
		#set path 123
		set path [tk_getSaveFile -filetypes $save::exts -title {Save current Document as} -typevariable save::filter -defaultextension *.pdf]
		if {$path ne {}} {
			
			set x [PDF create -ref page]
			set y [PDF create -ref pages]
			set k [dict get $::OBJTABLE $y *thing /MediaBox]
		
			set z [PDF create -ref stream text -text {} -fontname /Font1 -fontsize 9] ; #-fontname Calibri
			PDF create -hasref dict /Len *8
			PDF update -hasref $x thing /Parent *$y /Contents *$z
			PDF update -hasref 6 thing 0 *1
			PDF update -hasref $y thing /Kids *6 /Count 1
			PDF update -inline 5 array  {*}[lmap i [$::cc coords B] {subst {[expr {int($i)}]}}]
			lassign [$::cc bbox B] bxx byy bww bhh
			foreach x [$::cc find withtag TEXT] {
				lassign [$::cc bbox $x] xx yy ww hh
				set yy [expr $bhh-$yy]
				PDF update -minimal $z stream add text -text [string range [$::cc itemcget $x -text] 0 end-2] -x $xx -y $yy
			}
			
			#puts [dict get $::OBJTABLE  5]
			PDF header
			PDF trailer
			set B [PDF create -ref catalog]
			set I [PDF create -ref info]
			PDF update -hasref end thing /Info *$I /Root *$B
			PDF update -hasref $B thing /Pages *$y
			PDF reftable
			#puts [PDF display]
			concat
			puts $path
			set out [open $path w]
			fconfigure $out -translation lf
			puts -nonewline $out [PDF display]
			close $out
			concat
		}
		
}
	 
	proc ps {} {	
		set path [tk_getSaveFile -filetypes $save::exts -title {Save current Document as} -typevariable save::filter]
		lassign [$::cc bbox B] bx by bw bh
		puts "x=$bx y=$by width=$bw height=$bh pagewidth=$bw pageheight=$bh"
		$::cc postscript -fontmap -*-Courier-Bold-R-Normal--*-120-* -file $path[save::inusefilter] -x $bx -y $by -pagey $bh -width $bw -height $bh -pageanchor c -pagewidth [expr 2*$bw]
	
}
	proc inusefilter {} {
		set i  [lsearch -index 0 $save::exts $save::filter]
		set in [lindex $save::exts $i 1] ;# intermediate {"PostScript Files" {*.ps}}
		set r [string range $in 1 end]
		;#puts "filter=$save::filter i1=$in i=$i search=$r<"
		return $r
	}
}

proc PDF {what args} {

	
	set s [list]
	set hasref 0
	set ref 0
	set Ret 0
	set inline 0 ; # inline does not mean -> IndexToChange Value.
	set minimal 0;
	
	if {[set __i [lsearch -glob $args -upvar?*]] != {-1}}	 {
		set __s [lindex $args $__i]
		set args [lreplace $args $__i $__i]
		
		foreach __i [lrange [split $__s .] 1 end] {
			upvar 1 $__i __$__i
		}
		unset __s
		} else {}
	unset __i
	
	
	set count 0
	foreach v $args {
		if [string match -?* $v]  {incr count ; lappend s [string range $v 1 end] } else {break}
	}
	set args [lrange $args $count end]
	foreach v $s { set $v 1 }
	
	
	if $hasref {
		set i [lsearch -glob -all $args \\**]
		set ia [lmap v $i { subst [lindex $args $v] }]
		set ia [lmap v $ia {subst {[string trimleft $v *] 0 R} } ]
		set ib [lmap v $i { subst [lindex $args $v-1] }]
		foreach v $i vv $ia {lset args $v $vv}
		}
		
		switch $what {
			create {

				lassign $args type
				set args [lrange $args 1 end]
				
				set x [dict create *type [list] *thing {} *length 0 *begin [list] *end [list] *refcount 0 *ref [list] ]
				
				if $hasref {
					dict set x *refcount [llength $i]
					dict set x *ref $ib
					#dict incr x *length  -[string length [join $ia {} ]]
					}
				
				switch $type {
					
					catalog {
						set x [PDF create dict /Type /Catalog /Pages x]
						dict set x *type catalog
					} info {
						set x [PDF create dict /Title {(A PDF File)} /Author {(Abdullah Fatota)} /Creator (RegularPDF) /Producer (RegularPDF)]
						dict set x *type info
					} dict {
						dict set x *type dict
						dict set x *thing [dict create {*}$args]
						dict lappend x *begin {<< }
						dict lappend x *end { >>}
						dict incr x *length [string length [join [dict get $x *thing] ]]
						dict incr x *length [string length [join [dict get $x *begin] {}]]
						dict incr x *length [string length [join [dict get $x *end] {}]]
					} array {
						dict set x *type array
						dict set x *thing [list {*}$args]
						dict lappend x *begin \[
						dict lappend x *end \]
						dict append x *count [llength [dict get $x *thing]]
						dict incr x *length [string length [join [dict get $x *thing] ]]
						dict incr x *length [string length [join [dict get $x *begin] {}]]
						dict incr x *length [string length [join [dict get $x *end] {}]]
					} stream {
						dict set x *type stream
						
						dict lappend x *end "\nendstream"
						dict set x *thing [dict create]
						dict append x *count 0
						dict append x *streammiddle "\n"
						
						lassign $args type
						set args [lrange $args 1 end]
						set got [new_args [concat $args -XDefaults {{Tesst 50 200 /Font1 9}}] -text -x -y -fontname -fontsize ]
						lassign $got text tx ty fontn fonts
						
						switch $type {
							text {
								dict incr x *count
								dict append x child_text[dict get $x *count] [list [list $fontn $fonts Tf] [list BT] [list $tx $ty Td] [list ($text) Tj ] [list ET] ]
							}
						}
						dict incr x *length [string length [join [join [dict values [dict filter $x key child_*] ] { } ] [dict get $x *streammiddle]]  ]
							if !$minimal {
								dict lappend x *begin "[PDF str [PDF create dict /Length [dict get $x *length] ] ]\n"
								dict lappend x *begin "stream\n"
								dict incr x *length [string length [join [dict get $x *begin] {}]]
								dict incr x *length [string length [join [dict get $x *end] {}]]
							}
						} pages {
						set font1 [PDF create -ref dict /Type /Font /Subtype /Type1 /Name /Font1 /BaseFont /Tahoma]
						set font2 [PDF create -hasref -ref dict /Font1 *$font1]
						set font3 [PDF create -ref -hasref dict /Font *$font2]
						set array1 [PDF create -ref array 0 0 400 400]
						set array2 [PDF create -ref array]
						
						set x [PDF create -hasref dict /Type /Pages /MediaBox *$array1 /Resources *$font3 /Kids *$array2 /Count 0]
						dict set x *type pages
						concat
					} page {
						set got [new_args [concat $args -XDefaults {{null null}}] -parent -contents]
						lassign $got pa co
						set x [PDF create dict /Type /Page /Parent $pa /Contents $co]
						dict set x *type page	
						concat
					} 
				}
				
				if $ref {
					set c [incrobj]
					dict lappend x *type object
					dict set x *begin [list [set asa "$c 0 obj\n"] {*}[dict get $x *begin]]
					dict lappend x *end [set asb "\nendobj\n"]
					dict incr x *length [string length $asa$asb]
					dict append ::OBJTABLE $c $x }
				if $Ret { if $ref { return [list $x $c ] } else {return $x} } elseif $ref { return $c } else { return $x }
			}
			update {
				
				set UName [lindex $args 0]
				set UP {} ; # "U Payload"
				set args [lrange $args 1 end]
				

				if { [string is alpha $UName] } {
					
					if {$UName eq {end} && ![info exists itIsVar]} { ;# $endIsVar
						set UTarget [dict get $::OBJTABLE end]
					} else {
						upvar 1 $UName UTarget
						set UUpvared 1
					}
				} elseif { [string is digit $UName] } {
					set UTarget [dict get $::OBJTABLE $UName]
					
				}
				
				set USwitch [lindex $args 0]
				
				
				if {$USwitch eq {internal}} {
					set USwitch [lindex $args 2]
					
					switch [lindex $args 1] {
						
						list {
							set args [lrange $args 3 end]
							set UP [dict get $UTarget *$USwitch] ; dict incr UTarget *length -[string length [join $UP {}]]
							if $inline {
								set UP $args
							} else {
							
								set UIndicies [dict keys $args]
								set UIndexMax [::tcl::mathfunc::max {*}$UIndicies]
								incr UIndexMax 1
								
								if {$UIndexMax > [llength $UP]} { 
									set UP [list {*}$UP {*}[lrepeat [expr {$UIndexMax - [llength $UP]}] OUT.OF.RANGE ]] }
								
								foreach UV $UIndicies UVV [dict values $args] {
									
									lset UP $UV $UVV
								}
							}
							dict set UTarget *$USwitch $UP ; dict incr UTarget *length [string length [join $UP {}] ]
							if [info exists __ib] { 
								set UP [list {*}[dict get $UTarget *ref] {*}$__ib] ; set UP [lsort -unique $UP]
								dict set UTarget *ref $UP ; dict set UTarget *refcount [llength $UP]
								dict set UTarget *count [llength [dict get $UTarget *thing]] }
						}
						dict {
							set args [lrange $args 3 end]
							set UOld_length 0
							dict for {UK UVAL} $args {
								if [dict exists $UTarget *$USwitch $UK] {
									incr UOld_length -[string length [dict get $UTarget *$USwitch $UK]] 
								} else {
									dict set UTarget *$USwitch $UK $vv
								}
							}
							dict set UTarget *$USwitch [dict merge [dict get $UTarget *$USwitch] $args ]
							incr UOld_length [string length [join [dict values $args] {}]]
							dict incr UTarget *length $UOld_length
							if [info exists __ib] { 
								set UP [list {*}[dict get $UTarget *ref] {*}$__ib ] ; set UP [lsort -unique $UP]
								dict set UTarget *ref $UP ; dict set UTarget *refcount [llength $UP] }
							concat
						}
						component {
							set args [lrange $args 3 end]
							if { [dict exists $UTarget *$USwitch]  } {
								dict incr UTarget *length -[string length [dict get $UTarget *$USwitch]]
								dict set UTarget *$USwitch [lindex $args 1]
								dict incr UTarget *length [string length [dict get $UTarget *$USwitch]]
							}
						}
					} 
					return 
				}
		
				set args [lrange $args 1 end]
				lassign [dict get $UTarget *type] UT1 UT2
				
				switch $UT1 {
					
					stream {
						lassign [lrange $args 0 1] UOperation USwitch
						set args [lrange $args 2 end]
						switch $USwitch {
							text {
								switch $UOperation {
									add {
										set UP [PDF create -minimal stream text {*}$args]
										dict incr UTarget *count
										dict incr UTarget *length [dict get $UP *length]
										dict set UTarget child_text[dict get $UTarget *count] [dict get $UP child_text1]
										
										}
									remove {
										set UP child_text[lindex $args 0]
										set args [lrange $args 1 end]
										dict incr UTarget *count -1
										dict incr UTarget *length -[string length [join [join [dict values [dict filter $UTarget key child_*] ] { } ] [dict get $UTarget *streammiddle]]  ]
									}
								}
							}
						}
					}
					
					header {
						
						if { $USwitch in {thing end} } { 
							PDF update UTarget internal list $USwitch {*}$args
						} elseif [dict exists $UTarget $USwitch] {
							PDF update UTarget internal dict {*}$args
							
						}
					}
					trailer {
						if  {$USwitch eq {thing}} {
							PDF update {*}[expr { $hasref ? {-upvar.ib} : {} }] UTarget internal dict $USwitch {*}$args
						} elseif { $USwitch eq {type}} {
							PDF update {*}[expr { $hasref ? {-upvar.ib} : {} }] UTarget internal component $USwitch {*}$args
						} elseif {$USwitch in {begin end}} {
							PDF update {*}[expr { $hasref ? {-upvar.ib} : {} }] UTarget internal list $USwitch {*}$args
						}
					}
					page -
					pages -
					catalog -
					dict {
	
							PDF update [expr { $hasref ? {-upvar.ib} : {} }] UTarget internal dict thing {*}$args
						}
					array {
						
						if $inline {
							PDF update -inline {*}[expr { $hasref ? {-upvar.ib} : {} }] UTarget internal list thing {*}$args
							
						} else {
							PDF update {*}[expr { $hasref ? {-upvar.ib} : {} }] UTarget internal list thing {*}$args
						}
						
						}
				}
				
				if ![info exists UUpvared] {  dict set ::OBJTABLE $UName $UTarget }
				
			}
			reftable {
				set offset [lindex $args 0]
				
				if {$offset eq {}} { set offset 0 }
				if {![dict exists $::OBJTABLE 0]} { PDF trailer }
				
				incr offset [dict get $::OBJTABLE 0 *length]
				
				set rows [list "[format %010d 0] 65535 f"]
				set objcount 1
				
				dict for {key val} $::OBJTABLE {
					if {$key == 0 || $key eq {end}} {continue}
					lappend rows "[format %010d $offset] 00000 n"
					incr offset [dict get $val *length]
					incr objcount
				}
				return [ list $rows $objcount $offset  ]
			}
			str {
				lassign $args target
				if [string is digit $target] {set target [dict get $::OBJTABLE $target]}
				lassign [dict get $target *type] t1 t2
				set A "[join [dict get $target *begin] {}]"
				set B "[join [dict get $target *end] {}]"
				set M "[join [dict get $target *thing] ]"
				switch $t1 {
					header {
						set A "[dict get $target *begin][dict get $target *magic][dict get $target *version][lindex [dict get $target *end] 0]"
						set M [lmap i [dict get $target *thing] j [lrange [dict get $target *end] 1 end] {subst "$i$j"}]
						set B {}
					}
					stream {
						set S [dict values [dict filter $target key child_*] ]
						set S [join $S ]
						set S [join $S [dict get $target *streammiddle]]
						set M [string cat $M $S]
					
					}
					}
					return "$A$M$B"
			}
			display {
				set rows [PDF str [dict get $::OBJTABLE 0 ]]
				
				dict for {key val} $::OBJTABLE {
					if {$key == 0 || $key eq {end}} {continue}
					append rows [PDF str $key]
				}
				
				lassign [PDF reftable] table objcount offset
				append rows "xref\n0 $objcount\n[join $table \n]"
				if ![dict exists $::OBJTABLE end] { PDF trailer }
				
				PDF update end thing /Size $objcount
				PDF update end end 2 "\n$offset"
				
				append rows [PDF str [dict get $::OBJTABLE end ]]
				#puts "[string repeat /*\\ 5]$rows[string repeat /*\\ 5]"
				#clipboard clear
				#clipboard append $rows
				return $rows
			}
			header {
				
				lassign $args ver 
				set ver [expr { [expr {$ver eq {}}] ? {1.4} : $ver } ]
				set args [lrange $args 1 end]
				
				set x [PDF create array ]
				dict set x *type header
				dict set x *magic {%PDF-}
				dict set x *version $ver
				dict set x *begin {} 
				dict set x *end [list {*}[lrepeat [llength $args] "\n"] "\n"]
				dict set x *thing [list {*}$args]
				dict set x *length [string length [join [list [dict get $x *magic] [dict get $x *version] {*}[dict get $x *thing] {*}[dict get $x *end]] {}]]
				dict set ::OBJTABLE 0 $x
			}
			trailer {
				set x [PDF create dict /Size x /Info x /Root x]
				dict set x *type trailer
				dict set x *begin [ list "\ntrailer\n" {*}[dict get $x *begin] ]
				dict lappend x *end "\nstartxref" "\nx" "\n%%EOF"
				dict set x *length [string length [join [list {*}[dict get $x *begin] {*}[dict get $x *thing] {*}[dict get $x *end]] {}] ]
				dict set ::OBJTABLE end $x
			}
		}
	
} 


# Toolbar #
frame .toolbar -relief flat -bd 5
pack [ttk::separator .toolbar.endseparator -orient horizontal] -side bottom -expand 1 -fill x -pady 1
pack [button .toolbar.first -text {} -relief flat -state disabled] -side left -expand 0 -fill none
place .toolbar -x 0 -y 0 -relwidth 1 -height 0.4in
ToolbarButton .toolbar.stack -text "\u2b94 Stack Things" -command stack_things




pack .toolbar.stack -side left
pack [ttk::separator .toolbar.endseparator2 -orient vertical] -side left -expand 0 -fill y -padx 1
pack [ToolbarButton .toolbar.save1 -text "$IconSave Save as PostScript" -command save::ps] -side left
pack [ttk::separator .toolbar.endseparator3 -orient vertical] -side left -expand 0 -fill y -padx 1
pack [ToolbarButton .toolbar.save2 -text "$IconSave Save as PDF" -command save::pdf] -side left

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
	
	#puts "*****path($path)*******
	#*****files\[[llength $files]\]*******
	#[join $files \n]
	#*****directories\[[llength $dirs]\]*******
	#[join $dirs \n]
	#---------------"
	
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
bind $e <Visibility> { "[$c cget -command]" [file normalize ~/TestPDF]  ; Adjustf
 }
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
	set w [expr "[winfo vrootwidth .]/2 - [winfo width $win]/2"]
	set h [expr "[winfo vrootheight .]/2 - [winfo height $win]/2"]
	

	return "+$w+$h"
}

# Pane Children
foreach v "$a .pane.main .pane.tabs" {.pane add $v -sticky nswe -stretch always
#puts "**$v**"
}

# Root Menu
menu .mMenu -tearoff 0
proc setmenu {{what .mMenu}} {. config -menu $what}
proc debug {} {
	
#objectpages
#PDF create stream text -text Messages -x 0
	set x [PDF create -ref page]
	set y [PDF create -ref pages]
	set z [PDF create -ref stream text -text HELLO -fontname /Font1 -fontsize 12] ; #-fontname Calibri
	PDF create -hasref dict /Len *8
	PDF update -hasref $x thing /Parent *$y /Contents *$z
	PDF update -hasref 6 thing 0 *1
	PDF update -hasref $y thing /Kids *6 /Count 1
	PDF header
	PDF trailer
	PDF update $z stream add text -x 9 -y 19
	set B [PDF create -ref catalog]
	set I [PDF create -ref info]
	PDF update -hasref end thing /Info *$I /Root *$B
	PDF update -hasref $B thing /Pages *$y
	PDF reftable
	PDF display
	concat
}

# Help->About Menu
menu .mMenu.mHelp -tearoff 0
.mMenu.mHelp add command -command {wm deicon $z; wm geometry $z [get_center $z]} -label About
#
.mMenu add cascade -label Help -menu .mMenu.mHelp
#
.mMenu add command -label Console -command {console show}
.mMenu add command -label Debug -command debug ;

#Off-screen menu
menu .mDoc -tearoff 0
menu .mPage -tearoff 0

.mDoc add command -label Delete -command {puts .mDoc_delete}
.mDoc add separator

.mDoc add command -label Clone -command {puts .mDoc_clone}
.mDoc add separator


#Context Menu for Pages

.mPage add command -label {Add New Page Above} -command {$whoobject up $whocalled}
.mPage add separator

.mPage add command -label {Add New Page Below} -command {$whoobject down $whocalled}
.mPage add separator


.mPage add command -label Delete -command { .mDoc_delete}
.mPage add separator

.mPage add command -label Clone -command {$whoobject clone $whocalled }
.mPage add separator


.mPage add command -label Move -command {$whoobject rename $whocalled }


proc postgeneric {at menu} {
	$menu post [winfo rootx $at ]  [expr [winfo rooty $at ]+[winfo height $at ]]
}
proc postmenu {name menu} { 
	$menu post [winfo rootx .toolbar.menu$name ]  [expr [winfo rooty .toolbar.menu$name ]+[winfo height .toolbar.menu$name ]]
}


# Do not remove; 
dict append misc switchmenu 1

proc ToolbarMenu {args}  {
	
	foreach command $args {
		switch $command {
			put {
				foreach x {{Help .mMenu.mHelp} {Console } {Debug }} {
					lassign $x v m
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
	set ::paneY [expr max([winfo reqheight .toolbar],[winfo height .toolbar])]
	place configure .pane -y $::paneY
	
	
	
	bind . <Visibility>
}
bind $cc <Visibility> {
	set dx [expr [winfo x ${::cc}]-[winfo x ${::cc}top]]
	set dy [expr [winfo y ${::cc}]-[winfo y ${::cc}left]]
	;#put zzzzzzzzzz $dx $dy
	mark::moveonce $dx 0 0 $dy
	bind $cc <Visibility>
}
put1 "[winfo width .] = [winfo vrootwidth .] = [winfo reqwidth .]"
raise .pane.main
#bind $cc <Motion> {+ ; small_triangle::update %x ;}
focus $::cc