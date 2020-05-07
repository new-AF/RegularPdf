# RegularPDF
# Author: Abdullah Fatota
# Description: PDF Authoring Tool

package require Tk
package require TclOO
wm title . {RegularPDF}
wm geometry . "700x400+[expr [winfo vrootwidth .]/2]+[expr [winfo vrootheight .]/2]"
panedwindow .pane -showhandle 1 -sashwidth 10 -sashpad 20 -sashrelief raised -handlepad 0
place .pane -x 0 -y 0 -relwidth 1 -relheight 0.9

 frame .resize -bd 5 -relief groove
# About Dialog Window
proc buttonhover {w} {
	$w config -relief ridge
	
}
proc buttonleave {w} {
	$w config -relief flat
	
}
proc operation {operator args} {
	
	set target [lindex $args end]
	set args [lreplace $args end end]
	set count 0
	foreach v $args { 
	lset args $count [expr $v $operator $target]
	incr count}
	return $args
}
proc plus {args} { if [llength $args]==1 {set args [lindex $args 0]} ; return [operation + {*}$args] }
proc minus {args} { if [llength $args]==1 {set args [lindex $args 0]}  ; return [operation - {*}$args] }
proc product {args} { if [llength $args]==1 {set args [lindex $args 0]}  ; return [operation * {*}$args] }
proc divide {args} { if [llength $args]==1 {set args [lindex $args 0]}  ; return [operation / {*}$args] }
proc raise {args} { if [llength $args]==1 {set args [lindex $args 0]}  ; return [operation ** {*}$args] }
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
variable Font {TkDefaultFont} IconFolder "\ud83d\udcc2" IconBack "\u2190" IconReload "\u21bb" BlockCursor "\u25a0" IconFontIncrease "\ud83d\uddda" IconFontDecrease "\ud83d\udddb" IconPlus "\uff0b" IconMinus "\u2212" IconZoom "\ud83d\udd0e" IconSeethrough "\u239a" boldfont {-font {-weight bold}} eVar {} eDirCount {0} ePath {} fVar {} eHover {} sVar {} jVar {} pdff {} misc [dict create] cFont {} cSize {} cDim {} bCursor {} tIndex {} bIndex {} pi [expr asin(1)*2] paneY {} stackVar 0 lVar {} lId {} lPos {} lY {} tVar {} IconEnd "\ud83d\udccc"

set Cursor $BlockCursor

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

proc grand_annoucement {args} {
	puts -nonewline {******* }
	if {{-no} ni $args} {
	for {set i 0 ; set len [llength $args]} {$i < $len} {incr i 2} {
		puts -nonewline "[join [lrange $args $i $i+1 ] |] "
	}} else {puts -nonewline $args}
	puts {<<***}
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
;#grand_annoucement $r
	
	lassign $r angle radius shift dontConvertToRad ; set r $radius
	
;#puts "ANGLE>$angle<
;#RADIUS>$radius<
;#SHIFT>$shift<
;#DONT>$dontConvertToRad<
;#"
	
	lassign $shift xshift yshift
;#grand_annoucement $xshift $yshift
	
	set input [expr {$dontConvertToRad ? $angle : [deg_to_rad $angle] }]
	
	set x [expr $r*cos($input)+$xshift]
	set y [expr $r*sin($input)+$yshift] ; set y -$y
	
	return [list $x $y]
	
}

proc polygon {args} {
	grand_annoucement ARGS $args
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
			
			grand_annoucement -no $number $add ### "$number $FSIGN $add" ### [expr "$number $FSIGN $add"]
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
	
	
	
	grand_annoucement $r || $rr
	
	return "$r $rr"
}
#proc parallel {args} {parallelogram {*}$args}
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


proc stack_things {args} {
	
	
	if [set ::stackVar [expr !$::stackVar]] {
		place configure .pane -y 100 
		
	} else {
		place configure .pane -y $::paneY
	}
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

variable pdf2 {args} {
	namespace eval whitespace { null \x0 	htab \x9 	nextline \xa	nextpage \xc	cr \xd		space \x20 }
	namespace eval delimiter {leftparan \x28 rightparan \x29 leftangle	\x3c rightangle	\x3e lefsquare	\x5b rightsquare \x5d leftcurly	\x7b rightcurly \x7d rightslash	\x2f percent \x25}
	set i [split $:pdff {}]
	lsearch $i 
	
}

proc pdfparse {objpath} { #object is ::oo::objxxx it is result of [self] from the calling Object. 
	
	set str [subst $[subst $objpath]::str] ; puts "**pdf file Legnth: [string length $str] Bytes**"
	
	#set calc [expr 8*4+4*4-1] ; #12 ab cd 32 ....
	puts [lrepeat 10 *]
	
	set ::pdff $str

}

proc TEXThover0 {args} {
	#grand_annoucement TEXThover $args $::cDim
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
	grand_annoucement $x $y <> $info 
	if { $y+5 >= $bh || $x >= $bw } {grand_annoucement Nope ; 

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
			grand_annoucement [expr $g - 1]
			
			$c icursor BLOCK_CURSOR [incr ::bbVar -1]
			$c dchars BLOCK_CURSOR [expr $::bbVar + 1]
			$c insert BLOCK_CURSOR insert $::Cursor
			
			}
		}
		Return {
			set id [$c find withtag BLOCK_CURSOR]
			$c dchars BLOCK_CURSOR $::bbVar
			$c icursor BLOCK_CURSOR end
			$c insert BLOCK_CURSOR insert $::IconEnd
			;#$c rchars BLOCK_CURSOR $::bbVar [expr $::bbVar + 1] $::IconEnd
			$c itemconfig $id -tag TEXT
			$c create text $x $y -text $::Cursor -anchor w  -tag BLOCK_CURSOR
		}
		default {
			$c insert BLOCK_CURSOR insert $a
			incr ::bbVar
			
		}
	}
	
	return
	set r [ $c find overlapping $x $y [expr $x + [font measure TkDefaultFont -displayof $c $::Cursor] ] $y ]
	grand_annoucement $r
	foreach v $r { if { [$c type $v] eq {text} } { set r $v ; break } }
	grand_annoucement $r
	if { $r eq {} } {
	$c create text $x $y -tag TEXT -anchor w -text $k
	}
	
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
proc rotate {args} { 
	lappend args -XDefaults {"" 0 all}
	set got [new_args $args -c -angle -id] ;#tagOrID
	lassign $got c angle id
	grand_annoucement GOT $got
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
	set temp [new_args $args -xy -angles -radius -tag -c] ; grand_annoucement $temp
	lassign $temp ox langle radius tag c ; lassign $ox ox oy ; lassign $langle langle rangle
	grand_annoucement tag $tag
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
proc blink_line_cursor {{on 0}} {
	if $on {} else {}
}
proc hoverline {c {x ""} {y ""}} {
	
	if [string length $x]==0 {
		set f [font metrics TkDefaultFont] ; set s [lsearch -glob $f -linespace] ; set s [lindex $f $s+1]
		incr s 5
		set ::lVar $s ; lassign [$c bbox B] bx by bw bh ; #grand_annoucement ABA [$c bbox B]
		incr bx 7
		;#{grand_annoucement %W %x %y [%W canvasx %x] [%W canvasy %y] [%W bbox B]}
		set many [expr {$bh} / $s] ; set count -1 ; set i $by ; incr i $s ; while {[incr count] < $many} { grand_annoucement [$c create line $bx $i $bw $i -width 2 -dash _ -fill {} -tag LINE] ; incr i $s }
		;#$c bind B <Motion> {hoverline %W %x %y}
		$c create text $bx $by -text $::Cursor -anchor n -fill {} -tag BLOCK_CURSOR
		;#$c bind B <Enter> {%W config -cursor none}
		;#$c bind B <Leave> {%W config -cursor arrow}
		return
	}
	;#set x [$c canvasx $_x] ; set y [$c canvasy $_y] 
	;#$grand_annoucement many $many
	$c itemconfig LINE -fill {}
	;#set next [$c find closest $x $y $::lVar B]
	;#set next [$c find closest $x $y $::lVar ]
	;#set next [$c index LINE @$x,$y]
	set next [$c find overlapping $x $y [expr $x+1] [expr $y+$::lVar] ]
	foreach v $next { if {[$c type $v] eq {line}} {set next $v ; break} }
	;#grand_annoucement next $next
	
	if {[$c type $next] eq {line}} {$c itemconfig $next -fill gray ;
		set ::lId $next ; set ::lPos [$c bbox $next] ; set ::lY [lindex $::lPos 1]
		$c config -cursor hand1
		$c itemconfig BLOCK_CURSOR -fill black
		$c moveto BLOCK_CURSOR $x [expr $::lY - $::lVar + 5]
		$c focus BLOCK_CURSOR
		
	}
}

oo::class create Tabs { 
	
	variable fcount newcount lobj sobj m mc 		t tc
	set cm {}
	
	constructor {} {
	
	my create_main
	
	
	set t [labelframe .pane.tabs -text {Current Tabs} -relief ridge -bd 5]
	
	set tc [canvas $t.canvas]
	set com "[self] create {}"
	
	
	button $tc.add -text "\ud83d\uddcb New Document" -relief groove -command $com
	ttk::separator $tc.end -orient horizontal
	
	set fcount 0
	set newcount 0
	pack $tc -fill both -side top
	
	
	pack $tc.add -fill x -pady 0.05in -side top
	pack $tc.end -fill x -pady 0.05in -side bottom
	
	#place .tabs -relx 0.66 -y 0.4in -relwidth 0.3 -relheight 1
	
	
	}
	
	method create_main {} {
		set m [labelframe .pane.main -relief groove -bd 5]
		set tbar [frame .pane.main.toolbar -relief groove -bd 2]
		set mc [canvas $m.canvas -highlightbackground green]
		#place .main -relx 0.34 -y 0.4in -relwidth 0.3 -relheight 1
		
		set com "$mc configure -scrollregion  [$mc bbox all]"
		set pad [$m.canvas cget -highlightthickness]
		;#$mc create text [expr 0+$pad] [expr 0+$pad] -text {INITIAL TEXT} -tag TEXT -anchor nw
		
		bind $m <Configure> $com
		$mc bind TEXT <Motion> {TEXThover %W [%W canvasx %x] [%W canvasy %y] %h}
		put_scrolls -control $mc -put .pane.main -xplace {pack $put.scrollx -side bottom -fill x } -yplace {pack $put.scrolly -side right -fill y }
		pack $tbar -side top -expand 0 -fill x -pady 5
		pack [canvas .pane.main.canvastop -highlightthickness 2 -highlightbackground purple -height 1c] -side top -after $tbar -fill x
		pack [canvas .pane.main.canvasleft -highlightthickness 2 -highlightbackground brown -width 1c] -side left -fill y 
		pack $mc -expand 1 -fill both -side bottom
		my fill_canvas_toolbar
		my draw_document
		my make_ruler
		hoverline $mc ;# grand_annoucement $ox $oy $w $h <> $x $y; ; grand_annoucement OUT
		bind $mc <Motion> { lassign [%W bbox B] ox oy w h ; set x [%W canvasx %x] ; set y [%W canvasy %y] ; if { $x  >= $ox && $x <= [expr $ox + $w] && $y >= $oy && $y <= [expr $oy + $h]} { hoverline %W $x $y } else {%W config -cursor arrow } }
		$mc bind BLOCK_CURSOR <Key> { set x [%W canvasx %x] ; set y [%W canvasy %y] ; create_text %W $x $y %K %A}
		my triangle_tick
		focus $mc
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
		
		pack [button $tbar.enlarge -text "$::IconFontIncrease Enalrge Text" -relief groove -command "change_font +"] -side left -expand 0 -padx 1
		pack [button $tbar.ensmall -text "$::IconFontDecrease Ensmall Text" -relief groove -command "change_font -"] -side left -expand 0 -padx 1
		
		separator $tbar.separator2 label -pack
		
		pack [button $tbar.zoomin -text "$::IconPlus $::IconZoom Zoom In" -relief groove -command {.pane.main.canvas scale all [expr [.pane.main.canvas cget -width]/2] [expr [.pane.main.canvas cget -height]/2] 2 2 }] -side left -expand 0 -padx 1
		pack [button $tbar.zoomout -text "$::IconMinus $::IconZoom Zoom Out" -relief groove -command {.pane.main.canvas scale all [expr [.pane.main.canvas cget -width]/2] [expr [.pane.main.canvas cget -height]/2] .5 .5 }] -side left -expand 0 -padx 1
		
		separator $tbar.separator3 label -pack
		

		bind $c <ButtonPress> {.pane.main.canvas scan mark %x %y}
		bind $c <B1-Motion> {.pane.main.canvas scan dragto %x %y 1}
		bind $c <MouseWheel> "[self] mouse_wheel %x %y %D"
	}
	method draw_document {} {
		set c .pane.main.canvas
		#grand_annoucement [join [.pane.main.canvas config] \n]
		#grand_annoucement [join [winfo reqheight .pane.main.canvas ] \n]
		
		set w [$c cget -width]
		set h [$c cget -height]
		set pad 15
		
		$c create polygon [parallel -dangle [expr 180-45] -h 250 -v 10 -orig 20,17] -outline black -fill black -tag BOX
		;#$c create polygon [parallel -dangle [expr 90] -h 250 -v 10 -orig 20,17] -outline black -fill black
		
		$c create rectangle [polygon 250,18 +20,+402] -outline black -fill black -tag BOX
		$c create rectangle [polygon 10,20 +250,+400] -outline black -fill white -tag {BOX B}
		 
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
		set ct .pane.main.canvastop
		set c .pane.main.canvas
		set cl .pane.main.canvasleft
		;#puts **********[$c bbox BOX ]
		;#puts **********[$c cget -height ]
		
		variable w [$c cget -width]  h [$c cget -height]  pad [$c cget -highlightthickness]
		
		variable x $pad  y $pad
		
		variable tickLength [expr ($pad+1)*2]
		
		
		set testcm [$c create line 0 0 1cm 0 -fill ""]
		
		variable cm [lindex [$c bbox $testcm] 2]  tenth [expr $cm/10]
		set max_tickLength [expr $tickLength+$tenth+5]
		
		for {set i 0 ; set count 0} {$i < $w} {incr i $cm ; incr count} {
			set end [expr $i+$cm]
			for {set j $i ; set tick 0} {$j < $end} {incr j $tenth; incr tick} {
				$ct create line $j $pad $j [expr $tickLength+$tick] -fill black -tag TICK$tick
				
			}
			$ct create text $i $max_tickLength -text $count -anchor n
		}
		
		
		for {set i 0 ; set count 0} {$i < $w} {incr i $cm ; incr count} {
			set end [expr $i+$cm]
			for {set j $i ; set tick 0} {$j < $end} {incr j $tenth; incr tick} {
				$cl create line 0 $j [expr $tickLength+$tick] $j -fill black -tag TICK$tick
				
			}
			$cl create text $max_tickLength $i -text $count -anchor w
		}
	
	}
	method triangle_tick {} {
		set c .pane.main.canvas
		set ct .pane.main.canvastop
		
		set t [triangle -angles {80 80} -radius 1 -tag tri -c $ct -xy "50 [$ct cget -height]"]
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
set f [listbox $a.1list -relief flat -highlightthickness 2 -highlightcolor red -cursor hand2 -activestyle dotbox -bg [. cget -bg] -listvar fVar -justify center]

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
pack $f -side left -expand 0 -fill y
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
ToolbarButton .toolbar.stack -text "\u2b94 Stack Things" -command stack_things
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
	puts [polygon 123,456 +1,+1]
	puts [polygon 45.123,23.435 +100,+0]
	puts [polygon 45.123,23.435 +100,365]
	puts [polygon 45.123,23.435 +100,#0+0]
	puts [polygon 45.123,23.435 +100,#10+0]
	puts [polygon 45.123,23.435 +100,#10]
	
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
	set ::paneY [expr max([winfo reqheight .toolbar],[winfo height .toolbar])]
	place configure .pane -y $::paneY
	bind . <Visibility>
}