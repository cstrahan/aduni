#!/usr/bin/wish
wm title . Tracker

proc About { } {
    set choice [tk_messageBox -type ok -default ok \
	    -message "Do you roxxor?" \
	    -icon question]
}

proc CanvasHello { } { 

    menu .menubar
    . config -menu .menubar
    foreach m {File Edit Help} {
	set $m [menu .menubar.m$m]
	.menubar add cascade -label $m -menu .menubar.m$m
    }
    #File menu
    $File add command -label Quit  -command exit
    $File add command -label Foo -command {exec date}

    #Help menu
    $Help add command -label About -command About
    ####################################################

    set can [Scrolled_Canvas .c -width 400 -height 400 \
	    -scrollregion { 0 0 400 400 }]
    pack .c -fill both -expand true 

    ####################################################
    label .label -textvariable x
    ####################################################
    button .quit -text "Exit" \
	    -command exit
    pack .quit -side left
    button .foo -text "Foo" \
	    -command ShowDate
    pack .foo -side right
    # create text objects on the canvas
    $can create text 50 50 -text "Achilleus" -tag movable
    $can create text 50 350 -text "Hector" -tag movable
    $can create text 350 174 -text "Diomedes" -tag movable
    # bind actions to objects with the movable tag
    $can bind movable <Button-1> {CanvasMark %x %y %W}
    $can bind movable <B1-Motion> {CanvasDrag %x %y %W}
    
    ####################################################
}

proc ShowDate {} {
    set d [exec date]
    puts $d
}

proc Update_Coord { x y can } {
    #foo
}

proc CanvasMark { x y can } {
    global canvas
    # Map from view coord to canvas coord
    set x [$can canvasx $x]
    set y [$can canvasy $y]
    # Remember the object and its location
    set canvas($can,obj) [$can find closest $x $y]
    set canvas($can,x) $x 
    set canvas($can,y) $y
    set name $canvas($can,obj)
    puts stdout "Object $name: \($x,$y\)"
}
proc CanvasDrag { x y can } {
    global canvas
    # Map from view coord to canvas coord
    set x [$can canvasx $x]
    set y [$can canvasy $y]
    # Move the current object
    set dx [expr $x - $canvas($can,x)]
    set dy [expr $y - $canvas($can,y)]
    $can move $canvas($can,obj) $dx $dy
    set canvas($can,x) $x
    set canvas($can,y) $y
    set name $canvas($can,obj)
    puts stdout "Object $name: \($x,$y\)"
}

proc Scrolled_Canvas { c args } {
    frame $c
    eval {canvas $c.canvas \
	    -xscrollcommand [list $c.xscroll set] \
    	    -yscrollcommand [list $c.yscroll set] \
	    -highlightthickness 0 \
	    -borderwidth 0 } $args
    scrollbar $c.xscroll -orient horizontal \
	    -command [list $c.canvas xview]
    scrollbar $c.yscroll -orient vertical \
	    -command [list $c.canvas yview]
    grid $c.canvas  $c.yscroll -sticky news
    grid $c.xscroll -sticky ew
    grid rowconfigure $c 0 -weight 1
    grid columnconfigure $c 0 -weight 1
    return $c.canvas
}
eval CanvasHello



    