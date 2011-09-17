#!/usr/local/bin/wish
wm title . Howdy

proc CanvasHello {} {
    set can [Scrolled_Canvas .c -width 400 -height 300 \
	    -scrollregion { 0 0 1000 400 }]

#Scrolled_Canvas .c -width 400 -height 300 \
#	-scrollregion {0 0 1000 400}

    pack .c -fill both -expand true
    # create a text object on the canvas
    $can create text 50 50 -text "Howdy" -tag movable
    # bind actions to objects with the movable tag
    $can bind movable <Button-1> {CanvasMark %x %y %W}
    $can bind movable <B1-Motion> {CanvasDrag %x %y %W}
}

proc CanvasMark { x y can } {
    global canvas
    # Map from view coord to canvas coord
    set x [$can canvasx $x]
    set y [$can canvasy $y]
    # Remember the object and its location
    set canvas ($can, obj) [$can find closest $x $y]
    set canvas ($can, x) $x
    cet canvas ($can, y) $y
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
    set canvas ($can,x) $x
    cet canvas ($can,y) $y
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

#pack .c -fill both -expand true    

    