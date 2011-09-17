#!/usr/local/bin/wish
wm title . roxxor

menu .menubar
. config -menu .menubar
foreach m {File Edit Help} {
    set $m [menu .menubar.m$m]
    .menubar add cascade -label $m -menu .menubar.m$m
}
#File menu
$File add command -label Foo  -command exit
$File add command -label Quit -command exit

#Help menu
$Help add command -label About -command exit

####################################################
#a silly message
message .msg -aspect 1000 -justify center -text "roxor in extremitas"
pack .msg

#a scale
scale .scale1 -from 0 -to 60 -length 500 -variable m \
	-orient horizontal -label "Minute" \
	-tickinterval 5 -showvalue true
pack .scale1

#another scale
scale .scale2 -from 0 -to 24 -length 500 -variable h \
	-orient horizontal -label "Hour" \
	-tickinterval 2 -showvalue true
pack .scale2
