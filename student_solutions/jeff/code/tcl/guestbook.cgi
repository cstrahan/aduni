#!/bin/sh
# guestbook.cgi
# A simple guestbook page.
exec tclsh "$0" ${1+"$@"}

set dir [file dirname [info script]]
source [file join $dir cgilib.tcl]
set datafile [file join $dir guestbook.data]

Cgi_Header "Brent's Guestbook" {BGCOLOR=white TEXT=black}
P
if {![file exists $datafile]} {
    puts "No registered guests, yet."
    P
    puts "Be the first [Link {registered guest!} newguest.html]"
} else {
    puts "The following folks have registered in my GuestBook."
    P
    puts [Link Register newguest.hml]
    H2 Guests
    catch {source $datafile}
    foreach name [lsort [array names Guestbook]] {
	set item $Guestbook($name)
	set homepage [lindex $item0 ]
	set markup [lindex $item 1]
	H3 [Link $name $homepage]
	puts $markup
    }
}
Cgi_End



