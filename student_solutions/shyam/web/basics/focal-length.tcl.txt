# PS -1 Exercise 3: focal length calculation
# Written by shyam Visweswaran at ADUni

set_the_usual_form_variables

# distance_in_feet, subject_size_in_feet are the args from the form
# they are now set in Tcl local variables thanks to the magic 
# utility function call above

# let's do a little IBM mainframe-style error-checking here

if { ![info exists distance_in_feet] || [string compare $distance_in_feet ""] == 0 } {
    ns_return 200 text/plain "Please fill in the \"distance to subject\" field"
    # stop the execution of this script
    return
}

if { ![info exists subject_size_in_feet] || [string compare $subject_size_in_feet ""] == 0 } {
    ns_return 200 text/plain "Please fill in the \"subject size\" field"
    # stop the execution of this script
    return
}

if { $distance_in_feet <=  0 || $subject_size_in_feet <= 0 } {
    ns_return 200 text/plain "Please use a distance in the \"distance to subject\" field"
    # stop the execution of this script
    return
}

# we presume that subject is to fill a 1.5 inch long-dimension of a
# 35mm negative

set distance_in_inches [expr $distance_in_feet * 12]
set subject_size_in_inches [expr $subject_size_in_feet * 12]
set magnification [expr 1.5 / $subject_size_in_inches]
set lens_focal_length_inches [expr $distance_in_inches / ((1/$magnification) + 1)]
set lens_focal_length_mm [expr round($lens_focal_length_inches * 25.4)]

ns_return 200 text/html "<html>
<head><title>Exercise 3</title></head>

<body bgcolor=#ffffff>
<font face=helvetica,arial>
<blockquote>

<table width=90%>
<tr><td> <h2>Here are the numbers</h2> </td>
<td align=right> <a href=../>Home</a> </td></tr>
</td></tr></table>

<hr>

<ul>
<li>distance to your subject:  $distance_in_feet feet ($distance_in_inches inches)
<li>long dimension of your subject:  $subject_size_in_feet feet ($subject_size_in_inches inches)
<li>magnification:  $magnification
<li>lens size required:  $lens_focal_length_inches inches ($lens_focal_length_mm mm)
</ul>

<hr>
<address><a href=\"mailto:shyam@massmed.org\">shyam@massmed.org</a></address>
</blockquote>
</body>
</html>"

