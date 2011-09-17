# PS - 1 Exercise 2 contd: Dividing by zero and outputting warning to log file
# Written by Shyam Visweswaran at ADUni

ns_log Warning "About to divide by zero"
set bad_stuff [expr 7.8 / 0]

ns_return 200 text/html "<html>
<head><title>Exercise 2</title></head>

<body bgcolor=#ffffff>

<blockquote>
<font face=helvetica,arial>

<h2>Exercise 2</h2>
Trying to divide by zero
<br>
7.8 / 0 = $bad_stuff
<hr align=left width=50%>

<a href=../>Home</a>

</blockquote>

</body>
</html>"

# Output from the error log:
# [09/Apr/2001:16:52:55][1633.75781][-conn70-] Warning: About to divide by zero
# [09/Apr/2001:16:52:55][1633.75781][-conn70-] Error: divide by zero
# divide by zero
#    while executing
# "expr 7.8 / 0"
#    invoked from within
# "set bad_stuff [expr 7.8 / 0]"
#    (file "/opt/web/shyam/www/basics/my-broken-program.tcl" line 4)
#    invoked from within
# "source $file"
#    invoked from within
# "ns_sourceproc cns515 {}"