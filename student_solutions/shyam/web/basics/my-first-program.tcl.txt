# Exercise 2: output day and time
# Written by Shyam Visweswaran at ADUni

set current_time [clock format [clock seconds]]
ns_return 200 text/html "<html>
<head><title>Exercise 2</title></head>

<body bgcolor=#ffffff>
<blockquote>
<font face=helvetica,arial>

<table width=90%>
<tr><td> <h2>Current day and time</h2> </td>
<td align=right> <a href=../>Home</a> </td></tr>
</td></tr></table>

<hr>
<p>
The current day and time is <b>$current_time</b>.
<p>
<hr>

<address><a href=\"mailto:shyam@massmed.org\">shyam@massmed.org</a></address>

</blockquote>

</body>
</html>"


