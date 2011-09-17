# get-pwd.tcl - display form to enter email to where password will be
# emailed.
# Written by Shyam Visweswaran at ADUni, 27 April 2001

set page_content "
[html_head "Forgotten password"]
[html_body_start]
[html_body_top "Forgotten password" "<a href=\"login.tcl\">Login</a> > Forgotten password"]

<!-- stuff for body of page -->

<table width=90%>
<tr><td>

<form method=post action=get-pwd-2.tcl>
Type your login <b>email address</b> to have your password emailed to you.
<p>
<input type=text name=email size=40 value=>
&nbsp; &nbsp <input type=submit value=Submit>
</form>

</td></tr>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"
ns_return 200 text/html $page_content







