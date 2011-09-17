# password.tcl - display form to change a member's password
# parameters: pwd_id
# Written by Shyam Visweswaran at ADUni, 27 April 2001

set pwd_id [ns_queryget pwd_id]

set page_content "
[html_head "Change password"]
[html_body_start]
[html_body_top "Change password" "<a href=\"home.tcl\">Home</a> > Change password"]

<!-- stuff for body of page -->

<blockquote>
<table width=90%>
<tr><td>

<form method=post action=password-2.tcl>
<input type=hidden name=pwd_id value=$pwd_id>    
Please type your old password
<br>
<input type=password name=old_pwd size=30>
<p>

Now type your new password (should be at least 5 characters long)
<br>
<input type=password name=new_pwd1 size=30>
<p>

And retype your new password
<br>
<input type=password name=new_pwd2 size=30>
<p>

<input type=submit value=Submit>
</form>

</td></tr>
</table>
</blockquote>

[html_body_bottom]
[html_body_end]
[html_foot]
"

ns_return 200 text/html $page_content







