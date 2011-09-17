# PS-2 Written by Shyam Visweswaran at ADUni, 13 April 2001
# added to by todd sjoblom 26 apr 2001 respell viewer_id, remove group, db_release
# registration.tcl - check session_id and display form for registration of new user
# only Admin allowed on this page

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "../login.tcl" }

# test next for a valid viewer_id
set viewer_id [get_user_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "../relogin.tcl" }

# test next for admin status
set viewer_is_admin 0 ;# preset
if { [is_user_in_group $viewer_id "admin"] == 1 } {
    set viewer_is_admin 1
} else {
    ns_returnredirect "../login.tcl" 
}

set page_content "
[html_head "New user registration"]
[html_body_start]
[html_body_top "New user registration" "<a href=admin.tcl>Administration</a> > New user registration"]

<!-- stuff for body of page -->

<table width=90%>

<tr><td colspan=2>
Please complete the following information to register a new user:
<form method=post action=registration-2.tcl>  </td></tr>

<tr><td>First Name(s)</td>
<td> <input type=text name=first_names size=50>  </td></tr>

<tr><td>Last Name</td>
<td> <input type=text name=last_name size=50> </td></tr>

<tr><td>Email</td>
<td> <input type=text name=email size=50> </td></tr>

<tr><td>Password</td>
<td> <input type=text name=password size=30> </td></tr>

<tr><td>&nbsp;</td><td> <input type=reset value=\"Reset\">
        &nbsp &nbsp <input type=submit value=\"Submit\"> </form> </td></tr>
"

append page_content "
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"

ns_return 200 text/html $page_content








