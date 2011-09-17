# PS-2 
# login.tcl - processes login
# this script calls itself when user hits submit on the webpage
# Written by Shyam Visweswaran at ADUni, 11 April 2001

set page_content "
[html_head "Login"]
[html_body_start]
[html_body_top "Login" "&nbsp;"]

<!-- stuff for body of page -->

<table width=90%>
<tr><td>
</td><td>
"
set login_name ""

if { ![ns_queryexists "login_name"] } {
    append page_content ""
} elseif { [string length [ns_queryget "login_name"]] < 1 } {
    append page_content "<font color=red>Please enter your <b>Email</b>.</font>"
} elseif { [ns_queryexists "login_password"] && [string length [ns_queryget "login_password"]] < 1 } {
    set login_name [ns_queryget "login_name"]
    append page_content "<font color=red>Please enter your <b>Password</b>.</font>"
} elseif { [ns_queryexists "login_name"] && [ns_queryexists "login_password"] } {
    set login_name [ns_queryget "login_name"]
    set login_password [ns_queryget "login_password"]   

    set sql_user "select user_id, last_visit from users
                  where upper(email) = upper(:login_name) and password = :login_password
                  and status != 'banned'"

    if { [db_0or1row get_user_info $sql_user] == 1 } {
	make_new_session $user_id
	db_dml update_last_visit "update users 
	                          set second_to_last_visit = :last_visit, last_visit = sysdate
                                  where user_id = :user_id"
	ns_returnredirect "home.tcl"
    } else {
        append page_content "<font color=red>Your Email and Password do not match our records.</a>"
    }
}

db_release_unused_handles

append page_content "
<p>
</td></tr>

<form method=post action=login.tcl>
<tr><td>
Email
</td>
<td>
<input type=text name=login_name size=40 value=$login_name>
</td></tr>
<tr><td>
Password
</td>
<td>
<input type=password name=login_password size=30>
&nbsp &nbsp <input type=submit value=Login>
<br>

<a href=\"get-pwd.tcl\">Forgotten your password</a>?

</td></tr>
</form>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"
ns_return 200 text/html $page_content






