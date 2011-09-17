# PS-2 
# relogin.tcl - does login if session expires while visiting the site
# this script calls itself when user hits submit on the webpage
# Written by Shyam Visweswaran at ADUni, 15 April 2001

# test first for a valid session_id and get the user_id
set session_id [get_session_id]

set page_content "
[html_head "Login"]
[html_body_start]
[html_body_top "Login" "&nbsp;"]

<!-- stuff for body of page -->

<table width=90%>
<tr><td>
"

if { $session_id == 0 } {
    ns_returnredirect "login.tcl"
} else {
    set sql_sessions_query "select user_id from sessions where session_id = :session_id"
    db_foreach get_user_sessions $sql_sessions_query {
	set user_id $user_id
    } if_no_rows {
	ns_returnredirect "login.tcl"
    }

    # With the user_id get email and password
    set sql_users_query "select email, password from users where user_id = :user_id"
    if { [db_0or1row get_user_sessions $sql_users_query] == 0 } {
	ns_returnredirect "login.tcl"	
    } else {
	set login_name $email
	set password $password
    }
}

set page_content "
[html_header "Login"]
<hr>
"

if { [ns_queryexists "login_password"] && [string length [ns_queryget "login_password"]] > 0 } {
    set login_password [ns_queryget "login_password"]   
    
    if { $login_password == $password } {
	make_new_session $user_id
        ns_returnredirect "home.tcl"
    } else {
        append page_content "<font color=red>Your Password is not valid.</font>"
    }
}

db_release_unused_handles

append page_content "
<!-- this form transmits info to this script itself>
<form method=post action=relogin.tcl>

Hello <i>$login_name</i>, please enter your password.
<input type=hidden name=login_name size=40 value=$login_name>
<p>
<input type=password name=login_password size=30>
&nbsp &nbsp <input type=submit value=Login>
<p>

<i>$login_name</i> not your email?<br>
<a href=login.tcl>Sign in as a different user</a>.

<p>

<a href=\"get-pwd.tcl\">Forgotten your password</a>?

</form>
</td></tr>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"
ns_return 200 text/html $page_content







