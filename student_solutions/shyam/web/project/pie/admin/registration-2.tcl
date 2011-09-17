# PS-2 Written by Shyam Visweswaran at ADUni, 13 April 2001
# added to by todd sjoblom 26 apr 2001 to remove group, respell viewer_id
# registration-2.tcl - target for registration.tcl; check session_id and process registration form
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

# grab the values from the form
set_the_usual_form_variables

# do error checking on the form values
#ns_log Warning  ":$first_names;$last_name;$email."
set error_message ""
set first_names [string trim $first_names ]
if { [string equal $first_names ""] } {
    append error_message "<li><font color=red>First name is missing</font>"
}

set last_name [string trim $last_name ]
if { [string equal $last_name ""] } {
    append error_message "<li><font color=red>Last name is missing</font>"
}

set email [string trim $email ]
if { [string equal $email ""] } {
    append error_message "<li><font color=red>Email is missing</font>"
}
# will need a regexp here for more sophisticated email checking

set password [string trim $password ]
if { [string length $password] <= 4 } {
    append error_message "<li><font color=red>Password should be at least 5 characters long</font>"
}

# email is case-insensitive and password is case-sensitive
set sql_duplicate_check "select count(*) from users where upper(email) = upper(':email')"

if { 0 < [db_string my_count $sql_duplicate_check] } { ;# we should be able to guarantee <=1, because we've programmed uniqueness of upper(email).
    append error_message "<li><font color=red>Duplicate email already in database; will not create this user.</font>"
}

# if no errors go to the insertion of the record -- we could lock the select-insert as a transaction.
# If another admin changed or inserted the same email at this monet, a _un constraint appears in the log.
if { [string equal $error_message ""] } {
    # This is this the best place to do a seq, better than in registration.tcl.
    set user_id [db_string nextval "select user_seq.nextval from dual"] ;# set for insert below
    # so that we can soon redirect to display this particular user's information
    db_dml insert_user "insert into users
     (user_id, first_names, last_name, email, password, registration_date, status, creation_user)
     values
     (:user_id, :first_names, :last_name, :email, :password, sysdate, 'active', :viewer_id)"
    # redirect back to the admn page, where we see the user's name at the to of the list.
    # Administrator can there click on Details to add user_id to any groups as member/leader.
    # member/leader
    ns_returnredirect "admin.tcl"
}

# if errors display error messages and the prefilled form

set page_content "
[html_head "New user registration"]
[html_body_start]
[html_body_top "New user registration" "<a href=admin.tcl>Administration</a> > New user registration"]

<!-- stuff for body of page -->

<table width=90%>

<tr><td colspan=2>

We had some problems processing your entries.

<ul>
$error_message
</ul>

<form method=post action=registration-2.tcl>  </td></tr>

<tr><td>First Name</td>
<td> <input type=text name=first_names size=50 value=\"$first_names\">  </td></tr>

<tr><td>Last Name</td>
<td> <input type=text name=last_name size=50 value=\"$last_name\"> </td></tr>

<tr><td>Email</td>
<td> <input type=text name=email size=50 value=\"$email\"> </td></tr>

<tr><td>Password</td>
<td> <input type=text name=password size=30 value=\"$password\"> </td></tr>

<tr><td>&nbsp;</td><td> <input type=reset value=\"Reset\">
        &nbsp &nbsp <input type=submit value=\"Submit\"> </form> </td></tr>
"
db_release_unused_handles

append page_content "
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"

ns_return 200 text/html $page_content





