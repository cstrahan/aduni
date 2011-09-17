   # PS-2 Written by Therese Hendricks at ADUNI, 18 April 2001
#added to by todd sjoblom 20 April 2001
   # edit_user_info-2.tcl - target for edit_user_info.tcl;  grab form values and updat   # e users table

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "login.tcl" }

# test next for a valid viewer_id
set viewer_id [get_user_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "login.tcl" }

# test for admin status
set viewer_is_admin 0 ;# preset
if { [is_user_in_group $viewer_id "admin"] == 1 } {
    set viewer_is_admin 1
}

# grab the values from the form
set_the_usual_form_variables

# add code here for error checking on form values

if { !($viewer_is_admin == 1 || $viewer_id == $edit_id) } {
    ns_return text/plain "Only the administrator or the user can perform an edit!"
    # stop the execution of this script
    return
}

# do error checking on the form values (as in registration.-2.tcl)
set error_message ""
set first_names [string trim $first_names]
if { [string equal $first_names ""] } {
    ns_return 200 text/plain "We need your first name(s).  Please hit Back."
    # stop the execution of this script
    return
}
set last_name [string trim $last_name]
if { [string equal $last_name ""] } {
    ns_return 200 text/plain "We need your last name.  Please hit Back."
    # stop the execution of this script
    return
}
set email [string trim $email]
if { [string equal $email ""] } {
    ns_return 200 text/plain "We need your email.  Please hit Back."
    # stop the execution of this script
    return
}
# will need a regexp here for more sophisticated email checking

set sql_user "select * from users where upper(email) = upper(:email) and user_id != :edit_id"
db_foreach get_user_info $sql_user {
    # return if n >= 1 dupes (probably n == 1)
    ns_return 200 text/plain "Somebody else is already using that email, and we can't 
    assign it to you.  If you misspelled $email, please go Back and try again."
    # stop the execution of this script
    return
}

# if no errors, update users table and go to display user info page
db_dml update_user "update users
set name_prefix = :name_prefix, first_names = :first_names, 
 last_name = :last_name, name_suffix = :name_suffix,
 email = :email, email_bouncing_p = :email_bouncing_p, 
 phone_home = :phone_home, phone_work = :phone_work
 where user_id = :edit_id"

db_release_unused_handles

# redirect to display user info page to view updates
ns_returnredirect "display_user_info.tcl?display_id=$edit_id" 








