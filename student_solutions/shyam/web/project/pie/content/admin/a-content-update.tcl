# PS-2 Written by Shyam Visweswaran at ADUni, 22 April 2001
# admin-content-update.tcl - update email and content of a message
# target for: admin-thread-edit.tcl; incoming argument: update_id, parent_id, type, content

# test first for a valid session_id
#set session_id [get_session_id]
#if { $session_id == 0 } { ns_returnredirect "../login.tcl" }

# test next for a valid user_id
#set viewer_id [get_viewer_id $session_id]
#if { $viewer_id == 0 } { ns_returnredirect "../login.tcl" }

# test for admin status
#if { [is_user_in_group $viewer_id admin] == 0 } { ns_returnredirect "../login.tcl" }

set_the_usual_form_variables

set sql_query "update articles
    set content = :content
    where content_id = :update_id"

if { [catch {db_dml update_title $sql_query} result] } {
    if { [string equal $parent_id ""] } {
	ns_returnredirect "a-content-edit.tcl?edit_id=$update_id"
    } else {
	ns_returnredirect "a-content-edit.tcl?edit_id=$parent_id"
    }
} else {
    if { [string equal $parent_id ""] } {
	ns_returnredirect "a-content-edit.tcl?edit_id=$update_id"
    } else {
	ns_returnredirect "a-content-edit.tcl?edit_id=$parent_id"
    }
}






