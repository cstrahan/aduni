# PS-2 Written by Shyam Visweswaran at ADUni, 22 April 2001
# admin-title-update.tcl - update the title of article/question
# target for: admin-content-edit.tcl; incoming argument: update_id, table_type, title

# test first for a valid session_id
#set session_id [get_session_id]
#if { $session_id == 0 } { ns_returnredirect "../login.tcl" }

# test next for a valid user_id
#set viewer_id [get_viewer_id $session_id]
#if { $viewer_id == 0 } { ns_returnredirect "../relogin.tcl" }

# test for admin status
#if { [is_user_in_group $viewer_id admin] == 0 } { ns_returnredirect "../login.tcl" }

set_the_usual_form_variables
  ;# Note that :table is not allowed
set sql_query "update $table_type
    set title = :title
    where content_id = :update_id"

if { [catch {db_dml update_title $sql_query} result] } {
    set page_content "
    [html_head "Title update"]
    [html_body_start]
    [html_body_top "Title update" "<a href=\"../home.tcl\">Home</a> > <a href=\"admin-content.tcl\">Administer content</a> > Title update"]
    
    <!-- stuff for body of page -->
    
    <table width=90%>
    <tr><td colspan=2>
    There was a problem updating the title. Someone probably has deleted this thread.
    
    </td></tr>
    </table>
    
    [html_body_bottom]
    [html_body_end]
    [html_foot]
    "
    ns_return 200 text/html $page_content

} else {
    ns_returnredirect "admin-content-edit.tcl?type=$table_type&edit_id=$update_id"
}




