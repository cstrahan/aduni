# PS-2 Written by Shyam Visweswaran at ADUni, 22 April 2001
# admin-cat-update.tcl - update the category of article/question
# target for: admin-content-edit.tcl; incoming argument: forum, table, update_id, table_type, category

# test first for a valid session_id
#set session_id [get_session_id]
#if { $session_id == 0 } { ns_returnredirect "../../relogin.tcl" }

# test next for a valid user_id
#set viewer_id [get_viewer_id $session_id]
#if { $viewer_id == 0 } { ns_returnredirect "../../relogin.tcl" }

# test for admin status
#if { [is_user_in_group $viewer_id admin] == 0 } { ns_returnredirect "../../relogin.tcl" }

set_the_usual_form_variables

set new_category [string trim $new_category]

if { ![string equal $new_category ""] } {
    set sql_query "update $table
        set category = :new_category
        where content_id = :update_id"
} elseif { [string equal $category ""] } {
    set sql_query "update $table
        set category = NULL
        where content_id = :update_id"
} else {
    set sql_query "update $table
        set category = :category
        where content_id = :update_id"
}

if { [catch {db_dml update_title $sql_query} result] } {

    set page_content "
    [html_page_header 2 "Category update" "<a href=\"../../admin/admin.tcl\">Administration</a> > <a href=\"admin-content.tcl?forum=$forum\">Administer $forum Q and A</a> > Category update"]
    
    <!-- stuff for body of page -->
    
    <table width=90%>
    <tr><td colspan=2>
    There was a problem updating the category. Someone probably has deleted this thread.
    
    </td></tr>
    </table>

    [html_page_footer 2]  
    "
    ns_return 200 text/html $page_content

} else {
    ns_returnredirect "admin-content-edit.tcl?forum=$forum&table=$table&edit_id=$update_id"
}


