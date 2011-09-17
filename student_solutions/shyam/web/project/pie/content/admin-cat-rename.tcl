# PS-2 Written by Shyam Visweswaran at ADUni, 22 April 2001
# admin-cat-rename.tcl - rename an existing category
# target for: admin-cat-article.tcl, admin-cat-disc.tcl; incoming argument: category, content_type

# test first for a valid session_id
#set session_id [get_session_id]
#if { $session_id == 0 } { ns_returnredirect "../login.tcl" }

# test next for a valid user_id
#set viewer_id [get_viewer_id $session_id]
#if { $viewer_id == 0 } { ns_returnredirect "../login.tcl" }

# test for admin status
#if { [is_user_in_group $viewer_id admin] == 0 } { ns_returnredirect "../login.tcl" }

set_the_usual_form_variables

set page_content "
[html_head "Category rename"]
[html_body_start]
[html_body_top "Category rename" "<a href=\"../home.tcl\">Home</a> > <a href=\"admin-content.tcl\">Administer content</a> > Category rename"]
"

set sql_query "update $type
    set category = :category
    where category = :old"

if { [catch {db_dml update_title $sql_query} result] } {
    ns_return 200 text/plain "There was a problem with renaming the category: $result."
    # stop the execution of this script
    return
} else {
    append page_content "
    <!-- stuff for body of page -->

    <table width=90%>
    <tr><td colspan=2>
    Category successfully renamed!
    
    </td></tr>
    </table>
    
    [html_body_bottom]
    [html_body_end]
    [html_foot]
    "
    ns_return 200 text/html $page_content
}



