# PS-2 Written by Shyam Visweswaran at ADUni, 22 April 2001
# admin-cat-rename.tcl - rename an existing category
# target for: admin-cat-article.tcl, admin-cat-disc.tcl; incoming argument: forum, table, category, content_type

# test first for a valid session_id
#set session_id [get_session_id]
#if { $session_id == 0 } { ns_returnredirect "../../relogin.tcl" }

# test next for a valid user_id
#set viewer_id [get_viewer_id $session_id]
#if { $viewer_id == 0 } { ns_returnredirect "../../relogin.tcl" }

# test for admin status
#if { [is_user_in_group $viewer_id admin] == 0 } { ns_returnredirect "../../relogin.tcl" }

set_the_usual_form_variables

set sql_query "update $table
    set category = :category
    where category = :old"

if { [catch {db_dml update_title $sql_query} result] } {
    ns_return 200 text/plain "There was a problem with renaming the category: $result."
    # stop the execution of this script
    return
} else {
    ns_returnredirect "admin-cat-disc.tcl?forum=$forum&table=$table&category=$category"
}













