# PS-2 Written by Shyam Visweswaran at ADUni, 22 April 2001
# admin-content-del.tcl - delete a row in the contents table
# target for: admin-content-edit.tcl; incoming argument: forum, table, entire_del_id or del_id, table_type, parent_id

# test first for a valid session_id
#set session_id [get_session_id]
#if { $session_id == 0 } { ns_returnredirect "../../relogin.tcl" }

# test next for a valid user_id
#set viewer_id [get_viewer_id $session_id]
#if { $viewer_id == 0 } { ns_returnredirect "../../relogin.tcl" }

# test for admin status
#if { [is_user_in_group $viewer_id admin] == 0 } { ns_returnredirect "../../relogin.tcl" }

set forum [ns_queryget forum]
set table [ns_queryget table]
set parent_id [ns_queryget parent_id]

if { [ns_queryexists entire_del_id] } {
    set sql_query "delete from $table
        where content_id = [ns_queryget entire_del_id]"
    if { [catch {db_dml delete_content $sql_query} result] } {
	ns_returnredirect "admin-content.tcl?forum=$forum&table=$table"
    } else {
	ns_returnredirect "admin-content.tcl?forum=$forum&table=$table"
    }

} elseif { [ns_queryexists del_id] } {
    set sql_query "delete from $table
        where content_id = [ns_queryget del_id]"
    if { [catch {db_dml delete_content $sql_query} result] } {
	ns_returnredirect "admin-content-edit.tcl?forum=$forum&table=$table&edit_id=$parent_id"
    } else {
	ns_returnredirect "admin-content-edit.tcl?forum=$forum&table=$table&edit_id=$parent_id"
    }
}





