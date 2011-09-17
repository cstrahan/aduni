# PS-2 Written by Shyam Visweswaran at ADUni, 20 April 2001
# admin-thread-edit.tcl - display article/question/response in a prefilled form for editing
# target for: admin-content-edit.tcl; incoming argument: edit_id, parent_id, type

# test first for a valid session_id
#set session_id [get_session_id]
#if { $session_id == 0 } { ns_returnredirect "../login.tcl" }

# test next for a valid user_id
#set viewer_id [get_viewer_id $session_id]
#if { $viewer_id == 0 } { ns_returnredirect "../login.tcl" }

# test for admin status
#if { [is_user_in_group $viewer_id admin] == 0 } { ns_returnredirect "../login.tcl" }

set table_type [ns_queryget type]
set edit_id [ns_queryget edit_id]

set page_content "
[html_page_header 2 "Edit thread" "<a href=\"../../admin/admin.tcl\">Administration</a> > <a href=\"a-content.tcl\">Administer articles</a> > Edit thread"]
"

set sql_query_thread "select c.title, c.content, c.parent_id, c.posting_user, u.name_prefix, u.first_names, u.last_name, u.email,
    to_char(c.posting_time, 'Month dd, yyyy') \"posting_time\"
    from articles c, users u
    where c.content_id = :edit_id
    and u.user_id = c.posting_user"

if { [db_0or1row get_thread $sql_query_thread] == 0} {
    ns_return 200 text/plain "There was a problem. We couldn't locate the thread you are looking for. Go back and check
    if the thread still exists."
    # stop the execution of this script
    return
}
set prompt_message "<h3>Edit '$title'</h3>"

if { [string equal $title ""] } { ;# Note that :table is not allowed
    set sql_query_parent "select title from articles
        where content_id = :parent_id"
    
    if { [db_0or1row get_title $sql_query_parent] == 0} {
	ns_return 200 text/plain "There was a problem. We couldn't locate the thread you are looking for. Go back and check
	if the thread still exists."
    # stop the execution of this script
    return
    }
    set prompt_message "<h3>Edit response to '$title'</h3>"
}   

append page_content "
<!-- stuff for body of page -->

<table width=90%>
<tr><td colspan=2>

$prompt_message

<p>
Posted by $name_prefix $first_names $last_name ($posting_time)

<form method=post action=a-content-update.tcl>
<input type=hidden name=update_id value=$edit_id>
<input type=hidden name=parent_id value=$parent_id>

<textarea name=content rows=8 cols=70 wrap=physical>
$content
</textarea>
<p>

<input type=submit value=Submit>
</form>

</td></tr>
</table>

[html_page_footer 2]  
"

ns_return 200 text/html $page_content




