# PS-2 Written by Shyam Visweswaran at ADUni, 20 April 2001
# admin-content-edit.tcl - display article/question and corresponding responses in a prefilled form
# Ability to delete entire thread, one or more responses
# target for: admin-content.tcl; incoming argument: forum, table, edit_id

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
set edit_id [ns_queryget edit_id]

set page_content "
[html_page_header 2 "Edit content" "<a href=\"../../admin/admin.tcl\">Administration</a> > <a href=admin-content.tcl?forum=$forum>Administer $forum Q and A</a> > Edit content"]
"

set sql_query "select c.title, c.content_id, c.category \"current_cat\", c.content, c.posting_user, u.name_prefix, u.first_names, u.last_name,
     to_char(c.posting_time, 'Month dd, yyyy') \"posting_time\"
     from $table c, users u
     where c.content_id = :edit_id
     and u.user_id = c.posting_user"

if { [db_0or1row get_question $sql_query] == 0} {
    ns_return 200 text/plain "There was a problem. We couldn't locate the thread you are looking for."
    # stop the execution of this script
    return
}

append page_content "
<!-- stuff for body of page -->

<table cellpadding=5 cellspacing=5 width=90%>
<tr><td colspan=2>

<b>\[<a href=\"admin-content-del.tcl?forum=$forum&table=$table&entire_del_id=$edit_id\">Delete entire thread</a>\]</b>

<form method=post action=admin-title-update.tcl>
<input type=hidden name=forum value=$forum>
<input type=hidden name=table value=$table>
<input type=hidden name=update_id value=$edit_id>
<input type=text name=title size=75 maxsize=300 value=\"$title\">
<input type=submit value=\"Update title\">
</form>

<form method=post action=admin-cat-update.tcl>
<input type=hidden name=forum value=$forum>
<input type=hidden name=table value=$table>
<input type=hidden name=update_id value=$edit_id>
<select type=text name=category>
"

if { [string equal $current_cat ""] } {
    append page_content "<option value=\"\" selected>\[Uncategorized\]</option>"
} else {
    append page_content "<option value=\"\">\[Uncategorized\]</option>"
}

set sql_query "select distinct category from $table"

db_foreach get_categories $sql_query {
    if { [string match $category $current_cat] } {
	append page_content "<option value = \"$category\" selected>$category</option>"
    } else {
	append page_content "<option value = \"$category\">$category</option>"
    }
} if_no_rows {
    append page_content ""
}

append page_content "
</select>
<input type=submit name=submit_action value=\"Update category\">
&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; 
<input type=text size=30 name=new_category>
<input type=submit name=submit_action value=\"Type new category\">
</form>

</td></tr>

<tr bgcolor=#d9d9d9><td width=80%>
<p>
$content
<p>
Posted by <a href=\"../../display_user_info.tcl?display_id=$posting_user\">$first_names $last_name</a> ($posting_time)
<p>
</td>

<td>
<b>\[<a href=\"admin-thread-edit.tcl?forum=$forum&table=$table&edit_id=$edit_id\">Edit</a>\]</b>
</td>
"

set counter 0
set sql_query "select c.content_id, c.title, c.content, c.posting_user, u.name_prefix, u.first_names, u.last_name,
    to_char(c.posting_time, 'Month dd, yyyy') \"posting_time\"
    from $table c, users u
    where c.parent_id = :edit_id
    and u.user_id = c.posting_user
    order by c.posting_time"

db_foreach get_articles $sql_query {
    set counter [expr $counter + 1]
    if { [expr $counter % 2] == 0 } {
	append page_content "<tr bgcolor=#dbdbdb><td width=80%>"
    } else {
	append page_content "<tr><td width=80%>"
    }
    append page_content "
    <p>
    $content
    <p>
    Response by <a href=\"../../display_user_info.tcl?display_id=$posting_user\">$name_prefix $first_names $last_name</a> ($posting_time)
    <p>
    </td>

    <td><b>\[<a href=\"admin-thread-edit.tcl?forum=$forum&table=$table&edit_id=$content_id&parent_id=$edit_id\">Edit</a></b>
    <b> | <a href=\"admin-content-del.tcl?forum=$forum&table=$table&del_id=$content_id&parent_id=$edit_id\">Delete</a>\]</b>
    </td></tr>
"
} if_no_rows {
    append page_content ""
}

append page_content "
</table>

[html_page_footer 2]   
"
ns_return 200 text/html $page_content





