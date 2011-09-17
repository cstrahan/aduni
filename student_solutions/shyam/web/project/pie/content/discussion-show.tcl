# PS-2 Written by Shyam Visweswaran at ADUni, 23 April 2001
# discussion-show.tcl - display a question and responses to it
# target for: discussion.tcl; incoming argument: forum, table, content_id

set forum [ns_queryget forum]
set table [ns_queryget table]
set content_id [ns_queryget content_id]

set page_content "
[html_head "One question"]
[html_body_start]
[html_body_top "One question" "<a href=\"../home.tcl\">Home</a> > <a href=\"discussion.tcl?forum=$forum\">Q and A $forum</a> > One question "]
"

set sql_query "select u.name_prefix, u.first_names, u.last_name, d.title, d.content, d.posting_user,
    to_char(d.posting_time, 'Month dd, yyyy') \"posting_time\"
    from $table d, users u
    where d.content_id = :content_id
    and u.user_id = d.posting_user"

if { [db_0or1row get_question $sql_query] == 0} {
    ns_return 200 text/plain "There was a problem. We couldn't locate the discussion you are looking for."
    # stop the execution of this script
    return
}

append page_content "
<!-- stuff for body of page -->

<table width=90%>
<tr><td>
<b>$title</b>
<blockquote>
$content
<p>
--  <a href=\"../display_user_info.tcl?forum=$forum&table=$table&display_id=$posting_user\">$name_prefix $first_names $last_name</a> ($posting_time)
</blockquote>

<b>Answers</b>
</td></tr>
</table>
"


set sql_query "select d.content, u.name_prefix, u.first_names, u.last_name, d.posting_user,
    to_char(d.posting_time, 'Month dd, yyyy') \"posting_time\"
    from $table d, users u
    where d.parent_id = :content_id
    and u.user_id = d.posting_user
    order by d.posting_time desc"

set counter 0
db_foreach get_articles $sql_query {
    set counter [expr $counter + 1]
    if { [expr $counter % 2] == 0 } {
	append page_content "<table cellpadding=10 width=90%>
	<tr><td>"
    } else {
	append page_content "<table cellpadding=10 width=90%>
	<tr bgcolor=#eeeeee><td>"
    }
    append page_content "
    <blockquote>
    $content
    <p>
    -- <a href=\"../display_user_info.tcl?forum=$forum&table=$table&display_id=$posting_user\">$name_prefix $first_names $last_name</a> ($posting_time)
    </blockquote>
    </td></tr>
    </table>
"
} if_no_rows {
    append page_content "<table cellpadding=10 width=90%>
    <tr bgcolor=#eeeeee><td>
    <blockquote>
    No answers yet.
    </blockquote>
    </td></tr>
    </table>"
}

append page_content "<table width=90%>
<tr><td><blockquote>
<form method=post action=discussion-reply.tcl>
<input type=hidden name=forum value=\"$forum\">
<input type=hidden name=table value=\"$table\">
<input type=hidden name=content_id value=$content_id>
<input type=submit value=\"Contribute an answer\">
</form>
</blockquote>
</tr></td>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"
ns_return 200 text/html $page_content





