# PS-2 Written by Shyam Visweswaran at ADUni, 23 April 2001
# article-show.tcl - display an article and responses to it
# target for: article.tcl; incoming argument: content_id

set content_id [ns_queryget content_id]

set page_content "
[html_head "One Article"]
[html_body_start]
[html_body_top "One Article" "<a href=\"../home.tcl\">Home</a> > <a href=\"article.tcl\">Articles</a> > One Article "]
"

set sql_query "select u.first_names, u.name_prefix, u.last_name, a.title, a.content, a.posting_user,
    to_char(a.posting_time, 'Month dd, yyyy') \"posting_time\"
    from articles a, users u
    where a.content_id = :content_id
    and u.user_id = a.posting_user"

if { [db_0or1row get_question $sql_query] == 0} {
    ns_return 200 text/plain "There was a problem. We couldn't locate the article you are looking for."
    # stop the execution of this script
    return
}

append page_content "
<!-- stuff for body of page -->

<table width=90%>
<tr><td>
<center><h3>$title</h3></center>
$content
<p>
-- <a href=\"../display_user_info.tcl?display_id=$posting_user\">$name_prefix $first_names $last_name</a> ($posting_time)
<p>

<center><h3>Comments</h3></center>
</td></tr>
</table>
"

set sql_query "select a.content, u.first_names, u.last_name, a.posting_user,
    to_char(a.posting_time, 'Month dd, yyyy') \"posting_time\"
    from articles a, users u
    where a.parent_id = :content_id
    and u.user_id = a.posting_user
    order by a.posting_time desc"

set counter 0
db_foreach get_articles $sql_query {
    set counter [expr $counter + 1]
    if { [expr $counter % 2] == 0 } {
	append page_content "<table cellpadding=10 width=90%>
	<tr><td>"
    } else {
	append page_content "<table cellpadding=10 width=90%>
	<tr><td bgcolor=#eeeeee>"
    }
    append page_content "
    <p>
    $content
    <p>
    -- <a href=\"../display_user_info.tcl?display_id=$posting_user\">$first_names $last_name</a>, $posting_time
    <p>
    </td></tr>
    </table>
"
} if_no_rows {
    append page_content ""
}

append page_content "<table width=90%>
<tr><td>
<center>
<form method=post action=article-comment.tcl>
<input type=hidden name=content_id value=$content_id>
<input type=submit value=\"Contribute a comment\">
</form>
</center>
</tr></td>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"
ns_return 200 text/html $page_content






