##########################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 19 April 2001
# unanswered-questions.tcl - list all unanswered questions
# target for: discussion.tcl
# arguments: none

set forum [ns_queryget "forum"]
set table [ns_queryget "table"]

set page_content "
[html_head "All Unanswered Questions"]
[html_body_start]
[html_body_top "All Unanswered Questions" "<a href=\"../home.tcl\">Home</a> > <a href=\"discussion.tcl?forum=$forum\">$forum Q and A</a> > All Unanswered Questions"]

<!-- stuff for body of page -->

<table>
<tr><td>
<ul>
"

set sql_query "select d.content_id, d.title, d.posting_user, u.name_prefix, u.first_names, u.last_name,
    to_char(d.posting_time, 'Month dd, yyyy') \"posting_time\"
    from $table d, users u
    where content_id NOT IN
    (select d1.content_id from $table d1, $table d2 where d1.content_id = d2.parent_id)
    and u.user_id = d.posting_user
    and d.parent_id is NULL"

db_foreach get_articles $sql_query {
    append page_content "<li> <a href=discussion-show.tcl?forum=$forum&table=$table&content_id=$content_id><b>$title</b></a>
    by <a href=\"../display_user_info.tcl?forum=$forum&table=$table&display_id=$posting_user\">$name_prefix $first_names $last_name</a>
    (posted on $posting_time)"
} if_no_rows {
    append page_content "No unanswered questions"
}

append page_content "</ul></td></tr>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"

ns_return 200 text/html $page_content









