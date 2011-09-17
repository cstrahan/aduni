##########################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 19 April 2001
# new-answers.tcl - list all questoins that have recent answers
# target for: discussion.tcl
# arguments: forum, table

set forum [ns_queryget "forum"]
set table [ns_queryget "table"]

set number_of_days 7

set page_content "
[html_head "Recent Answers"]
[html_body_start]
[html_body_top "Recent Answers" "<a href=\"../home.tcl\">Home</a> > <a href=\"discussion.tcl?forum=$forum\">$forum Q and A</a> > Recent Answers"]

<!-- stuff for body of page -->

<table>
<tr><td>
<ul>
"

set sql_query "select distinct d3.content_id, d3.title,
    GREATEST(to_char(d4.posting_time, 'Month dd, yyyy')) \"posting_time\"
    from $table d3, $table d4
    where d3.content_id IN
    (select d1.content_id from $table d1, $table d2 where d1.content_id = d2.parent_id)
    and d3.parent_id is NULL
    and d4.parent_id is NOT NULL
    and d4.parent_id = d3.content_id"

db_foreach get_articles $sql_query {
    append page_content "<li> <a href=\"discussion-show.tcl?forum=$forum&table=$table&content_id=$content_id\">$title</a>
    (most recent answer posted on $posting_time)"
} if_no_rows {
    append page_content "No new answers"
}

append page_content "</ul>
</td></tr>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"

ns_return 200 text/html $page_content









