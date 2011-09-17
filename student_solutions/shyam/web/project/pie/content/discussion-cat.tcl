# PS-2 Written by Shyam Visweswaran at ADUni, 23 April 2001
# discussion-cat.tcl - display questions that belong to a particular category
# target for: discussion.tcl; incoming argument: forum, table, category

set forum [ns_queryget forum]
set table [ns_queryget table]
set category [ns_queryget category]

set page_content "
[html_head "One category"]
[html_body_start]
[html_body_top "One category" "<a href=\"../home.tcl\">Home</a> > <a href=discussion.tcl?forum=$forum>$forum Q and A</a> > One category "]

<!-- stuff for body of page -->

<table width=90%>
<tr><td>
"

if { [string equal $category "Uncategorized"] } {
    
    append page_content "<h3>Uncategorized</h3> <ul>"
    set sql_query "select d.content_id, d.title, u.name_prefix, u.first_names, u.last_name, u.email, d.posting_user,
       to_char(d.posting_time, 'Month dd, yyyy') \"posting_time\"
       from $table d, users u
       where d.parent_id is NULL
       and category is NULL
       and u.user_id = d.posting_user
       order by d.posting_time desc"

} else {
    
    append page_content "<h3>$category</h3> <ul>"
    set sql_query "select d.content_id, d.title, u.name_prefix, u.first_names, u.last_name, u.email, d.posting_user,
       to_char(d.posting_time, 'Month dd, yyyy') \"posting_time\"
       from $table d, users u
       where d.parent_id is NULL
       and category = :category
       and u.user_id = d.posting_user
       order by d.posting_time desc"

}

db_foreach get_articles $sql_query {
    append page_content "<li> <a href=discussion-show.tcl?forum=$forum&table=$table&content_id=$content_id>$title</a>
    by <a href=\"../display_user_info.tcl?forum=$forum&table=$table&display_id=$posting_user\">$name_prefix $first_names $last_name</a> ($posting_time)\n"
} if_no_rows {
    append page_content "No new questions"
}

append page_content "
</ul>
</tr></td>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"
ns_return 200 text/html $page_content

