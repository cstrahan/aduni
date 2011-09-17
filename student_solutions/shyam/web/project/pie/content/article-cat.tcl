# PS-2 Written by Shyam Visweswaran at ADUni, 23 April 2001
# article-cat.tcl - display articles that belong to a particular category
# target for: article.tcl; incoming argument: category

set category [ns_queryget category]

set page_content "
[html_head "One category"]
[html_body_start]
[html_body_top "One category" "<a href=\"../home.tcl\">Home</a> > <a href=article.tcl>Articles</a> > One category "]

<!-- stuff for body of page -->

<table width=90%>
<tr><td>
"

if { [string equal $category "Uncategorized"] } {
    
    append page_content "<h3>Uncategorized</h3> <ul>"
    set sql_query "select a.content_id, a.title, u.name_prefix, u.first_names, u.last_name, u.email, a.posting_user,
       to_char(a.posting_time, 'Month dd, yyyy') \"posting_time\"
       from articles a, users u
       where a.parent_id is NULL
       and category is NULL
       and u.user_id = a.posting_user
       order by a.posting_time desc"

} else {
    
    append page_content "<h3>$category</h3> <ul>"
    set sql_query "select a.content_id, a.title, u.name_prefix, u.first_names, u.last_name, u.email, a.posting_user,
       to_char(a.posting_time, 'Month dd, yyyy') \"posting_time\"
       from articles a, users u
       where a.parent_id is NULL
       and category = :category
       and u.user_id = a.posting_user
       order by a.posting_time desc"

}

db_foreach get_articles $sql_query {
    append page_content "<li> <a href=article-show.tcl?content_id=$content_id>$title</a>
    by <a href=\"../display_user_info.tcl?display_id=$posting_user\">$name_prefix $first_names $last_name</a>
    ($posting_time) \n"
} if_no_rows {
    append page_content "No new articles"
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

