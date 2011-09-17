# PS-2 Written by Shyam Visweswaran at ADUni, 23 April 2001
# article.tcl - display all expert articles by title and author

set page_content "
[html_head "Articles"]
[html_body_start]
"

set search_box "<form method=post action=\"../search.tcl\">
                  <input type=text size=20 name=articles>
                  <input type=submit value=Search>"


append page_content "
[html_page_header 0 "Articles" "<a href=\"../home.tcl\">Home</a> > Articles" "" $search_box]

<table width=90%>
<tr><td>
\[ <b><a href=article-new.tcl>Post a New Article</a>
 | Upload an Article</b> \]
<hr size=1>
</td></tr>
"

set sql_query "select count(*) from articles
    where posting_time > (sysdate - 7)"

append page_content "
<tr><td>
Articles and responses posted in the last 7 days: [db_string stats $sql_query]
<p>

<h3>Recent Articles</h3>
<ul>
"
set sql_query "select a.content_id, a.title, u.name_prefix, u.first_names, u.last_name, u.email, a.posting_user,
    to_char(a.posting_time, 'Month dd, yyyy') \"posting_time\"
    from articles a, users u
    where a.parent_id is NULL
    and u.user_id = a.posting_user
    order by a.posting_time desc"

db_foreach get_articles $sql_query {
    append page_content "<li> <a href=article-show.tcl?content_id=$content_id>$title</a>
    by <a href=\"../display_user_info.tcl?display_id=$posting_user\">$name_prefix $first_names $last_name</a>
    ($posting_time) \n"
} if_no_rows {
    append page_content "No new articles"
}

append page_content "</ul>
<h3>Older Articles (by category)</h3>
<ul>
"
set sql_query "select category, count(*) \"cat_count\"
    from articles
    where parent_id is NULL
    group by category
    order by category"

db_foreach get_articles $sql_query {
    if { [string match $category ""] } {
	append page_content "<li><a href=\"article-cat.tcl?category=Uncategorized\"><b>Uncategorized</b></a> ($cat_count)"
    } else {
	append page_content "<li><a href=\"article-cat.tcl?category=$category\"><b>$category</b></a> ($cat_count)"
    }
} if_no_rows {
    append page_content "No categorized articles"
}

db_release_unused_handles

append page_content "
</ul>
</td></tr>
</table>

[html_page_footer 0]   
"
db_release_unused_handles

ns_return 200 text/html $page_content








