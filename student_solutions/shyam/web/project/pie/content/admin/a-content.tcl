##########################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 20 April 2001
# a-content.tcl - display articles/questions and categories to admin
# arguments: none

# test first for a valid session_id
# set session_id [get_session_id]
# if { $session_id == 0 } { ns_returnredirect "login.tcl" }

# test next for a valid user_id
# set user_id [get_user_id $session_id]
# if { $user_id == 0 } { ns_returnredirect "login.tcl" }

set page_content "
[html_page_header 2 "Administer articles" "<a href=\"../../admin/admin.tcl\">Administration</a> > Administer articles"]

<!-- stuff for body of page -->

<table width=90%>
<tr><td>

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
    append page_content "<li> <a href=a-content-edit.tcl?edit_id=$content_id>$title</a><br>
    &nbsp &nbsp from <a href=\"../../display_user_info.tcl?display_id=$posting_user\">$name_prefix $first_names $last_name</a>
    ($posting_time)\n"
} if_no_rows {
    append page_content "No new articles"
}

append page_content "
</ul>
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
	append page_content "<li><a href=\"a-cat-article.tcl?category=Uncategorized\"><b>Uncategorized</b></a> ($cat_count)"
    } else {
	append page_content "<li><a href=\"a-cat-article.tcl?&category=$category\"><b>$category</b></a> ($cat_count)"
    }
} if_no_rows {
    append page_content "No categorized articles"
}

db_release_unused_handles

append page_content "
</ul>
</td></tr>
</table>

[html_page_footer 2]  
"

ns_return 200 text/html $page_content










