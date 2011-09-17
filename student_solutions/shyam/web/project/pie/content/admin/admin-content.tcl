##########################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 20 April 2001
# admin-content.tcl - display articles/questions and categories to admin
# arguments: forum

set forum [ns_queryget forum]

set sql_query "select short_name from content_metadata
    where pretty_name like '$forum'"

set table [db_string get_table_name $sql_query]

# test first for a valid session_id
# set session_id [get_session_id]
# if { $session_id == 0 } { ns_returnredirect "../../relogin.tcl" }

# test next for a valid user_id
# set user_id [get_user_id $session_id]
# if { $user_id == 0 } { ns_returnredirect "../../relogin.tcl" }

set page_content "
[html_page_header 2 "Administer $forum" "<a href=\"../../admin/admin.tcl\">Administration</a> > Administer $forum"]

<!-- stuff for body of page -->

<table width=90%>
<tr><td>

\[ <b><a href=\"../discussion.tcl?forum=$forum\">Browse $forum Q and A</a></b> \]
<hr size=1>

<h3>Recent Questions</h3>
<ul>
"
set sql_query "select d.content_id, d.title, u.name_prefix, u.first_names, u.last_name, u.email, d.posting_user,
     to_char(d.posting_time, 'Month dd, yyyy') \"posting_time\"
     from $table d, users u
     where d.parent_id is NULL
     and u.user_id = d.posting_user
     order by d.posting_time desc"

db_foreach get_articles $sql_query {
    append page_content "<li> <a href=admin-content-edit.tcl?forum=$forum&table=$table&edit_id=$content_id>$title</a> 
    <br>
    &nbsp &nbsp from <a href=\"../../display_user_info.tcl?forum=$forum&table=$table&display_id=$posting_user\">$name_prefix $first_names $last_name</a>
    ($posting_time)\n"
} if_no_rows {
    append page_content "No new questions"
}

append page_content "
</ul>
<h3>Older Questions (by category)</h3>
<ul>
"
set sql_query "select category, count(*) \"cat_count\"
    from $table
    where parent_id is NULL
    group by category
    order by category"

db_foreach get_articles $sql_query {
    if { [string match $category ""] } {
	append page_content "<li><a href=\"admin-cat-disc.tcl?forum=$forum&table=$table&category=Uncategorized\"><b>Uncategorized</b></a> ($cat_count)"
    } else {
	append page_content "<li><a href=\"admin-cat-disc.tcl?forum=$forum&table=$table&category=$category\"><b>$category</b></a> ($cat_count)"
    }
} if_no_rows {
    append page_content "No categorized questions"
}

append page_content "
</ul>
</td></tr>
</table>

[html_page_footer 2] 
"
db_release_unused_handles

ns_return 200 text/html $page_content










