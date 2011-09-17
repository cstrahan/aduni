##########################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 23 April 2001
# discussion.tcl - displays recent questions and categories containing older questions
# target for: admin.tcl, physician.tcl, user.tcl i.e. any user on site
# arguments: forum

# test first for a valid session_id
# set session_id [get_session_id]
# if { $session_id == 0 } { ns_returnredirect "login.tcl" }

# test next for a valid user_id
# set user_id [get_user_id $session_id]
# if { $user_id == 0 } { ns_returnredirect "login.tcl" }

set forum [ns_queryget forum]

set sql_query "select short_name from content_metadata
    where pretty_name like '$forum'"

set table [db_string get_table_name $sql_query]

set search_box "<form method=post action=\"../search.tcl\">
                  <input type=hidden name=forum value=$forum>
                  <input type=hidden name=table value=$table>
                  <input type=text size=20 name=discussions>
                  <input type=submit value=Search>"

append page_content "
[html_page_header 0 "Q and A" "<a href=\"../home.tcl\">Home</a> > $forum Q and A" "" $search_box]

<!-- stuff for body of page -->

<table width=90%>
<tr><td>
\[ <b><a href=discussion-new.tcl?forum=$forum&table=$table>Post a New Question</a>
 | <a href=unanswered-questions.tcl?forum=$forum&table=$table>All Unanswered Questions</a>
 | <a href=new-answers.tcl?forum=$forum&table=$table>Recent Answers</a>
 | About </b>\]
<hr size=1>
</td></tr>
"

set sql_query "select count(*) from $table
    where posting_time > (sysdate - 7)"

append page_content "
<tr><td>
<p>
Questions and answers posted in the last 7 days: <b>[db_string stats $sql_query]</b>
<p>

<h3>Recent Questions</h3>
<ul>
"

set sql_query "select d.content_id, d.title, u.name_prefix, u.first_names, u.last_name, d.posting_user,
    to_char(d.posting_time, 'Month dd, yyyy') \"posting_time\"
    from $table d, users u
    where d.parent_id is NULL
    and u.user_id = d.posting_user
    order by d.posting_time desc"

db_foreach get_articles $sql_query {
    append page_content "<li> <a href=discussion-show.tcl?forum=$forum&table=$table&content_id=$content_id>$title</a>
    by <a href=\"../display_user_info.tcl?forum=$forum&table=$table&display_id=$posting_user\">$name_prefix $first_names $last_name</a>
    ($posting_time) \n"
} if_no_rows {
    append page_content "No new questions"
}

append page_content "</ul>
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
	append page_content "<li><a href=\"discussion-cat.tcl?forum=$forum&table=$table&category=Uncategorized\"><b>Uncategorized</b></a> ($cat_count)"
    } else {
	append page_content "<li><a href=\"discussion-cat.tcl?forum=$forum&table=$table&category=$category\"><b>$category</b></a> ($cat_count)"
    }
} if_no_rows {
    append page_content "No classified questions"
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

