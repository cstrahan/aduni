##########################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 23 April 2001
# article-comment.tcl - display the article title and a form to enter a comment
# target for: article-show.tcl; incoming argument: content_id

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "../login.tcl" }

# test next for a valid user_id
set viewer_id [get_viewer_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "../relogin.tcl" }

# Now get hold of the content_id
if { [ns_queryexists content_id] && [ns_queryget content_id] != "" } {
    set content_id [ns_queryget content_id]
} else {
    ns_return 200 text/plain "There was a problem. No content_id was obtained."
    return
}

set page_content "
[html_head "Comment"]
[html_body_start]
[html_body_top "Comment" "<a href=\"../home.tcl\">Home</a> > <a href=\"article.tcl\">Articles</a> > Comment"]
"

set sql_query "select title from articles where content_id = :content_id"

if { [db_0or1row get_question $sql_query] == 0} {
    ns_return 200 text/plain "There was a problem. We couldn't locate the article you want to comment on."
    return
}

set new_content_id [db_string next_value "select content_seq.nextval from dual"]
db_release_unused_handles

append page_content "
<!-- stuff for body of page -->

<table width=90%>
<tr><td>

<h3>Add a comment</h3>
to <b>$title</b>
<p>
<form method=post action=article-comment-confirm.tcl>
<input type=hidden name=viewer_id value=$viewer_id>
<input type=hidden name=parent_id value=$content_id>
<input type=hidden name=content_id value=$new_content_id>

<p>

<textarea name=new_content rows=8 cols=70 wrap=physical></textarea>
<p>

<input type=submit value=Submit>
</form>

</td></tr>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"

ns_return 200 text/html $page_content









