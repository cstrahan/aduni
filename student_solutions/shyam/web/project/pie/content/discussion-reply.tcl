##########################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 18 April 2001
# discussion-reply.tcl - display the question and a form to enter reply
# target for: discussion-show.tcl; incoming argument: forum, table, content_id

set_the_usual_form_variables

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "../relogin.tcl" }

# test next for a valid user_id
set viewer_id [get_viewer_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "../relogin.tcl" }

set page_content "
[html_head "Response"]
[html_body_start]
[html_body_top "Response" "<a href=\"../home.tcl\">Home</a> > <a href=\"discussion.tcl?forum=$forum\">$forum Q and A</a> > Response"]
"

set sql_query "select u.name_prefix, u.first_names, u.last_name, d.title, d.content,
    to_char(d.posting_time, 'Month dd, yyyy') \"posting_time\"
    from $table d, users u
    where d.content_id = :content_id
    and u.user_id = d.posting_user"

if { [db_0or1row get_question $sql_query] == 0} {
    ns_return 200 text/plain "There was a problem. We couldn't locate the discussion you want to reply to."
    return
}

set new_content_id [db_string next_value "select content_seq.nextval from dual"]
db_release_unused_handles

append page_content "
<!-- stuff for body of page -->

<table width=90%>
<tr><td>

<b>Original question: $title</b>
<p>
$content
<p>
--  $name_prefix $first_names $last_name ($posting_time)
<p>

<form method=post action=discussion-reply-confirm.tcl>
<input type=hidden name=forum value=\"$forum\">
<input type=hidden name=table value=\"$table\">
<input type=hidden name=viewer_id value=$viewer_id>
<input type=hidden name=parent_id value=$content_id>
<input type=hidden name=content_id value=$new_content_id>

<b>Your Response</b>
<br>
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









