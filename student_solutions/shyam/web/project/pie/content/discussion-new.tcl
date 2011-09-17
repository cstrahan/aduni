########################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 18 April 2001
# discussion-new.tcl - display form to enter a new question
# target for: discussion.tcl; incoming argument: forum, table

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "../relogin.tcl" }

# test next for a valid user_id
set viewer_id [get_viewer_id $session_id]
if { $session_id == 0 } { ns_returnredirect "../relogin.tcl" }

set forum [ns_queryget "forum"]
set table [ns_queryget "table"]

set page_content "
[html_head "Post a new question"]
[html_body_start]
[html_body_top "Post a new question" "<a href=\"../home.tcl\">Home</a> > <a href=\"discussion.tcl?forum=$forum\">$forum Q and A</a> > Post a new question"]
"

set new_content_id [db_string next_value "select content_seq.nextval from dual"]
db_release_unused_handles

append page_content "
<!-- stuff for body of page -->

<table>
<tr><td>

<form method=post action=discussion-new-confirm.tcl>
<input type=hidden name=forum value=\"$forum\">
<input type=hidden name=table value=\"$table\">
<input type=hidden name=viewer_id value=$viewer_id>
<input type=hidden name=content_id value=$new_content_id>

<b>Summary of your question (in one line)</b>
<br>
<input type=text name=new_title size=70>
<p>

<b>Choose best category for your question</b><br>
<select type=text name=category>
<option value=\"\"  selected>\[Uncategorized\]</option>
"

set sql_query "select distinct category from $table"

db_foreach get_categories $sql_query {
    append page_content "<option value=\"$category\">$category</option>"
} if_no_rows {
    append page_content ""
}

append page_content "
</select>
<p>

<b>Please describe your question in detail</b>
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



