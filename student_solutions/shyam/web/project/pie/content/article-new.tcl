########################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 19 April 2001
# article-new.tcl - display form to enter a new article or upload a new article
# target for: article.tcl; incoming argument: none

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "article.tcl" }

# test next for a valid user_id
set viewer_id [get_viewer_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "article.tcl" }

set page_content "
[html_head "Post a new article"]
[html_body_start]
[html_body_top "Post a new article" "<a href=\"../home.tcl\">Home</a> > <a href=\"article.tcl\">Articles</a> > Post a new article"]
"

set content_id [db_string next_value "select content_seq.nextval from dual"]
db_release_unused_handles

append page_content "
<!-- stuff for body of page -->

<table>
<tr><td>

<form method=post action=article-new-confirm.tcl>
<input type=hidden name=viewer_id value=$viewer_id>
<input type=hidden name=content_id value=$content_id>

<b>Title of your article</b>
<br>
<input type=text name=new_title size=70>
<p>

<b>Choose best category for your question</b><br>
<select type=text name=category>
<option value=\"\" selected>\[Uncategorized\]</option>
"

set sql_query "select distinct category from articles"

db_foreach get_categories $sql_query {
    append page_content "<option value = \"$category\">$category</option>"
} if_no_rows {
    append page_content ""
}

append page_content "
</select>
<p>

<b>Article</b>
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
