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
[html_head "Upload a new article"]
[html_body_start]
[html_body_top "Upload a new article" "<a href=\"../home.tcl\">Home</a> > <a href=\"article.tcl\">Articles</a> > Upload a new article"]
"

set content_id [db_string next_value "select content_seq.nextval from dual"]
db_release_unused_handles

append page_content "
<!-- stuff for body of page -->

<table>
<tr><td>

<form method=post action=article-upload.tcl enctype=\"multipart/form-data\">
<input type=hidden name=viewer_id value=$viewer_id>

<b>Title of your article</b>
<br>
<input type=text name=new_title size=70>
<input type=hidden name=viewer_id value=$viewer_id>
<input type=hidden name=content_id value=$content_id>
<p>


<b>Upload your article in TXT format</b>
<br>
<input type=file name=new_content accept=\"text/txt\">
<p>
<input type=submit value=\"Upload\">
</form>

</td></tr>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"

ns_return 200 text/html $page_content
