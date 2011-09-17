######################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 19 April 2001
# discussion-new-confirm.tcl - insert non-empty reply to a discussion question into content db
# target for: discussion-new.tcl, this script; incoming arguments: forum, table, viewer_id, new_title, new_content

set_the_usual_form_variables
set new_title [string trim $new_title]
set new_content [string trim $new_content]
set current_cat $category

# Lets do some error checking on information typed by user.
if { [string equal $new_title ""] } {
    append error_text "<li><font color=red>you forgot to type a summary of your question.</font><br>"
}

if { [string equal $new_content ""] } {
    append error_text "<li><font color=red>you forgot to describe your question in detail.</font><br>"
}

if { ![info exists error_text] } {
    # At this point the user's input is reasonably good. Lets insert into db
    set sql_insert "insert into $table
       (content_id, title, content, category, posting_user, posting_time)
       values
       (:content_id, :new_title, :new_content, :category, :viewer_id, sysdate)"

    if { [catch {db_dml insert_message $sql_insert} result] } {
	ns_returnredirect "discussion.tcl?forum=$forum"
    } else {
	ns_returnredirect "discussion.tcl?forum=$forum"
    }	

} else {
    # We caught some errors. Reshow the pre-filled form with error messages.
    set page_content "
    [html_head "Post a new question"]
    [html_body_start]
    [html_body_top "Post a new question" "<a href=\"../home.tcl\">Home</a> > <a href=\"discussion.tcl?forum=$forum\">$forum Q and A</a> > Post a new question"]

    <!-- stuff for body of page -->   

    <table>
    <tr><td>
    We were unable to process your information because
    <ul>
    $error_text
    </ul>

    <form method=post action=discussion-new-confirm.tcl>
    <input type=hidden name=forum value=\"$forum\">
    <input type=hidden name=table value=\"$table\">
    <input type=hidden name=viewer_id value=$viewer_id>
    <input type=hidden name=content_id value=$content_id>
    
    <b>Summary of your question (in one line)</b> 
    <br>
    <input type=text name=new_title size=70 value=\"$new_title\">
    <p>
    
    <b>Choose best category for your question</b><br>
    <select type=text name=category>
    "

    if { [string equal $current_cat ""] } {
	append page_content "<option value=\"\" selected>\[Uncategorized\]</option>"
    } else {
	append page_content "<option value=\"\">\[Uncategorized\]</option>"
    }

    set sql_query "select distinct category from discussions"
    
    db_foreach get_categories $sql_query {
	if { [string match $category $current_cat] } {
	    append page_content "<option value = \"$category\" selected>$category</option>"
	} else {
	    append page_content "<option value = \"$category\">$category</option>"
	}
    } if_no_rows {
	append page_content ""
    }

    append page_content "
    </select>
    <p>

    <b>Please describe your question in detail</b>
    <br>
    <textarea name=new_content rows=8 cols=70 wrap=physical>$new_content</textarea>
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
}
