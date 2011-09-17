######################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 18 April 2001
# article-comment-confirm.tcl - insert non-empty comment for an article
# target for: article-comment.tcl, this script; incoming arguments: content_id, viewer_id, parent_id, new_content

set_the_usual_form_variables
set new_content [string trim $new_content]

# Lets do some error checking on information typed by user.
if { ![info exists new_content] || [string equal $new_content ""] } {
    append error_text "<li><font color=red>you forgot to type your comment.</font><br>"
}

if { ![info exists error_text] } {
    # At this point the user's input is reasonably good. Lets insert the reply into db.
    set sql_insert "insert into articles
       (content_id, content, parent_id, posting_user, posting_time)
       values
       (:content_id, :new_content, :parent_id, :viewer_id, sysdate)"

    if { [catch {db_dml insert_message $sql_insert} result] } {
	ns_returnredirect "article-show.tcl?content_id=$parent_id"
    } else {
	ns_returnredirect "article-show.tcl?content_id=$parent_id"
    }	

} else {
    # We caught some errors.
    set page_content "
    [html_head "Comment"]
    [html_body_start]
    [html_body_top "Comment" "<a href=\"../home.tcl\">Home</a> > <a href=\"article.tcl\">Articles</a> > Comment"]
    "

    # Grab the title of article from the db.
    set sql_query "select title from articles where content_id = :parent_id"

    if { [db_0or1row get_question $sql_query] == 0} {
	ns_return 200 text/plain "There was a problem. We couldn't locate the article you want to comment on."
	return
    }

    append page_content "
    <!-- stuff for body of page -->
    
    <table width=90%>
    <tr><td>

    We were unable to process your information because
    <ul>
    $error_text
    </ul
>
    <h3>Add a comment</h3>
    to <b>$title</b>
    <p>
    <form method=post action=article-comment-confirm.tcl>
    <input type=hidden name=viewer_id value=$viewer_id>
    <input type=hidden name=parent_id value=$parent_id>
    <input type=hidden name=content_id value=$content_id>
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
}

