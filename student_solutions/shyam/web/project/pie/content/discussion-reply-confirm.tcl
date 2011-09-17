######################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 23 April 2001
# discussion-reply-confirm.tcl - insert non-empty reply to a discussion question
# target for: discussion-reply.tcl, this script; incoming arguments: forum, table, viewer_id, parent_id, new_content

set_the_usual_form_variables
set new_content [string trim $new_content]

# Lets do some error checking on information typed by user.
if { ![info exists new_content] || [string equal $new_content ""] } {
    append error_text "<font color=red>You forgot to type your reply.</font><br>"
}

if { ![info exists error_text] } {
    # At this point the user's input is reasonably good. Lets insert the reply into db.
    set sql_insert "insert into $table
       (content_id, content, parent_id, posting_user, posting_time)
       values
       (:content_id, :new_content, :parent_id, :viewer_id, sysdate)"

    if { [catch {db_dml insert_message $sql_insert} result] } {
	ns_returnredirect "discussion-show.tcl?forum=$forum&table=$table&content_id=$parent_id"
    } else {
	ns_returnredirect "discussion-show.tcl?forum=$forum&table=$table&content_id=$parent_id"
    }	

    if {0} {
	# Now display a confirmation message to viewer.
	set page_content "
	[html_head "Response confirmation"]
	[html_body_start]
	[html_body_top "Response confirmation" "<a href=\"../home.tcl\">Home</a> > <a href=\"discussion.tcl?forum=$forum\">$forum Q and A</a> > Response confirmation"]
	
	<!-- stuff for body of page -->
	
	<table width=90%>
	<tr><td>
	
	Congratulations! You have posted successfully to the discussion forum.
	<p>
	<b>Your response:</b> $new_content
	
	</td></tr>
	</table>
	
	[html_body_bottom]
	[html_body_end]
	[html_foot]
	"
	
	ns_return 200 text/html $page_content
    }

} else {
    # We caught some errors.
    set page_content "
    [html_head "Response"]
    [html_body_start]
    [html_body_top "Response" "<a href=\"../home.tcl\">Home</a> > <a href=\"discussion.tcl?forum=$forum\">$forum Q and A</a> > Response"]
    "

    # Grab the original question from the db.
    set sql_query "select u.name_prefix, u.first_names, u.last_name, d.title, d.content, d.posting_time
        from $table d, users u
        where d.content_id = :parent_id
        and u.user_id = d.posting_user"
    if { [db_0or1row get_question $sql_query] == 0} {
	ns_return 200 text/plain "There was a problem. We couldn't locate the discussion you want to reply to."
	return
    }

    append page_content "
    <!-- stuff for body of page -->
    
    <table width=90%>
    <tr><td>

    We were unable to process your information. Please correct the following.
    <p>
    $error_text
    <p>

    <b>Original question: $title</b>
    <p>
    $content
    <p>

    <p>
    --  $name_prefix $first_names $last_name ($posting_time)
    <p>
    
    <form method=post action=discussion-reply-confirm.tcl>
    <input type=hidden name=forum value=\"$forum\">
    <input type=hidden name=table value=\"$table\">
    <input type=hidden name=viewer_id value=$viewer_id>
    <input type=hidden name=parent_id value=$parent_id>
    <input type=hidden name=content_id value=$content_id>
    
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
}











