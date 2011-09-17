######################################################################
# PS-2 Written by Shyam Visweswaran at ADUni, 19 April 2001
# article-reply-confirm.tcl - insert non-empty new article
# target for: article-new.tcl, this script; incoming arguments: viewer_id, new_title, new_content

set_the_usual_form_variables
set new_title [string trim $new_title]
set new_content [string trim $new_content]
set current_cat $category

# Lets do some error checking on information typed by user.
if { ![info exists new_title] || [string equal $new_title ""] } {
    append error_text "<li><font color=red>you forgot to type a title for your article.</font><br>"
}

if { ![info exists new_content] || [string equal $new_content ""] } {
    append error_text "<li><font color=red>you forgot to type your article.</font><br>"
}

if { ![info exists error_text] } {
#    set new_content "<html><head></head><body><pre>$new_content</pre></body></html>"
    set sql_insert "insert into articles
       (content_id, title, content, category, posting_user, posting_time)
       values
       (:content_id, :new_title, :new_content, :category, :viewer_id, sysdate)"

    if { [catch {db_dml insert_message $sql_insert} result] } {
	ns_returnredirect "article.tcl"
    } else {
	ns_returnredirect "article.tcl"
    }	

} else {
    # We caught some errors. Reshow the pre-filled form with error messages.
    set page_content "
    [html_head "Post a new article"]
    [html_body_start]
    [html_body_top "Post a new article" "<a href=\"../home.tcl\">Home</a> > <a href=\"article.tcl\">Articles</a> > Post a new article"]

    <!-- stuff for body of page -->   

    <table>
    <tr><td>
    We were unable to process your information because
    <ul>
    $error_text
    </ul>

    <form method=post action=article-new-confirm.tcl>
    <input type=hidden name=viewer_id value=$viewer_id>
    <input type=hidden name=content_id value=$content_id>

    <b>Title of your article</b>
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

    set sql_query "select distinct category from articles"
    
    db_foreach get_categories $sql_query {
	if { [string match $category $current_cat] } {
	    append page_content "<option value = \"$category\" selected>$category</option>"
	} else {
	    append page_content "<option value = \"$categorys\">$category</option>"
	}
    } if_no_rows {
	append page_content ""
    }

    append page_content "
    </select>
    
    <p>
    <b>Article</b>
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




