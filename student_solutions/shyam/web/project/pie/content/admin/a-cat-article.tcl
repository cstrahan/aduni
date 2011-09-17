# PS-2 Written by Shyam Visweswaran at ADUni, 22 April 2001
# admin-cat-article.tcl - display articles that belong to a particular category
# provide ability to rename category
# target for: admin-content.tcl; incoming argument: category

set category [ns_queryget category]

set page_content "
[html_page_header 2 "One category" "<a href=\"../../admin/admin.tcl\">Administration</a> > <a href=a-content.tcl>Administer articles</a> > One category"]

<!-- stuff for body of page -->

<table width=90%>
<tr><td>
"

if { ![string match $category "Uncategorized"] } {
    append page_content "
    <form method=post action=a-cat-rename.tcl>
    <input type=hidden name=old value=\"$category\">
    Name:  
    <input type=text name=category size=70 maxsize=300 value=\"$category\">
    <input type=submit value=\"Update category name\">
    </form>"
}

if { [string equal $category "Uncategorized"] } {
    
    append page_content "<h3>Uncategorized</h3> <ul>"
    set sql_query "select a.content_id, a.title, u.name_prefix, u.first_names, u.last_name, u.email, a.posting_user,
       to_char(a.posting_time, 'Month dd, yyyy') \"posting_time\"
       from articles a, users u
       where a.parent_id is NULL
       and category is NULL
       and u.user_id = a.posting_user
       order by a.posting_time desc"

} else {
    
    append page_content "<h3>$category</h3> <ul>"
    set sql_query "select a.content_id, a.title, u.name_prefix, u.first_names, u.last_name, u.email, a.posting_user,
       to_char(a.posting_time, 'Month dd, yyyy') \"posting_time\"
       from articles a, users u
       where a.parent_id is NULL
       and category = :category
       and u.user_id = a.posting_user
       order by a.posting_time desc"

}

db_foreach get_articles $sql_query {
    append page_content "<li> <a href=a-content-edit.tcl?edit_id=$content_id>$title</a>
    by <a href=\"../display_user_info.tcl?display_id=$posting_user\">$first_names $last_name</a>
    ($posting_time)\n"
} if_no_rows {
    append page_content "No new articles"
}

append page_content "
</ul>
</tr></td>
</table>

[html_page_footer 2]   
"
ns_return 200 text/html $page_content



