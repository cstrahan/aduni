# PS-2 Written by Shyam Visweswaran at ADUni, 22 April 2001
# admin-cat-disc.tcl - display questions that belong to a particular category
# provide ability to rename category
# target for: admin-content.tcl; incoming argument: forum, table, category

set forum [ns_queryget forum]
set table [ns_queryget table]
set category [ns_queryget category]

set page_content "
[html_page_header 2 "One category" "<a href=\"../../admin/admin.tcl\">Administration</a> > <a href=admin-content.tcl?forum=$forum>Administer $forum Q and A</a> > One category"]

<!-- stuff for body of page -->

<table width=90%>
<tr><td>
"

if { ![string match $category "Uncategorized"] } {
    append page_content "
    <form method=post action=admin-cat-rename.tcl>
    <input type=hidden name=forum value=$forum>
    <input type=hidden name=table value=$table>
    <input type=hidden name=old value=\"$category\">
    Name:  
    <input type=text name=category size=70 maxsize=300 value=\"$category\">
    <input type=submit value=\"Update category name\">
    </form>"
}

if { [string equal $category "Uncategorized"] } {
    
    append page_content "<h3>Uncategorized</h3> <ul>"
    set sql_query "select d.content_id, d.title, u.name_prefix, u.first_names, u.last_name, u.email, d.posting_user,
       to_char(d.posting_time, 'Month dd, yyyy') \"posting_time\"
       from $table d, users u
       where d.parent_id is NULL
       and category is NULL
       and u.user_id = d.posting_user
       order by d.posting_time desc"

} else {
    
    append page_content "<h3>$category</h3> <ul>"
    set sql_query "select d.content_id, d.title, u.name_prefix, u.first_names, u.last_name, u.email, d.posting_user,
       to_char(d.posting_time, 'Month dd, yyyy') \"posting_time\"
       from $table d, users u
       where d.parent_id is NULL
       and category = :category
       and u.user_id = d.posting_user
       order by d.posting_time desc"

}

db_foreach get_articles $sql_query {
    append page_content "<li> <a href=admin-content-edit.tcl?forum=$forum&table=$table&edit_id=$content_id>$title</a>
    by <a href=\"../../display_user_info.tcl?display_id=$posting_user\">$name_prefix $first_names $last_name</a>
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




