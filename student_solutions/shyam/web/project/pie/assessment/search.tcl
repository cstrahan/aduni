# Written by Shyam Visweswaran at ADUni, 25 April 2001
# search.tcl - search on names and display results
# target for: main.tcl; incoming argument: search

set_the_usual_form_variables

if { [string equal $action "Search names"] } {
    set search [string trim $search_names]
}

if { [string equal $search ""] } {
    ns_return 200 text/plain "Go back and enter a search term."
    # stop the execution of this script
    return
}

set s_search "%[string toupper $search]%"

set search_box "<form method=post action=search.tcl>
<input type=text name=search value=\"$search\" size=30 maxsize=200> &nbsp;
<input type=submit value=Search>
</form>"

append page_content "
[html_page_header 1 "Search results" "<a href=main.tcl>Main</a> > Search results" "" $search_box]

<!-- stuff for body of page -->
<table width=90%>
<tr><td><b>Name</b></td>
<td><b>Gail Assessment</b></td>
<td><b>Heart Assessment</b></td></tr>
"

set sql_search "select distinct n.id, n.first_names, n.last_name,
 g.gail_id, to_char(g.creation_date, 'Month dd, yyyy, hh: mm am') \"gail_date\",
 h.heart_id, to_char(h.creation_date, 'Month dd, yyyy, hh: mm am') \"heart_date\"
 from names n, gail g, heart h
 where (upper(n.first_names) like :s_search or upper(n.last_name) like :s_search)
 and n.id = g.id and n.id = h.id
 order by n.last_name, n.first_names"      

db_foreach get_hits $sql_search {

    if { [info exists current_id] && [string equal $current_id $id] } {
	append page_content "<tr><td>&nbsp</td>"
    } else {
	append page_content "<tr><td><a href=\"display.tcl?id=$id\">$last_name, $first_names</a>
	                     &nbsp <b>\[<a href=\"gail.tcl?id=$id\">New Gail</a>
	                     | <a href=\"heart.tcl?id=$id\">New Heart</a>\]</b></td>"
	set current_id $id
    }

    if { [info exists current_gail_id] && [string equal $current_gail_id $gail_id] } {
	append page_content "<tr><td>&nbsp</td>"
    } else {
	append page_content "<td>$gail_date <b>\[<a href=gail-edit.tcl?name_id=$name_id&gail_id=$gail_id>Edit</a>\]</b>"
        set current_gail_id $gail_id
    }

    if { [info exists current_heart_id] && [string equal $current_heart_id $heart_id] } {
	append page_content "<tr><td>&nbsp</td>"
    } else {
	append page_content "<td>$heart_date <b>\[<a href=heart-edit.tcl?name_id=$name_id&heart_id=$heart_id>Edit</a>\]</b>"
        set current_heart_id $heart_id
    }

} if_no_rows {

    append page_content "<tr><td>No such name(s) found.</td></tr>"

}

append page_content "
</table>

[html_page_footer 1]       
"
    
ns_return 200 text/html $page_content
