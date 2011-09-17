# Written by Shyam Visweswaran at ADUni, 25 April 2001
# search-names.tcl - search on names and display results
# target for: main.tcl; incoming argument: search_names

set_the_usual_form_variables

set search_names [string trim $search_names]
if { [string equal $search_names ""] } {
    ns_return 200 text/plain "You forgot to type a name. Please go back and type a name."
    # stop the execution of this script
    return
}

set search "%[string toupper $search_names]%"
set search "[ns_dbquotevalue $search]"

set search_box "<form method=post action=search-names.tcl>
<input type=text name=search_names value=\"$search_names\" size=30 maxsize=200> &nbsp;
<input type=submit value=\"Search names\">
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
 where (upper(n.first_names) like $search or upper(n.last_name) like $search)
 and n.id = g.id(+) and n.id = h.id(+)
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
	append page_content "<td>&nbsp</td>"
    } elseif { [string equal $gail_id ""] } {
	append page_content "<td> - </td>"
    } else {
	append page_content "<td>$gail_date <b>\[<a href=gail-edit.tcl?id=$id&gail_id=$gail_id>Edit</a>\]</b>"
        set current_gail_id $gail_id
    }

    if { [info exists current_heart_id] && [string equal $current_heart_id $heart_id] } {
	append page_content "<td>&nbsp</td>"
    } elseif { [string equal $heart_id ""] } {
	append page_content "<td> - </td>"
    } else {
	append page_content "<td>$heart_date <b>\[<a href=heart-edit.tcl?id=$id&heart_id=$heart_id>Edit</a>\]</b>"
        set current_heart_id $heart_id
    }
    
} if_no_rows {
    ns_return 200 text/plain "No person matching your query was found. Please go back and retype a new name."    
    # stop the execution of this script
    return
}

append page_content "
</table>

[html_page_footer 1]       
"

ns_return 200 text/html $page_content




