# Written by Shyam Visweswaran at ADUni, 27 April 2001
# display.tcl - display results for a particular person
# target for: search-names.tcl, search-mgh.tcl; incoming argument: id

set id [ns_queryget id]



set sql_query "select first_names, last_name,
 months_between(sysdate, birth_date)/12 \"age\", to_char(birth_date, 'dd Month, yyyy') \"birth_date\"
 from names where id = :id"

if { [db_0or1row get_names $sql_query] == 0 } {

    ns_return 200 text/html "Unable to get the person you are looking for."

} else {

    set page_content "
    [html_page_header 1 "Member information" "<a href=main.tcl>Main</a> > Member information"]

    <!-- stuff for body of page -->
    <table width=90%>
    <tr><td colspan=2>

    <b>$last_name, $first_names</b> &nbsp &nbsp Date of birth: <b>$birth_date</b> &nbsp &nbsp Current age: <b>[expr int($age)]</b>
    </td></tr>

    <tr>
    <td><b>Gail Assessment</b></td>
    <td><b>Heart Assessment</b></td></tr>
    "

    set sql_search "select distinct n.id, g.gail_id, to_char(g.creation_date, 'Month dd, yyyy, hh: mm am') \"gail_date\",
    to_char(g.edit_date, 'Month dd, yyyy hh:mi am') \"gail_edit_date\",
    g.race, g.age_at_menarche, g.age_at_first_live_birth, g.relatives_with_cancer, g.biopsy_count, g.biopsy_hyperplasia,
    h.heart_id, to_char(h.creation_date, 'Month dd, yyyy, hh: mm am') \"heart_date\",
    to_char(h.edit_date, 'Month dd, yyyy hh:mi am') \"heart_edit_date\",
    h.total_cholesterol, h.hdl_cholesterol, h.systolic_bp, h.diabetes, h.smoker
    from names n, gail g, heart h
    where n.id = g.id(+) and n.id = h.id(+) and n.id = :id"

    db_foreach get_hits $sql_search {

	if { [info exists current_gail_id] && [string equal $current_gail_id $gail_id] } {
	    append page_content "<td>&nbsp</td>"
	} elseif { [string equal $gail_id ""] } {
	    append page_content "<td> - </td>"
	} else {
	    append page_content "<tr><td>
	
	    Created: <b>$gail_date</b><br>
	    Last edited: <b>$gail_edit_date</b>
	    <p>
	    Race: <b>$race</b><br>
	    Age of first menstrual period: <b>$age_at_menarche</b><br>
	    Age at birth of first child: <b>$age_at_first_live_birth</b><br>
	    Number of relatives with breast cancer: <b>$relatives_with_cancer</b><br>
	    Number of breast biopsies: <b>$biopsy_count</b><br>
	    Biopsy with atypical hyperplasia? <b>$biopsy_hyperplasia</b><br>
	    <hr align=left size=1 width=100>
	    </td>"
	}
	
	if { [info exists current_heart_id] && [string equal $current_heart_id $heart_id] } {
	    append page_content "<td>&nbsp</td>"
	} elseif { [string equal $heart_id ""] } {
	    append page_content "<td> - </td>"
	} else {
	    append page_content "
	    <td>
	    Created: <b>$heart_date</b><br>
	    Last edited: <b>$heart_edit_date</b>
	    <p>
	    Total cholesterol: <b>$total_cholesterol</b><br>
	    HDL cholesterol: <b>$hdl_cholesterol</b><br>
	    Systolic blood pressure: <b>$systolic_bp</b><br>
	    Diabetes? <b>$diabetes</b><br>
	    Smoker? <b>$smoker</b><br>
	    <hr align=left size=1 width=100>
	    </td></tr>"
	}

    } if_no_rows {
	
	append page_content "<tr><td colspan=2>Unable to get details of the person you are looking for.</td></tr>"
	
    }
}

append page_content "
</table>

[html_page_footer 1]   
"
    
ns_return 200 text/html $page_content

