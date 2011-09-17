# Written by Shyam Visweswaran at ADUni, 25 April 2001
# gail-edit.tcl - edit information in the Gail Risk Assessment form
# target for: search.tcl; incoming arguments: name_id, gail_id

set id [ns_queryget id]
set gail_id [ns_queryget gail_id]

set sql_query "select n.first_names, n.last_name,
 months_between(sysdate, n.birth_date)/12 \"age\", to_char(n.birth_date, 'dd Month, yyyy') \"birth_date\",
 g.race, g.age_at_menarche, g.age_at_first_live_birth, g.relatives_with_cancer, g.biopsy_count, g.biopsy_hyperplasia,
 to_char(g.edit_date, 'Month dd, yyyy hh:mi am') \"edit_date\"
 from names n, gail g
 where n.id = :id and g.gail_id = :gail_id"

if { [db_0or1row get_info $sql_query] == 0 } {
    ns_return 200 text/html "Unable to get details of the person you are looking for."

} else {

    append page_content "
    [html_page_header 1 "Update Gail Assessment" "<a href=main.tcl>Main</a> > Update Gail Assessment"]
    
    <!-- stuff for body of page -->
    <table width=90%>
    <tr><td>

    Here is the information on the Gail Assessment for <b>$first_names $last_name</b> collected on $edit_date.
    <p>

    <form method=post action=gail-edit-2.tcl>
    <input type=hidden name=gail_id value=$gail_id>
    <input type=hidden name=id value=$id>
    <input type=hidden name=first_names value=\"$first_names\">
    <input type=hidden name=last_name value=\"$last_name\">
    <input type=hidden name=birth_date value=\"$birth_date\">
    <input type=hidden name=age value=[expr int($age)]>

    <ol>
    <li>
    Date of birth: $birth_date   &nbsp; &nbsp; &nbsp; &nbsp;   Age: [expr int($age)]

    <li>What is your race?
    "

    if { [string match $race "white"] } {
	append page_content "&nbsp; &nbsp; <input type=radio name=race value=white checked> White"
    } else {
	append page_content "&nbsp; &nbsp; <input type=radio name=race value=white> White"
    }

    if { [string match $race "black"] } {
	append page_content "&nbsp; &nbsp; <input type=radio name=race value=black checked> Black"
    } else {
	append page_content "&nbsp; &nbsp; <input type=radio name=race value=black> Black"
    }

    if { [string match $race "asian"] } {
	append page_content "&nbsp; &nbsp; <input type=radio name=race value=asian checked> Asian"
    } else {
    append page_content "&nbsp; &nbsp; <input type=radio name=race value=asian> Asian"
    }

    append page_content "    
    <li>How old were you when you had your first menstrual period?
    &nbsp; &nbsp;
    <input type=text name=age_at_menarche size=2 value=\"$age_at_menarche\">
    
    <li> How old were you when your first child was born? (If you never had a child, enter 0)
    &nbsp; &nbsp;
    <input type=text name=age_at_first_live_birth size=2 value=\"$age_at_first_live_birth\">
 
    <li> How many of your sisters, daughters or mother have had breast cancer?
    &nbsp; &nbsp;
    <input type=text name=relatives_with_cancer size=2 value=\"$relatives_with_cancer\">
    
    <li>How many breast biopsies have you had? (breast tissue removed to test for cancer)
    &nbsp; &nbsp;
    <input type=text name=biopsy_count size=2 value=\"$biopsy_count\">
    
    <li>Did you have a biopsy with atypical hyperplasia?
    "

    if { [string match $biopsy_hyperplasia "yes"] } {
	append page_content "&nbsp; &nbsp; <input type=radio name=biopsy_hyperplasia value=yes checked> Yes"
    } else {
	append page_content "&nbsp; &nbsp; <input type=radio name=biopsy_hyperplasia value=yes> Yes"
    }

    if { [string match $biopsy_hyperplasia "no"] } {
	append page_content "&nbsp; &nbsp; <input type=radio name=biopsy_hyperplasia value=no checked> No"
    } else {
	append page_content "&nbsp; &nbsp; <input type=radio name=biopsy_hyperplasia value=no> No"
    }
    
    if { [string match $biopsy_hyperplasia "unknown"] } {
	append page_content "&nbsp; &nbsp; <input type=radio name=biopsy_hyperplasia value=unknown checked> Unknown"
    } else {
	append page_content "&nbsp; &nbsp; <input type=radio name=biopsy_hyperplasia value=unknown> Unknown"
    }

    append page_content "    
    </ol>

    <p>
    &nbsp;  &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
    <input type=submit value=Update> &nbsp; &nbsp;
    <input type=reset>
    
    </form>
    
    </tr></td>
    </table>
    [html_page_footer 1]       
    "    
    ns_return 200 text/html $page_content
}

