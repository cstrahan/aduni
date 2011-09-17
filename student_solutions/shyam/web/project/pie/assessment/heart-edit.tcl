# Adapted by Therese Hendricks from gail-edit code written by Shyam Visweswaran at ADUni, 25 April 2001
# heart-edit.tcl - edit information in the Heart Assessment form
# target for: search.tcl; incoming arguments: name_id

set id [ns_queryget id]
set heart_id [ns_queryget heart_id]

set sql_query "select n.first_names, n.last_name,
 months_between(sysdate, n.birth_date)/12 \"age\", to_char(n.birth_date, 'dd Month, yyyy') \"birth_date\",
 h.total_cholesterol, h.hdl_cholesterol, h.systolic_bp, h.diabetes, h.smoker,
 to_char(h.edit_date, 'Month dd, yyyy hh:mi am') \"edit_date\"
 from names n, heart h
 where n.id = :id and h.heart_id = :heart_id"

if { [db_0or1row get_info $sql_query] == 0 } {
    ns_return 200 text/html "Could not get the record"

} else {

    append page_content "
    [html_page_header 1 "Update Heart Assessment" "<a href=main.tcl>Main</a> > Update Heart Assessment"]
    
    <!-- stuff for body of page -->
    <table width=90%>
    <tr><td>

    Here is the information on the Heart Assessment for <b>$first_names $last_name</b> collected on $edit_date.
    <p>

    <form method=post action=heart-edit-2.tcl>
    <input type=hidden name=heart_id value=$heart_id>
    <input type=hidden name=id value=$id>
    <input type=hidden name=first_names value=\"$first_names\">
    <input type=hidden name=last_name value=\"$last_name\">
    <input type=hidden name=birth_date value=\"$birth_date\">
    <input type=hidden name=age value=[expr int($age)]>

    <ol>
    <li>
    Date of birth: $birth_date   &nbsp; &nbsp; &nbsp; &nbsp;   Age: [expr int($age)]
    
    <li>What is your total cholesterol level (mg per dL \[mmol per L\])?
    &nbsp; &nbsp;
    <input type=text name=total_cholesterol size=2 value=\"$total_cholesterol\">
    
    <li>What is your HDL cholesterol level (mg per dL \[mmol per L\])?
    &nbsp; &nbsp;
    <input type=text name=hdl_cholesterol size=2 value=\"$hdl_cholesterol\">
 
    <li>What is your systolic blood pressure (mm Hg)?
    &nbsp; &nbsp;
    <input type=text name=systolic_bp size=2 value=\"$systolic_bp\">
    
    <li>Do you have diabetes (fasting glucose >140mg per dl \[7.8mmol per L\])?
    "

    if { [string match $diabetes "yes"] } {
	append page_content "&nbsp; &nbsp; <input type=radio name=diabetes value=yes checked> Yes"
    } else {
	append page_content "&nbsp; &nbsp; <input type=radio name=diabetes value=yes> Yes"
    }

    if { [string match $diabetes "no"] } {
	append page_content "&nbsp; &nbsp; <input type=radio name=diabetes value=no checked> No"
    } else {
	append page_content "&nbsp; &nbsp; <input type=radio name=diabetes value=no> No"
    }
    
    append page_content "
    <li>Do you smoke (any smoking within past month)?
    "
    if { [string match $smoker "yes"] } {
	append page_content "&nbsp; &nbsp; <input type=radio name=smoker value=yes checked> Yes"
    } else {
	append page_content "&nbsp; &nbsp; <input type=radio name=smoker value=yes> Yes"
    }

    if { [string match $smoker "no"] } {
	append page_content "&nbsp; &nbsp; <input type=radio name=smoker value=no checked> No"
    } else {
	append page_content "&nbsp; &nbsp; <input type=radio name=smoker value=no> No"
    }


    append page_content "    
    </ol>

    <p>
    &nbsp;  &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
    <input type=submit> &nbsp; &nbsp;
    <input type=reset>
    
    </form>
    
    </tr></td>
    </table>

    [html_page_footer 1]       
    "
    ns_return 200 text/html $page_content
}

