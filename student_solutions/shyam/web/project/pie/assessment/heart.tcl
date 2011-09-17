# Adopted by Therese Hendricks from gail.tcl written by Shyam Visweswaran at ADUni, 25 April 2001
# heart.tcl - the Heart Assessment form
# target for: add-name.tcl; incoming arguments: name_id

set id [ns_queryget "id"]
set sql_query "select months_between(sysdate, birth_date)/12 \"age\", to_char(birth_date, 'dd Month, yyyy') \"birth_date\",
    first_names, last_name from names where id = :id"

if { [db_0or1row get_name_info $sql_query] == 0} {
    ns_return 200 text/plain "Sorry! Unable to get information for this person." 
    return

} else {

    set heart_id [db_string get_heart_id "select heart_seq.nextval from dual"]    

    set page_content "
    [html_page_header 1 "Heart Assessment" "<a href=main.tcl>Main</a> > Heart Assessment" "<a href=gail.tcl?id=$id><b>Gail Assessment<b></a>"]
    
    <!-- stuff for body of page -->
    <table width=90%>
    <tr><td>

    <b>Heart Assessment for $first_names $last_name</b>

    <form method=post action=heart-2.tcl>
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
    <input type=text name=total_cholesterol size=3>
    
    <li>What is your HDL cholesterol level (mg per dL \[mmol per L\])?
    &nbsp; &nbsp;
    <input type=text name=hdl_cholesterol size=3>
 
    <li>What is your systolic blood pressure (mm Hg)?
    &nbsp; &nbsp;
    <input type=text name=systolic_bp size=3>

    <li>Do you have diabetes (fasting glucose >140mg per dl \[7.8mmol per L\])?
    &nbsp; &nbsp;   
    <input type=radio name=diabetes value=yes> Yes
    &nbsp; &nbsp;
    <input type=radio name=diabetes value=no> No
    
    <li>Do you smoke (any smoking within past month)?
    &nbsp; &nbsp;
    <input type=radio name=smoker value=yes> Yes
    &nbsp; &nbsp;
    <input type=radio name=smoker value=no> No

    
    </ol>

    <p>
    &nbsp;  &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
    <input type=submit value=Submit> &nbsp; &nbsp;
    <input type=reset>
    
    </form>
    
    </tr></td>
    </table>

    [html_page_footer 1]       
    "
    ns_return 200 text/html $page_content
}



