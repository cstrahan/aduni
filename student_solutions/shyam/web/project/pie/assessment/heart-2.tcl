# Adapted by Therese Hendricks from gail-2.tcl code written by Shyam Visweswaran at ADUni, 25 April 2001
# heart-2.tcl - processes the Heart Assessment form
# target for: heart.tcl

set smoker ""
set diabetes ""
set_the_usual_form_variables

set error_message ""
if { ![string is integer $total_cholesterol] } {
    append error_message "<li><font color=red>Total cholesterol has to be a number</font>"
}
if { ![string is integer $hdl_cholesterol] } {
    append error_message "<li><font color=red>HDL cholesterol has to be a number</font>"
}
if { ![string is integer $systolic_bp] } {
    append error_message "<li><font color=red>Systolic blood pressure has to be a number</font>"
}


if { [string equal $error_message ""] } {
    db_dml insert_info "insert into heart
    (heart_id, id, total_cholesterol, hdl_cholesterol, systolic_bp, diabetes, smoker, creation_date, edit_date) 
    values
    (:heart_id, :id, :total_cholesterol, :hdl_cholesterol, :systolic_bp, :diabetes, :smoker, sysdate, sysdate)"

    set page_content "
    [html_page_header 1 "Heart Assessment" "<a href=main.tcl>Main</a> > Heart Assessment" "<a href=gail.tcl?id=$id><b>Gail Assessment<b></a>"]

    <!-- stuff for body of page -->
    <table width=90%>
    <tr><td>

    The following information has been saved for the Heart Assessment for <b>$first_names $last_name</b>.
    <p>
    
    <b>Date of birth:</b> $birth_date &nbsp; &nbsp; <b>Age:</b> $age<br>   
    <b>Total cholesterol:</b> $total_cholesterol<br>
    <b>HDL cholesterol:</b> $hdl_cholesterol<br>
    <b>Systolic blood pressure:</b> $systolic_bp<br>
    <b>Diabetes?</b> $diabetes<br>
    <b>Smoker?</b> $smoker<br>

    </tr></td>
    </table>

    [html_page_footer 1]       
    "
    ns_return 200 text/html $page_content

} else {

    set page_content "
    [html_page_header 1 "Heart Assessment" "<a href=main.tcl>Main</a> > Heart Assessment" "<a href=gail.tcl?id=$id><b>Gail Assessment<b></a>"]
    
    <!-- stuff for body of page -->
    <table width=90%>
    <tr><td>

    <b>Heart Assessment for $first_names $last_name</b>
    <p>

    We had problems with the information that you entered.
    <ul>
    $error_message
    </ul>

    <form method=post action=heart-2.tcl>
    <input type=hidden name=id value=$id>
    <input type=hidden name=heart_id value=$heart_id>
    <input type=hidden name=first_names value=\"$first_names\">
    <input type=hidden name=last_name value=\"$last_name\">
    <input type=hidden name=birth_date value=\"$birth_date\">
    <input type=hidden name=age value=[expr int($age)]>

    <ol>

    <li>What is your total cholesterol level (mg per dL \[mmol per L\])?
    &nbsp; &nbsp;
    <input type=text name=total_cholesterol size=3 value=\"$total_cholesterol\">
    
    <li>What is your HDL cholesterol level (mg per dL \[mmol per L\])?
    &nbsp; &nbsp;
    <input type=text name=hdl_cholesterol size=3 value=\"$hdl_cholesterol\">
 
    <li>What is your systolic blood pressure (mm Hg)?
    &nbsp; &nbsp;
    <input type=text name=systolic_bp size=3 value=\"$systolic_bp\">
    
    
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











