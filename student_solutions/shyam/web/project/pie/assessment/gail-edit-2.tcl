# Written by Shyam Visweswaran at ADUni, 25 April 2001
# gail-edit-2.tcl - processes the Gail Risk Assessment edit form
# and update db if all is good
# target for: gail-edit.tcl, this script; incoming arguments: ....

set race ""
set biopsy_hyperplasia ""
set_the_usual_form_variables

set error_message ""
if { ![string is integer $age_at_menarche] } {
    append error_message "<li><font color=red>Age at menarche has to be a number</font>"
}
if { ![string is integer $age_at_first_live_birth] } {
    append error_message "<li><font color=red>Age when first child born has to be a number</font>"
}
if { ![string is integer $relatives_with_cancer] } {
    append error_message "<li><font color=red>Sisters/daughters/mother with cancer has to be a number</font>"
}
if { ![string is integer $biopsy_count] } {
    append error_message "<li><font color=red>Number of biopsies has to be number</font>"
}

if { [string equal $error_message ""] } {
    db_dml insert_info "update gail
    set race = :race, age_at_menarche = :age_at_menarche, age_at_first_live_birth = :age_at_first_live_birth,
    relatives_with_cancer = :relatives_with_cancer, biopsy_count = :biopsy_count, biopsy_hyperplasia = :biopsy_hyperplasia,
    edit_date = sysdate
    where gail_id = :gail_id"

    set page_content "
    [html_page_header 1 "Gail Assessment Update" "<a href=main.tcl>Main</a> > Gail Assessment Update"]
    
    <!-- stuff for body of page -->
    <table width=90%>
    <tr><td>

    The following information has been updated for the Gail Assessment for <b>$first_names $last_name</b>.
    <p>
    Date of birth: <b>$birth_date</b> &nbsp; &nbsp; Age: <b>$age</b><br>
    
    Race: <b>$race</b><br>
    Age of first menstrual period: <b>$age_at_menarche</b><br>
    Age at birth of first child: <b>$age_at_first_live_birth</b><br>
    Number of relatives with breast cancer: <b>$relatives_with_cancer</b><br>
    Number of breast biopsies: <b>$biopsy_count</b><br>
    Biopsy with atypical hyperplasia? <b>$biopsy_hyperplasia</b><br>
    
    </tr></td>
    </table>

    [html_page_footer 1]           
    "
    ns_return 200 text/html $page_content

} else {

    set page_content "
    [html_page_header 1 "Gail Assessment Update" "<a href=main.tcl>Main</a> > Gail Assessment Update"]
    
    <!-- stuff for body of page -->
    <table width=90%>
    <tr><td>

    Gail Assessment Update for <b>$first_names $last_name</b>
    <p>

    We had problems with the information that you entered.
    <ul>
    $error_message
    </ul>

    <form method=post action=gail-edit-2.tcl>
    <input type=hidden name=name_id value=$name_id>
    <input type=hidden name=first_names value=\"$first_names\">
    <input type=hidden name=last_name value=\"$last_name\">
    <input type=hidden name=birth_date value=\"$birth_date\">
    <input type=hidden name=age value=[expr int($age)]>

    <ol>
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
    <input type=text name=age_at_menarche size=3 value=\"$age_at_menarche\">
    
    <li> How old were you when your first child was born? (If you never had a child, enter 0)
    &nbsp; &nbsp;
    <input type=text name=age_at_first_live_birth size=3 value=\"$age_at_first_live_birth\">
 
    <li> How many of your sisters, daughters or mother have had breast cancer?
    &nbsp; &nbsp;
    <input type=text name=relatives_with_cancer size=3 value=\"$relatives_with_cancer\">
    
    <li>How many breast biopsies have you had? (breast tissue removed to test for cancer)
    &nbsp; &nbsp;
    <input type=text name=biopsy_count size=3 value=\"$biopsy_count\">
    
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
    <input type=submit> &nbsp; &nbsp;
    <input type=reset>
    
    </form>
    
    </tr></td>
    </table>

    [html_page_footer 1]        
    "
    ns_return 200 text/html $page_content
}







