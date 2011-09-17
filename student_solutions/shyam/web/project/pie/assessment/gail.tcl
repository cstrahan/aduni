# Written by Shyam Visweswaran at ADUni, 25 April 2001
# gail.tcl - the Gail Risk Assessment form
# into name db if all is good
# target for: add-name.tcl; incoming arguments: id

set id [ns_queryget "id"]
set sql_query "select months_between(sysdate, birth_date)/12 \"age\", to_char(birth_date, 'dd Month, yyyy') \"birth_date\",
    first_names, last_name from names where id = :id"

if { [db_0or1row get_name_info $sql_query] == 0} {
    ns_return 200 text/plain "Sorry! Unable to get information for this person." 
    return

} else {
    
    set gail_id [db_string get_gail_id "select gail_seq.nextval from dual"]

    append page_content "
    [html_page_header 1 "Gail Assessment" "<a href=main.tcl>Main</a> > Gail Assessment" "<a href=heart.tcl?id=$id><b>Heart Assessment<b></a>"]
    
    <!-- stuff for body of page -->
    <table width=90%>
    <tr><td>
    <b>Gail Assessment for $first_names $last_name</b>

    <form method=post action=gail-2.tcl>
    <input type=hidden name=gail_id value=$gail_id>
    <input type=hidden name=id value=$id>
    <input type=hidden name=first_names value=\"$first_names\">
    <input type=hidden name=last_name value=\"$last_name\">
    <input type=hidden name=birth_date value=\"$birth_date\">
    <input type=hidden name=age value=[expr int($age)]>

    <ol>
    <li>
    Date of birth: $birth_date   &nbsp; &nbsp; &nbsp; &nbsp;   Age: [expr int($age)]

    <li>
    What is your race?
    &nbsp; &nbsp; 
    <input type=radio name=race value=white> White
    &nbsp; &nbsp;    
    <input type=radio name=race value=black> Black
    &nbsp; &nbsp;
    <input type=radio name=race value=asian> Asian

    <li>
    How old were you when you had your first menstrual period?    
    &nbsp; &nbsp;  
    <input type=text name=age_at_menarche size=2>

    <li>
    How old were you when your first child was born? (If you never had a child, enter 0)
    &nbsp; &nbsp;  
    <input type=text name=age_at_first_live_birth size=2>

    <li>
    How many of your sisters, daughters or mother have had breast cancer?
    &nbsp; &nbsp;
    <input type=text name=relatives_with_cancer size=2>

    <li>
    How many breast biopsies have you had? (breast tissue removed to test for cancer)
    &nbsp; &nbsp;
    <input type=text name=biopsy_count size=2>
    
    <li>
    Did you have a biopsy with atypical hyperplasia?
    &nbsp; &nbsp;
    <input type=radio name=biopsy_hyperplasia value=yes> Yes
    &nbsp; &nbsp;
    <input type=radio name=biopsy_hyperplasia value=no> No
    &nbsp; &nbsp;
    <input type=radio name=biopsy_hyperplasia value=unknown> Unknown

    </ol>
    &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
    <input type=submit value=Submit> &nbsp; &nbsp;
    <input type=reset>


    </form>
    </tr></td>
    </table>
    
    [html_page_footer 1]   
    "
    ns_return 200 text/html $page_content
}







