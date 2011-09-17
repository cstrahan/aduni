# Written by Shyam Visweswaran at ADUni, 25 April 2001
# main.tcl - main page for the collection of risk assessment data module
# target for: none; incoming arguments: none

set page_content "
[html_page_header 1 "Risk Assessment" "<a href=\"../admin/admin.tcl\">Administration</a> > Main"]

<!-- stuff for body of page -->
<table width=90%>
<tr><td>
"

set sql_query_person "select count(*) \"count\" from names"
set person_count [db_string get_count $sql_query_person]

set sql_query_gail "select count(*) \"count\" from gail"
set gail_count [db_string get_count $sql_query_gail]

set sql_query_heart "select count(*) \"count\" from heart"
set heart_count [db_string get_count $sql_query_heart]

append page_content "
<ul>
<li>Total number of persons assessed: <b>$person_count</b>
<li>Total Gail Assessments: <b>$gail_count</b>
<li>Total heart Assessments: <b>$heart_count</b>
</ul>

<h3>Follow-up assessments</h3>
<blockquote>

Locate the person you are looking for by doing one of the following searches:
<p>

<form method=post action=search-names.tcl>
Type a first name or last name to locate a person.
<br>
<input type=text name=search_names size=35 maxsize=200> &nbsp;
<input type=submit value=\"Search names\">
</form>

<p>
<form method=post action=search-mgh.tcl>
Type MGH id to locate a person.
<br>
<input type=text name=search_mgh_id size=10 maxsize=10> &nbsp;
<input type=submit value=\"Search MGH id\">

</form>

</blockquote>

<h3>First time assessment</h3>
<blockquote>

If this is a new person, please enter the following information
and then choose the type of assessment you want to do.
<p>

<form method=post action=add-name.tcl>
First name(s)
<br>
<input type=text name=first_names size=40 maxsize=100>

<p>
Last name
<br>
<input type=text name=last_name size=40 maxsize=100>

<p>
Date of birth (use 4-digit year)
<br>
<select name=day>
<option value=\[Day\]>\[Day\]</option>
"

for { set i 1 } { $i <= 31 } { incr i } {
    append page_content "<option value=$i>$i</option>"
}

append page_content "
</select>

<select name=month>
<option value=\[Month\]>\[Month\]</option>
"

set month_names [list "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]

for { set j 0 } { $j <= 11 } { incr j } {
    append page_content "<option value=[lindex $month_names $j]>[lindex $month_names $j]</option>"
}

append page_content "
</select>

<input type=text name=year size=4>


<p>
MGH id (optional)

<br>
<input type=text name=mgh_id size=10 maxsize=10>

<p>
<input type=reset value=Reset> &nbsp; &nbsp;
<input type=submit name=action value=\"Gail Assessment\"> &nbsp; &nbsp;
<input type=submit name=action value=\"Heart Assessment\">

</form>

</blockquote>

</tr></td>
</table>

[html_page_footer 1]
"
    
ns_return 200 text/html $page_content





