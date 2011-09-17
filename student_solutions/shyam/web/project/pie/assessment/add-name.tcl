# Written by Shyam Visweswaran at ADUni, 25 April 2001
# add-name.tcl - process the names form; do some error checking and insert
# into name db if all is good
# target for: main.tcl; incoming arguments: first_names, last_name, day, month, year, mgh_id

set_the_usual_form_variables

set error_message ""
if { [string equal $first_names ""] } {
    append error_message "<li><font color=red>First name(s) missing</font>"
}

if { [string equal $last_name ""] } {
    append error_message "<li><font color=red>Last name is missing</font>"
}

if { [string equal $day "\[Day\]"] } {
    append error_message "<li><font color=red>You forgot to choose the day in Date of birth</font>"
}

if { [string equal $month "\[Month\]"] } {
    append error_message "<li><font color=red>You forgot to choose the month in Date of birth</font>"
}

if { [string equal $year ""] } {
    append error_message "<li><font color=red>Year in Date of birth is missing</font>"
} elseif { [string length $year] < 4 } {
    append error_message "<li><font color=red>Year in Date of birth is invalid</font>"
} elseif { ![string is integer $year]} {
    append error_message "<li><font color=red>Year in Date of birth is invalid</font>"
}

if { [string equal $error_message ""] } {

    set birth_date [db_string get_birth_date "select to_date('$day $month $year', 'dd Month YYYY') from dual"]
    set id [db_string get_id "select names_seq.nextval from dual"]
    db_dml insert_info "insert into names
    (id, first_names, last_name, birth_date, mgh_id, creation_date)
    values
    (:id, :first_names, :last_name, :birth_date, :mgh_id, sysdate)"

    if { [string equal $action "Gail Assessment"] } {
	ns_returnredirect "gail.tcl?id=$id"
    } elseif { [string equal $action "Heart Assessment"] } {
	ns_returnredirect "heart.tcl?id=$id"
    }

} else {

    set page_content "
    [html_page_header 1 "New Assessment" "<a href=main.tcl>Main</a> > New Assessment"]
    
    <!-- stuff for body of page -->
    <table width=90%>
    <tr><td>

    <blockquote>

    There were problems processing your information.
    <ul>
    $error_message
    </ul>

    <form method=post action=add-name.tcl>
    First name(s)
    <br>
    <input type=text name=first_names size=40 maxsize=100 value=\"$first_names\">
    
    <p>
    Last name
    <br>
    <input type=text name=last_name size=40 maxsize=100 value=\"$last_name\">
    
    <p>
    Date of birth (use 4-digit year)
    <br>
    <select name=day>
    <option value=\[Day\]>\[Day\]</option>
    "

    for { set i 1 } { $i <= 31 } { incr i } {
	if { $i == $day } {
	    append page_content "<option value=$i selected>$i</option>"
	} else {
	    append page_content "<option value=$i>$i</option>"
	}
    }

    append page_content "
    </select>
    
    <select name=month>
    <option value=\[Month\]>\[Month\]</option>
    "

    set month_names [list "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]

    for { set j 0 } { $j <= 11 } { incr j } {
	if { $month == [lindex $month_names $j] } {
	    append page_content "<option value=[lindex $month_names $j] selected>[lindex $month_names $j]</option>"
	} else {
	    append page_content "<option value=[lindex $month_names $j]>[lindex $month_names $j]</option>"	    
	}
    }

    append page_content "
    </select>

    <input type=text name=year value=\"$year\" size=4>

    <p>
    MGH id (optional)
    <br>
    <input type=text name=mgh_id size=10 maxsize=10 value=\"$mgh_id\">
    
    <p>
    <input type=submit value=Submit> &nbsp; &nbsp;
    <input type=reset value=Reset>
    
    </form>

    </blockquote

    </tr></td>
    </table>
    
    [html_page_footer 1]
    "
    ns_return 200 text/html $page_content
}





