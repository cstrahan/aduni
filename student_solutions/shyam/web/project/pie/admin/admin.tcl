# PS-2 Written by Shyam Visweswaran at ADUni, 12 April 2001
# -- edited by todd sjoblom 18 April 2001

# admin.tcl - target of ../home.tcl

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "../login.tcl" }

# test next for a valid user_id
set viewer_id [get_viewer_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "../login.tcl" }

# test for admin status
if { [is_user_in_group $viewer_id admin] == 0 } { ns_returnredirect "../login.tcl" }

# We are going to filter the page to display recent information first.
set days_ago 7 ;# and we allow 2nd to last login at "new"
if { [ns_queryexists days_ago] } { set days_ago [ns_queryget days_ago] }
set i_time_args [list 1 7 30 180 -1 ] ;# this may cause order "8 7 30 180 all"
set s_time_names {"1 day" "1 week" "1 month" "6 months" "beginning"}
set i_temp [lsearch $i_time_args $days_ago] ;# reset index into list
if { $i_temp == -1 } { ;# since 2nd-to-last login
    set s_time_name "last login"
} else {
    set s_time_name [lindex $s_time_names $i_temp] ;# reset
}
# build the links to redisplay the time choices
set time_actions "[html_action $i_time_args $s_time_names "admin.tcl?days_ago=" $days_ago ]"


set page_content "
[html_page_header 2 "Administration" "<a href=\"../home.tcl\">Home</a> > Administration" $time_actions]

<table width=90%>
<tr><td colspan=2>
\[ <b><a href=\"../content/admin/admin-content.tcl?forum=Menopause\">Menopause Q and A</a>
 | <a href=\"../content/admin/admin-content.tcl?forum=Physician\">Physician Q and A</a>
 | <a href=\"../content/admin/a-content.tcl\"> Articles</a>
 | <a href=\"../assessment/main.tcl\">Assessments</a>
 | <a href=\"registration.tcl\">Register new user</a>
 | <a href=\"../edit_group_info.tcl?edit_id=0\">Create new group</a> </b> \]
<hr size=1>
</td></tr>

<td><font size=+1><b>Users who registered in past $s_time_name</b></font></td>
<td align=right><form method=post action=\"../search.tcl\">
&nbsp; <input type=text size=10 name=users><input type=submit value=\"Search All Users\"></form></span>
   </td></tr>
     <tr><td colspan=2>
<ol>
"

#set s_args {"active" "inactive" "banned" }
#set s_names {"active" "inactive" "banned" }

set sql_query "select user_id, name_prefix, first_names, last_name, name_suffix, email,
     status, registration_date from users"
if { $days_ago >= 0 } { append sql_query " where (registration_date >= sysdate - :days_ago)" }
append sql_query " order by registration_date desc"
db_foreach get_users $sql_query {
    append page_content "
      <li>$name_prefix $first_names $last_name $name_suffix
      (<a href=\"mailto:$email\"><i>$email</i></a>) &nbsp; &nbsp; &nbsp;
      \[ <b><a href=\"../display_user_info.tcl?display_id=$user_id\">Details</a>
       | <a href=\"../edit_user_info.tcl?edit_id=$user_id\">Edit</a></b> \]"
#       | [html_action $s_args $s_names "do_action.tcl?user_id=$user_id&action=" $status ]
} if_no_rows {
    append page_content "No new users found."
}

#Groups
append page_content "
</ol>
</td></tr>
  <tr> <td><font size=+1><b>Groups created since $s_time_name</b></font>
   </td> <td align=right>
    <form method=post action=\"../search.tcl\">
    <input type=text size=10 name=groups><input type=submit value=\"Search All Groups\"></form>
   </td></tr>
<tr><td colspan=2>
<ol>
"
set s_args {"active" "inactive" }
set s_names {"active" "inactive" }
set sql_query "select g.creation_date, g.pretty_name, g.short_name, g.status, 
      u.name_prefix, u.first_names, u.last_name, u.name_suffix,
      g.group_id from groups g, users u where u.user_id = g.creation_user"
if { $days_ago >= 0 } { append sql_query " and (g.creation_date >= sysdate - :days_ago)" }
append sql_query " order by g.creation_date desc"
db_foreach get_groups $sql_query {
    append page_content "
      <li><b>$pretty_name</b> &nbsp; &nbsp
       (by $name_prefix $first_names $last_name $name_suffix) &nbsp; &nbsp; &nbsp;
       \[ <b><a href=\"../display_group_info.tcl?display_id=$group_id\">Details</a>
       | <a href=\"../edit_group_info.tcl?edit_id=$group_id\">Edit</a></b> \]"
#       | [html_action $s_args $s_names "do_action.tcl?table=group_id=$group_id&action="  $status ]
} if_no_rows {
    append page_content "No new groups found."
}

db_release_unused_handles

append page_content "
</ol>
</td></tr></table>
[html_page_footer 2]  
"

ns_return 200 text/html $page_content










