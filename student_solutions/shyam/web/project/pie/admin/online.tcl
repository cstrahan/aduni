# PS-5 Written by Todd Sjoblomat ADUni, 28 April 2001
# -- edited from admin.tcl

# admin.tcl - target of ../home.tcl

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "../login.tcl" }

# test next for a valid user_id
set viewer_id [get_viewer_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "../login.tcl" }

# test for admin status
if { [is_user_in_group $viewer_id admin] == 0 } { ns_returnredirect "../login.tcl" }

set time_actions "" ;# we don't need to reset this, it's from admin.tcl

set page_content "
[html_head "Administration"]
[html_body_start]

<!-- Stuff for top of page -->
<table width=90%>

<tr><td colspan=2>
<span class=logo>Partners in Education</span>
</td><td align=right valign=center>

<!--    <form method=post action=\"../search.tcl\"> -->
<!--   <input type=text size=20 name=search>        -->
<!--   <input type=submit value=\"Search\"></form>  -->

</td></tr>


<tr><td>
   <span class=title>Administration</span>
</td><td align=center>
   <span class=action>$time_actions</span>
</td><td align=right>
</td></tr>

<tr bgcolor=#bbbbbb><td colspan=3>
<span class=navigation><a href=\"../home.tcl\">Home</a> > Administration </span>
</td></tr></table>
"

append page_content "
 <table width=90%>
  <tr>
   <td colspan=3><font size=+1><b>Users who are online now</b></font>
   </td>
   <td align=right valign=center>
       <form method=post action=\"../search.tcl\">
       &nbsp; <input type=text size=10 name=users><input type=submit value=\"Search All Users\"></form>
   </td>
  </tr>
 </table>
 <table width=90%>
"
set sql_query "select user_id, name_prefix, first_names, last_name, name_suffix, email,
      trunc((sysdate - last_visit)*24, 1) as s_hours from users" ;# , 'dd Month YYYY h24:mi'
append sql_query " where (sysdate - last_visit < 2/24)"
append sql_query " order by last_visit desc"
db_foreach get_users $sql_query {
    append page_content "
     <tr>
      <td> $s_hours hr</td>
      <td> <b>$name_prefix $first_names $last_name $name_suffix</b> </td>
      <td> <a href=\"mailto:$email\"><i>$email</i> </td>
      <td> <a href=\"../display_user_info.tcl?display_id=$user_id\">Details</a>
       | <a href=\"../edit_user_info.tcl?edit_id=$user_id\">Edit</a>"
#       | [html_action $s_args $s_names "do_action.tcl?user_id=$user_id&action=" $status ]
append page_content "</td>
     </tr>"
} if_no_rows {
    append page_content "No online users found -- is there an error in your own login timestamps?"
}
append page_content "</table>"

db_release_unused_handles

append page_content "

[html_body_bottom]
[html_body_end]
[html_foot]
"

ns_return 200 text/html $page_content











