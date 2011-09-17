# PS-2 Written by Shyam Visweswaran at ADUni, 15 April 2001
# -- edited by todd sjoblom 20 April 2001

# display_group_info.tcl - target of various .tcl files in various directories

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "login.tcl" }

# test next for a valid group_id
set viewer_id [get_viewer_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "login.tcl" }

set display_id [ns_queryget "display_id"]

# test for admin status and then leader status
set viewer_is_admin 0 ;# preset
set viewer_is_leader 0 ;# preset
if { [is_user_in_group $viewer_id admin] == 1 } {
    set viewer_is_admin 1
} else { ;# is viewer able to edit this group?
   set sql_query "select count(*) from user_group_map where
       user_id = :viewer_id and group_id = :display_id and role = 'leader'"
    if { 0 < [ db_string my_count $sql_query ] } { set viewer_is_leader 1 }
}

set sql_query "select group_id,
 short_name, pretty_name, description,
 meeting_time, meeting_place, meeting_note,
 creation_user, status, inactivation_user,
 to_char(inactivation_date, 'Month dd, yyyy') \"s_inactivation_date\"
 from groups where group_id = :display_id"

if { [ db_0or1row is_group $sql_query ] == 0 } {
    ns_return 200 text/plain "There is no group number $display_id to display.
    # stop the execution of this script
    return
}
  
set page_content "
[html_head "Display group information"]
[html_body_start]
"

set page_actions ""
if { $viewer_is_admin == 1 || $viewer_is_leader == 1 } {
    append page_actions " <div class=action><a href=\"edit_group_info.tcl?edit_id=$display_id\">Edit</a></div>"
} ;# if 

append page_content "
[html_body_top "$pretty_name $short_name" "<a href=home.tcl>Home</a> > Group info" "#bbbbbb" $page_actions]

<!-- stuff for body of page -->

<table width=90%>
"

set i_row 0; set s_color [row_color $i_row]
append page_content " <tr><td $s_color>Description</td><td $s_color> $description &nbsp;</td></tr>"
incr i_row; set s_color [row_color $i_row]
append page_content "<tr><td $s_color>Meeting time</td><td $s_color> $meeting_time &nbsp;</td></tr>"
incr i_row; set s_color [row_color $i_row]
append page_content "<tr><td $s_color>Meeting place</td><td $s_color> $meeting_place &nbsp;</td></tr>"
incr i_row; set s_color [row_color $i_row]
append page_content "<tr><td $s_color>Meeting note</td><td $s_color> $meeting_note &nbsp;</td></tr>"


if { $viewer_is_admin == 1 } {
    incr i_row; set s_color [row_color $i_row]
    append page_content "<tr><td $s_color>Creator</td>
     <td $s_color> [get_user_name $creation_user] </td></tr>"

    incr i_row; set s_color [row_color $i_row]
    append page_content "<tr><td $s_color>Status</td> <td $s_color> $status </td></tr>"

    if { $inactivation_user > 0 } {
	incr i_row; set s_color [row_color $i_row]
	append page_content "<tr><td $s_color>Inactivating person</td>
	                     <td $s_color> [get_user_name $inactivation_user] </td></tr>"

	incr i_row; set s_color [row_color $i_row]
	append page_content "<tr><td $s_color>Inactivation date</td>
	<td $s_color> $s_inactivation_date</td></tr>"
    }
}

# we want to display the user_group_map of the display_id here


append page_content "<tr><td colspan=2>&nbsp;</td></tr>
                     <tr><td colspan=2><h3>Leaders and Members</h3></td></tr>"
set sql_group_query "select
 u.user_id, u.name_prefix, u.first_names, u.last_name, u.name_suffix,
 m.role  
 from users u, user_group_map m
 where m.group_id = :display_id
 and m.user_id = u.user_id
 order by role" ;# accidentally alphabetical role
set i_row 0; # use parity for color of rows
db_foreach all_users $sql_group_query {
    set s_color [ row_color $i_row ]
    append page_content "<td $s_color><div class=action>
     <a href=\"display_user_info.tcl?display_id=$user_id\">
     $name_prefix $first_names $last_name $name_suffix </a></div></td>"
    append page_content "<td $s_color>"
    if { ($viewer_is_admin == 0 && $viewer_is_leader == 0) ||
     $viewer_id == $user_id } {
	 ;# don't have permission to remove this user
	 if { $group_id == 1 } {
	     append page_content "administrator " ;# built-in role
	 } else {
	     append page_content "$role "
	 }
     } elseif { $group_id == 1 } {
	append page_content "<div class=action>
	<a href=\"admin/do_action.tcl?user_id=$user_id&group_id=$display_id&action=inactive\">
	Remove as administrator</a></div>"
    } else {
	append page_content "<div class=action>
	<a href=\"admin/do_action.tcl?user_id=$user_id&group_id=$display_id&role=$role&action=inactive\">
	Remove as $role</a></div>"
    }
    append page_content "</td></tr>"
    incr i_row
} if_no_rows {
    append page_content "<tr><td [row_color 0 ] colspan=2>No members at this time.</td></tr>"
}


db_release_unused_handles

append page_content "
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"

ns_return 200 text/html $page_content












