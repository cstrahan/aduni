# Written by todd sjoblom 18-20 April 2001 from admin.tcl as a basis
# home.tcl - target of login.tcl, relogin.tcl

set i_groups {} ;# the distinct groups viewer is mapped to

set viewer_id 1

# We don't ask for distinct goup_id, since a doctor can be leader & member of a practice!
set sql_query "select m.group_id, m.role, g.short_name
  from user_group_map m, groups g
  where m.user_id = :viewer_id and m.group_id = g.group_id"
db_foreach get_users $sql_query {
    if { [lsearch $i_groups $group_id] == -1 } { ;# distinct groups
	lappend i_groups $group_id
	if { $short_name == "admin" } { set viewer_is_admin 1 }
    }
    if { $role == "leader" } { set viewer_is_leader 1 }
}


set page_content "
<table>
 <tr><td>&nbsp;</td></tr>
 <tr><td colspan=3><font size=+1><b>My groups</b></font></td></tr>
 <tr><td colspan=3>&nbsp;</td></tr>"

#ns_log Warning "about to do group"
set n_active 0 ;# preset number of active groups
set i_row 0; # use parity for color of rows
foreach i_group $i_groups {
    #ns_log Warning "about to do group 2"
    set sql_query "select g.group_id, g.status, 
     g.short_name, g.pretty_name, g.meeting_time, g.meeting_note  
     from groups g, user_group_map m where
     g.group_id = :i_group and g.group_id = m.group_id
     and m.user_id = :viewer_id 
     and (g.status = 'active' or :viewer_is_admin = 1)"
    #ns_log Warning "about to do group 3"
    if { [db_0or1row get_groups $sql_query] == 1 } { ;# <= 1
	set s_color [ row_color $i_row ]
	append page_content "<tr $s_color><td $s_color>
	 <a href=\"display_group_info.tcl?display_id=$group_id\">
	 $pretty_name $short_name </a> </td>"
	if { $status == "active" } {
	    append page_content "<td $s_color>$meeting_time &nbsp;</td>"
	} else { ;# viewer_is_admin
	    append page_content "
	     <td $s_color><a href=\"edit_group_info.tcl?edit_id=$group_id\">
	     Activate</a> </td>"
	}
	append page_content "
	 <td $s_color>$meeting_note &nbsp;</td>
	 </tr>"
	incr n_active
    }
    incr i_row
} ;# foreach
if { $n_active == 0 } {
    append page_content "<tr><td [row_color 0 ] colspan=3>You don't seem to be in
      any active groups.</td></tr>"
}
# end of Groups

append page_content "
</table>
"

db_release_unused_handles

ns_return 200 text/html $page_content




