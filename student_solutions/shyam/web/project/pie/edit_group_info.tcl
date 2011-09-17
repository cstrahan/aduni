# PS-2 Written by Therese Hendricks at ADUNI, 21 April 2001
# added to by todd sjoblom 28 april 2001

# edit_group_info.tcl - target of links from admin/admin.tcl (edit_id > 0 for update or = 0 for insert)
#                     - target of display_group_info.tcl     (edit_id > 0 for update)

# We will post the form variables to edit_group_info-2.tcl, with  edit_id > 0 and insert_id = 0 for update
#                      -----------                           or   edit_id = 0 and insert_id > 0 for insert


# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "login.tcl" }

#test next for a valid viewer_id
set viewer_id [get_viewer_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "login.tcl" }

set edit_id [ns_queryget "edit_id"] ;# incoming is edit_id; outgoing will be both edit_id and insert_id, with one != 0

# test for admin status and then leader status
set viewer_is_admin 0 ;# preset
if { [is_user_in_group $viewer_id admin] == 1 } {
    set viewer_is_admin 1
} elseif { $edit_id == 0 }  {
    ns_return 200 text/plain "Only an administrator can create a group."
    # stop the execution of this script
    return
} elseif  { $edit_id == 1 }  {
    ns_return 200 text/plain "Only an administrator can edit the admin group."
    # stop the execution of this script
    return
} else {
    ;# is viewer allowed to edit this already existing group?
    set sql_query "select count(*) from user_group_map where
     user_id = :viewer_id and group_id = :edit_id and role = 'leader'"
    if { 0 == [ db_string my_count $sql_query ] } {
	ns_return 200 text/plain "Only an administrator or the leader of this group can edit it."
	# stop the execution of this script
	return
    }
}

if { $edit_id > 0 } {
    set insert_id 0 ;# we want to update an existing group, not insert one
    set sql_query "select * from groups where group_id = :edit_id"
    # get ALL the fields from a single row of groups table
    if { [db_0or1row get_group_info $sql_query ] == 0 } {
	ns_return 200 text/plain "There is no group number $edit_id to edit."
	# stop the execution of this script
	return
    }
} else {
    # preset values
    set insert_id [db_string nextval "select group_seq.nextval from dual"] ;# for insert
    ;# insert_id > 0 so we will insert a new group into groups table
    ;# edit_id == 0 so edit_group_info-2.tcl won't update
    set short_name "meeting"
    set pretty_name ""
    set description ""
    set meeting_time "" ;# this is not a date though if it were we could sort nicely on it in What's new
    set meeting_place ""
    set meeting_note ""
    set creation_user $viewer_id  ;# this is who is responsible for the creation
    set status "active"           ;# default
    set inactivation_user ""
    set inactivation_date ""
}

set page_content "
[html_head "Edit group info"]
[html_body_start]
[html_body_top "Edit group information" "<a href=\"home.tcl\">Home</a> > Edit group info"]

<!-- stuff for body of page -->

<table width=90%>
<tr><td colspan=2><font size=+2>Items marked with a <font color=red>*</font> have to be completed.</font>

<form method=post action=edit_group_info-2.tcl>
<input type=hidden name=edit_id value=$edit_id>
<input type=hidden name=insert_id value=$insert_id>

</td></tr>
<tr><td><font color=red>*</font>Type</td><td>   
"

if { $edit_id == 1 } {
    # admin
    append page_content "$short_name<input type=hidden name=short_name value=\"$short_name\">"
} else {
    if { $short_name == "meeting"  } { set check_meeting  " checked" } else { set check_meeting  "" }
    if { $short_name == "practice" } { set check_practice " checked" } else { set check_practice "" }
    append page_content "<input type=radio name=short_name value=\"meeting\" $check_meeting> Meeting
     &nbsp; &nbsp;<input type=radio name=short_name value=\"practice\" $check_practice> Medical Practice"
}

append page_content "
</td></tr>
<tr><td><font color=red>*</font>Name</td>
<td> <input type=text name=pretty_name size=50 maxlength=100 value=\"$pretty_name\">  </td></tr>

<tr><td>&nbsp;Description</td>
<td> <input type=text name=description size=50 maxlength=100 value=\"$description\"> </td></tr>

<tr><td>&nbsp;Meeting Time (e.g. Mon 7pm)</td>
<td> <input type=text name=meeting_time size=20 maxlength=20 value=\"$meeting_time\">  </td></tr>

<tr><td>&nbsp;Meeting Place</td>
<td> <input type=text name=meeting_place size=30 maxlength=30 value=\"$meeting_place\"> </td></tr>

<tr><td>&nbsp;Meeting Note (e.g. bring questionnaire) </td>
<td> <input type=text name=meeting_note size=30 maxlength=30 value=\"$meeting_note\">  </td></tr>
"

if { $viewer_is_admin == 1 } {
    append page_content "<tr><td>Creator</td><td> [get_user_name $creation_user]</td></tr>"

    append page_content "<tr><td>Status</td>"
    if { $edit_id <= 1 } {
	;# 'admin' group or a being-created group
	append page_content "<td>$status</td></tr>"
    } else {
	set s_args {"active" "inactive" }
	set s_names {"active" "inactive" }
	set s_actions "[html_action $s_args $s_names "admin/do_action.tcl?group_id=$edit_id&action=" $status ]"
	append page_content "<td><div class=action>$s_actions</div></td></tr>"
    }

    if { $inactivation_user > 0 } {
	append page_content "<tr><td>Inactivating person</td><td> [get_user_name $inactivation_user] </td></tr>
                             <tr><td>Inactivation date</td><td> $inactivation_date </td></tr>"
    }
}
 
append page_content "<tr><td colspan=2>&nbsp;</tr>
                    <tr><td>&nbsp;</td>
                    <td><input type=reset value=\"Reset\"> &nbsp &nbsp <input type=submit value=\"Submit\"> </td></tr>

<tr><td colspan=2> </form> </td></tr>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"

db_release_unused_handles

ns_return 200 text/html $page_content





