# PS-2 Written by Shyam Visweswaran at ADUni, 13 April 2001
# -- edited by todd sjoblom 20 April 2001

# edit_user_info.tcl - target of various .tcl files in various directories

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "login.tcl" }

# test next for a valid user_id
set viewer_id [get_viewer_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "login.tcl" }

# test for admin status
set viewer_is_admin 0 ;# preset
if { [is_user_in_group $viewer_id "admin"] == 1 } {
    set viewer_is_admin 1
}
if { ![ns_queryexists "edit_id"] } {
    ns_return 200 text/plain "There is no user number to edit.  Please hit Back."
    # stop the execution of this script
    return
}
set edit_id [ns_queryget "edit_id"]
if { ![string is integer $edit_id] || $edit_id <= 0 } {
    ns_return 200 text/plain "There is no such user number $edit_id.  Please hit Back."
    # stop the execution of this script
    return
}
if { !($viewer_is_admin == 1 || $viewer_id == $edit_id) } {
    ns_return 200 text/plain "Only the administrator or the user can perform an edit!  Please hit Back."
    # stop the execution of this script
    return
}

set s_member_can_see "" ;# preset that edit_id is not a leader
set sql_query "select count(*) from user_group_map where
 user_id = :edit_id and role = 'leader'"
if { 0 < [ db_string my_count $sql_query ] } { 
    set s_member_can_see " members and"
}

set sql_query "select user_id,
 name_prefix, first_names, last_name, name_suffix,
 email, email_bouncing_p,
 phone_home, phone_work,
 to_char(registration_date, 'Month dd, yyyy') \"s_registration_date\",  
 to_char(last_visit, 'Month dd, yyyy') \"s_last_visit\",  
 status, creation_user,
 to_char(banning_date, 'Month dd, yyyy') \"s_banning_date\",
 banning_user, banning_note
 from users where user_id=:edit_id"
if { [db_0or1row get_user_info $sql_query] == 0 } {
    ns_return 200 text/plain "There is no such user number $edit_id.  Please hit Back."
    # stop the execution of this script
    return
}

set page_actions "<span class=action><a href=\"password.tcl?pwd_id=$edit_id\">Change password</a></span>"

set page_content "
[html_head "Edit personal info"]
[html_body_start]
[html_body_top "Edit personal information" "<a href=\"home.tcl\">Home</a> > Edit personal info" "#bbbbbb" $page_actions]

<!-- stuff for body of page -->

<table width=90%>

<tr><td colspan=2>
<font size=+2>Items marked with a <font color=red>*</font> have to be completed.</font>
<p>
<b>Note: This information will be displayed to any member of this site.</b>
<form method=post action=edit_user_info-2.tcl>
<input type=hidden name=edit_id value=$edit_id>


</td></tr>
<tr><td>Title (e.g. Dr.)</td>
<td> <input type=text name=name_prefix size=20 maxlength=20 value=\"$name_prefix\">  </td></tr>

<tr><td><font color=red>*</font>First names</td>
<td> <input type=text name=first_names size=50 maxlength=100 value=\"$first_names\">  </td></tr>

<tr><td><font color=red>*</font>Last name</td>
<td> <input type=text name=last_name size=50 maxlength=100 value=\"$last_name\"> </td></tr>

<tr><td>Suffix (e.g. M.D.)</td>
<td> <input type=text name=name_suffix size=20 maxlength=20 value=\"$name_suffix\"> </td></tr>

<tr><td>Home phone</td>
<td> <input type=text name=phone_home size=30 maxlength=30 value=\"$phone_home\">  </td></tr>

<tr><td>Work phone</td>
<td> <input type=text name=phone_work size=30 maxlength=30 value=\"$phone_work\">  </td></tr>
"
append page_content "
<tr><td colspan=2> &nbsp; </td></tr>
<tr><td colspan=2><b>Note: The following information is available only to Administrators and your group$s_member_can_see leaders.</b></td></tr>
<tr><td><font color=red>*</font>Email</td>
<td> <input type=text name=email size=50 maxlength=100 value=\"$email\">"
append page_content "<input type=hidden name=email_bouncing_p value=\"$email_bouncing_p\">
  </td><td>&nbsp;</td>
  </tr>"


if { $viewer_is_admin == 1 } { ;# here's information to edit or at least to examine before banning users
    append page_content "
    <tr><td>Member since</td>
    <td> $s_registration_date  </td></tr>

    <tr><td>Last visit</td>
    <td> $s_last_visit  </td></tr>
    
    <tr><td>Creator</td>
    <td> [get_user_name $creation_user ]</td></tr>

    <tr><td>Status</td>"
    set s_args {"active" "inactive" "banned" }
    set s_names {"active" "inactive" "banned" }
    set s_actions "[html_action $s_args $s_names "admin/do_action.tcl?user_id=$edit_id&action=" $status ]"
    append page_content "<td><div class=action>$s_actions</div></td></tr>"

    if { $banning_user > 0 } {
	append page_content "<tr><td>Banning person</td><td>  [get_user_name $banning_user]  </td></tr>
	                     <tr><td>Banning date</td><td> $s_banning_date  </td></tr>
	                     <tr><td>Banning note</td> <td> $banning_note  </td></tr>"
    }
}
 
append page_content "<tr><td>&nbsp</td></tr>
                     <tr>&nbsp;<td> </td><td><input type=reset value=\"Reset\">
                      &nbsp; &nbsp; <input type=submit value=\"Submit\"> </td></tr>
                     <tr><td colspan=2</form></td></tr>
                     </table>

[html_body_bottom]
[html_body_end]
[html_foot]
"

db_release_unused_handles

ns_return 200 text/html $page_content











