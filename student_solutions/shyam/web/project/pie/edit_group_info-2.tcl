# PS-2 Written by Therese Hendricks at ADUNI, 18 April 2001
# added to by todd sjoblom 21 april 2001 to create a group if incoming insert > 0
# edit_group_info-2.tcl 
# target for edit_group_info.tcl with form variable edit_id > 0 to update groups table
#                                 ---               insert_id > 0 to insert into groups table


# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "login.tcl" }

# test next for a valid viewer_id
set viewer_id [get_user_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "login.tcl" }

# test next for admin status
set viewer_is_admin 0 ;# preset
if { [is_user_in_group $viewer_id "admin"] == 1 } {
    set viewer_is_admin 1
}

# grab the values from the form
set_the_usual_form_variables
# edit_id and insert_id, exactly one of which must be non-zero
# also: short_name pretty_name description meeting_time meeting_place meeting_note

# add code here for error checking on form values
if { ($insert_id > 0 && $edit_id > 0) || ($insert_id == 0 && $edit_id == 0) } {
    ns_return 200 text/plain "Bad group id's.  Please go Back."
    # stop the execution of this script
    return
}

set pretty_name [string trim $pretty_name]
if { [string equal $pretty_name ""] } {
    ns_return 200 text/plain "We need a name for the group.  Please hit Back."
    # stop the execution of this script
    return
}

if { $insert_id > 0 } {
    ;# create a group number $insert_id
    if { $viewer_is_admin == 0 } {
	ns_return 200 text/plain "Only an administrator can create a group.  Please hit Back"
	# stop the execution of this script
	return
	# If, for this or any other error where insert_id > 0, the admin does hit Back and then
	# re-Submit, we will re-see the same old seq number.
	# If the admin hits Back and then ReLoad and SUbmit, then they
        # don't see their old form variables in edit_group_info.tcl, and we've wasted a seq number, that's all.
    }
    # if no errors, insert into groups table and go to display group info page
    # We have prevented n*Submit from repeatedly adding dupes to sequentially greater group_id's.
    ns_log Warning "group_id =$insert_id; short_name =$short_name; pretty_name =$pretty_name."
    db_dml insert_group "insert into groups
      (group_id, short_name, pretty_name, description, meeting_time, meeting_place, meeting_note, 
      status, creation_user, creation_date)
     values
      (:insert_id, :short_name, :pretty_name, :description, :meeting_time, :meeting_place, :meeting_note,
      'active', :viewer_id, sysdate)"
    # redirect to admin page where newly created group is visible at top of list
    ns_returnredirect "admin/admin.tcl" 
} else {
    # update group number $edit_id
    if { $viewer_is_admin == 0 } {
	;# is viewer able to edit this group?
	if { $edit_id == 1 } {
	    ns_return 200 text/plain "Only an administrator can edit this group."
	    # stop the execution of this script
	    return
	}
	set sql_query "select count(*) from user_group_map where
	 user_id = :viewer_id and group_id = :edit_id and role = 'leader'"
	if { 0 == [ db_string my_count $sql_query ] } {
	    ns_return 200 text/plain "Only an administrator or a leader of this group can edit it."
	    # stop the execution of this script
	    return
	}
    }
    # if no errors, update groups table and go to display group info page
    db_dml update_user "update groups
     set short_name = :short_name, pretty_name = :pretty_name, description = :description,
      meeting_time = :meeting_time, meeting_place = :meeting_place,
      meeting_note = :meeting_note where group_id = :edit_id"
    # redirect to display group info page to view updates
    ns_returnredirect "display_group_info.tcl?display_id=$edit_id" 
}







