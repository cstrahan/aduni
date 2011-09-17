# PS-2 Written by Shyam Visweswaran at ADUni, 15 April 2001
# -- edited by todd sjoblom 19-21 April 2001

# do_action.tcl - target for admin.tcl or ../displayxxx or ../editxxx and self-referential target for itself.
# 4 actions upon table users. groups, or user_group_map:
# 1. active/inactive user --done with no prompt, but are easily undone
# 2. active/inactive group (group != 1) -- done with no prompt, easily undone, except we activation clears previous inactivation user/dater
# 3. active/inactive causes insert/remove of a user_group_map record (role = ""/member/leader) -- easily undone -- but don't let user_id == viewer_id
# 4. ban a user, for which we require a note, and call do_action.tcl itself to process that note and do an update

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "../login.tcl" }

# test next for a valid viewer_id
set viewer_id [get_viewer_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "../login.tcl" }
set viewer_name [get_user_name $viewer_id]

# test for admin status and then leader status
set viewer_is_admin 0 ;# preset
set viewer_is_leader 0 ;# preset
if { [is_user_in_group $viewer_id admin] == 1 } {
    set viewer_is_admin 1
}

set_the_usual_form_variables
# action.  1 (or 2) of user_id and group_id.  If both, then possibly also role.

if { ! [ns_queryexists action] } {
    ns_return 200 text/plain "There is no action to do.  Please hit Back."
    # stop the execution of this script
    return
}
if { ![ns_queryexists user_id] } { set user_id 0 }
if { ![ns_queryexists group_id] } { set group_id 0 }
if { $user_id == 0 && $group_id == 0 } {
    ns_returnredirect "../login.tcl"
} elseif { $user_id > 0 && $group_id > 0 } {
    ;# action type 3
    if { $group_id == 1 } {
	if { $viewer_is_admin == 0 || [ns_queryexists role] } {
	    ns_returnredirect "../home.tcl"
	}
    } else { ;# >1
	if { ![ns_queryexists role] } {
	    ns_returnredirect "../home.tcl"
	} elseif { $viewer_is_admin == 0 } {
	    set sql_query "select count(*) from user_group_map where
	     user_id = :viewer_id and group_id = :group_id and role = 'leader'"
	    if { 0 < [ db_string my_count $sql_query ] } { set viewer_is_leader 1 }
	    if { $viewer_is_leader == 0 } {
		ns_returnredirect "../home.tcl"
	    }
	}
    }
}

set item_name ""
if { $user_id > 0} {
    ;# action type 1 3 4
    set item_name [ get_user_name $user_id]
}
if { $group_id > 0 } {
    ;# action type 2 3 
    if { $user_id == 0 && $group_id == 1 } {
	;# prevent action type 2 on 'admin' group
	ns_returnredirect "../display_group_info.tcl?display_id=$group_id"
    }
    if { $user_id > 0 && $group_id > 1} { append item_name " $role of " }
    db_0or1row item_stmt "select pretty_name, short_name from groups where group_id = :group_id"
    append item_name " Group $pretty_name $short_name"
}

set prompt "$viewer_name, you are about to make $item_name $action." ;# may not show
set sql_stmt "" ;# preset

if { $user_id > 0 } {
    ;# action type 1 3 or 4
    if { $action == "banned" } {
	;# action type 4 requires a banning note fro auditing trail
	append prompt "<p>Please give a reason:"
	if { [ns_queryexists note] } {
	    if { [string equal [string trim $note] ""] } {
		;# not good enough
		set prompt "<font color=red>$prompt</font>"
	    } else {
		;#set note [ns_dbquotevalue $note]
		set sql_stmt "update users
		 set users.status = :action, users.banning_user = :viewer_id,
		 users.banning_date = sysdate, users.banning_note = :note
		 where user_id = :user_id"
	    }
	}
	;# Note that if if  sql_stmt still == "", we won't yet do a sql update
    } elseif { $action == "active" || $action == "inactive" } {
	;# action type 1 or 3
	if { $group_id == 0 } {
	    ;# action type 1
	    set sql_stmt "update users set users.status = :action where user_id = :user_id"
	} else {
	    ;# action type 3
	    #ns_log Warning "Trying to make $user_id in Group $group_id $action, please"
	    # Note -- we don't select to see if a 2nd admin has just done this action to the map moments ago.
            # Instead, the error log will show the named uniqueness constraint.
	    # The 1st admin will see a server error, but then reload will show that their desire was actually performed.
	    if { $action == "active" } {
		if { $group_id == 1 } {
		    set sql_stmt "insert into user_group_map (user_id, group_id) values (:user_id, :group_id)"
		} else {
		    set sql_stmt "insert into user_group_map (user_id, group_id, role) 
		    values (:user_id, :group_id, :role)"
		}
	    } else { ;# inactivate
		if { $user_id == $viewer_id } {
		    append prompt "  Please get another adminstrator to remove you from this group."
		} else {
		    set sql_stmt "delete from user_group_map where 
		    user_id = :user_id and group_id = :group_id"
		    # not needed since role is unique# if { $group_id > 1 } { append sql_stmt " and role = :role" }
		}
	    }
	}
    } else { ;# don't do this unknown action on users
	ns_returnredirect  "../display_user_info.tcl?display_id=$user_id"
    }
    if { [string equal $sql_stmt ""] == 0 } {
	;# not empty, so we try
	db_dml item_stmt $sql_stmt
	if { $group_id == 0 } {
	    ;# we were editing a user
	    ns_returnredirect "../edit_user_info.tcl?edit_id=$user_id"
	} else {
	    ;# we were in the midst of modifying the list of displayed user's group memberships
	    ns_returnredirect "../display_user_info.tcl?display_id=$user_id"
	}
    } else {
	;# we need to display an input prompt to the viewer_id person, to get banning information
	set page_content "
	[html_header "$action"]
	<p>
	<div class=title>$action</div>
	<div class=navigation><a href=../home.tcl>Home</a> >
	<a href=\"../display_user_info.tcl?display_id=$user_id\">Display Info</a> > $action</div>
	<hr>
	$prompt
	<!-- this form will send the information to this script -->
	<form method=post action=do_action.tcl>
	
	<input type=hidden name=action value=$action>
	<input type=hidden name=user_id value=$user_id>"
	if { $action == "banned" } {
	    ;# action type 4
	    append page_content "<TEXTAREA COLS=50 ROWS=10 WRAP=hard NAME=note></TEXTAREA>"
	} else { ;# here we don't need a note, but we
	    ;# may decide we need to show our prompt and the submit button for viewer to validate.
	    append page_content "<input type=hidden name=note value=none>"
	}
	append page_content "<p> 
	<input type=reset value=\"Reset\">
	&nbsp &nbsp <input type=submit value=\"Submit\">
	</form>
	<p>
	[html_footer]"
	
	ns_return 200 text/html $page_content
    }
    ;# else we just (re)displayed prompt to get banning information from the viewer_id person
} elseif { $group_id > 0 } {
    ;# action type 2
    if { $action == "active" || $action == "inactive" } {
	set sql_stmt "update groups
	 set groups.status = :action"
	if { $action == "inactive" } {
	    append sql_stmt ", groups.inactivation_user = :viewer_id,
	    groups.inactivation_date = sysdate"
	} else {
	    append sql_stmt ", groups.inactivation_user = null,
	    groups.inactivation_date = null" ;# these work properly
	}
    	append sql_stmt " where group_id = :group_id"
	db_dml item_stmt $sql_stmt
    }
    ns_returnredirect  "../edit_group_info.tcl?edit_id=$group_id"
}




