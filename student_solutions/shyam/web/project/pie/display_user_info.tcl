# PS-2 Written by Shyam Visweswaran at ADUni, 15 April 2001
# -- edited by todd sjoblom 20 April 2001

# display_user_info.tcl - target of various .tcl files in various directories

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "login.tcl" }

# test next for a valid viewer_id
set viewer_id [get_viewer_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "login.tcl" }

set display_id [ns_queryget "display_id"]

# test for admin status and then for leader status
set viewer_is_admin 0 ;# preset
set viewer_is_leader 0 ;# preset
set viewer_is_member 0 ;# preset 
if { [is_user_in_group $viewer_id admin] == 1 } {
    set viewer_is_admin 1
} else {
    ;# is viewer able to edit add to any group?
    set sql_query "select count(*) from user_group_map where
     user_id = :viewer_id and role = 'leader'"
    if { 0 < [ db_string my_count $sql_query ] } { 
	set viewer_is_leader 1
    } elseif { $viewer_id != $display_id } {
	;# query only if we were not otherwise showing the email.
	;#, We show email to :viewer_id == member of a group led by $display_id
	set sql_query "select count(*)
	 from user_group_map v, user_group_map d
         where v.group_id = d.group_id
	 and v.user_id = :viewer_id  and v.role = 'member'
         and d.user_id = :display_id and d.role = 'leader'"
	if { 0 < [ db_string my_count $sql_query ] } { 
	    set viewer_is_member 1
	}
    }
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
 from users where user_id = :display_id"

if { [ db_0or1row get_user_info $sql_query ] == 0 } { 
    ns_return 200 text/plain "There is no such user number $display_id.  Please hit Back."
    # stop the execution of this script
    return
}

set page_actions ""
if { $viewer_is_admin == 1 || $viewer_id == $display_id } {
    set page_actions "<span class=action><a href=\"edit_user_info.tcl?edit_id=$display_id\">Edit</a></span>"
}

append page_content "
[html_page_header 0 "$name_prefix $first_names $last_name $name_suffix" "<a href=\"home.tcl\">Home</a> > Member info" $page_actions]
<!-- stuff for body of page -->

<table width=90%>
<tr><td>
<ul>"

if { ![string equal $phone_home ""] } {
    append page_content "<li>Home phone: $phone_home"
}

if { ![string equal $phone_work ""] } {
    append page_content "<li>Work phone: $phone_work"
}

append page_content "<li>Member since $s_registration_date"

if { $viewer_is_admin == 1 || $viewer_is_leader == 1 || $viewer_id == $display_id || $viewer_is_member } {

    append page_content "<li>Email: <a href=\"mailto:$email\"><i>$email</i></a>"
    if { $email == "" || $viewer_is_admin == 0 } {
	;#
    } elseif { $email_bouncing_p == "t" } {
	append page_content " (Bounces)"
    } else {
	append page_content " (OK)"
    }
}

if { $viewer_is_admin == 1 } {

    append page_content "<li>Last visit: $s_last_visit
    <li>Creator: [get_user_name $creation_user]
    <li>Status: $status"

    if { $banning_user > 0 } {
	append page_content "<li>Banning person: [get_user_name $banning_user]
	<li>Banning date: $s_banning_date
	<li>Banning note: $banning_note"
    }
}

# we want to display the user_group_map of the display_id here

#ns_log Warning "at dui 1"

append page_content "</ul>
<h3>In these Groups</h3>
<ul>"

set sql_group_query "select m.group_id, m.role, g.pretty_name 
 from user_group_map m, groups g
 where m.user_id = :display_id and m.group_id = g.group_id"

#ns_log Warning "at dui 2 $sql_group_query"

db_foreach all_groups $sql_group_query {

    append page_content "<li><a href=\"display_group_info.tcl?display_id=$group_id\">$pretty_name</a> &nbsp; &nbsp"
    #ns_log Warning "at dui 3"
    
    if { $viewer_is_admin == 0 || $viewer_id == $display_id } { ;# can't remove
	if { $group_id == 1 } {
	    append page_content " (administrator) " ;# built-in role
	} else {
	    append page_content " ($role) "
	}
    } else {
	#ns_log Warning "at dui 4"
	append page_content "\[ <b><a href=\"admin/do_action.tcl?user_id=$display_id&group_id=$group_id&action=inactive"
	if { $group_id == 1 } {
	    append page_content "\">Remove as administrator</a></b> \]"
	} else {
	    append page_content "&role=$role\">Remove as $role</a></b> \]"
	}
    }

} if_no_rows {
    append page_content "Not in any groups."
}

if { $viewer_is_admin == 1 || $viewer_is_leader == 1} {

    append page_content "</ul>
    <h3>Enroll $first_names into my Groups</h3>
    <ul>"

    set sql_group_query "select g.group_id, g.pretty_name 
     from groups g
     where not exists (select * from user_group_map m
      where m.user_id = :display_id and m.group_id = g.group_id)"

    if { $viewer_is_admin == 0 && $viewer_is_leader == 1 } {
	append sql_group_query " and g.group_id in 
	 (select me.group_id from user_group_map me where
         me.role = 'leader' and me.user_id = :viewer_id)"
    }

    db_foreach all_groups $sql_group_query {

	append page_content "<li><a href=\"display_group_info.tcl?display_id=$group_id\">$pretty_name</a> &nbsp; &nbsp; "
	set s_activate "\[ <b><a href=\"admin/do_action.tcl?user_id=$display_id&group_id=$group_id&action=active"
	if { $group_id == 1 } {
	    append page_content "$s_activate\">Add as administrator</a>"
	} else {	    
	    append page_content " $s_activate&role=member\">Add as Member</a>
	        | <a href=\"admin/do_action.tcl?user_id=$display_id&group_id=$group_id&action=active&role=leader\">Add as Leader</a>"             
	}
	append page_content "</b> \]"

    } if_no_rows {
	append page_content "Already enrolled."
    }
}

set s_questions "" ;# preset
# grab any questions posted
set sql_query "select content_id, title, to_char(posting_time, 'Month dd, yyyy') \"posting_time\"
    from discussions
    where posting_user = :display_id
    and parent_id is null"
db_foreach get_question_info $sql_query {
    append s_questions "<li><a href=\"content/discussion-show.tcl?content_id=$content_id\">$title</a> (Asked on $posting_time)"
} if_no_rows {
    set s_questions "No postings"
}

append page_content "</ul>"

set s_answers "" ;# preset
# grab any responses to questions
set sql_query "select distinct d1.content_id, d1.title,
    GREATEST(to_char(d2.posting_time, 'Month dd, yyyy')) \"posting_time\"
    from discussions d1, discussions d2
    where d1.parent_id is null
    and d2.parent_id is not null and d2.posting_user = :display_id
    and d2.parent_id = d1.content_id"
db_foreach get_question_info $sql_query {
    append s_answers "<li><a href=\"content/discussion-show.tcl?content_id=$content_id\">$title</a> (Response posted on $posting_time)"
} if_no_rows {
    if { [ string equal $s_questions "No postings" ] == 1 } {
	set s_questions "" ;# reset, leaving $s_answers == ""
    } else {
	set s_answers "No responses" ;# we have questions but no answers
    }
}

set s_articles "" ;# preset
# grab any articles contributed
set sql_query "select content_id, title, to_char(posting_time, 'Month dd, yyyy') \"posting_time\"
    from articles
    where posting_user = :display_id
    and parent_id is null"
db_foreach get_article_info $sql_query {
    append s_articles "<li><a href=\"content/article-show.tcl?content_id=$content_id\">$title</a> (Article posted on $posting_time)"
} if_no_rows {
    set s_articles "No postings"
}

set s_responses "" ;# preset
# grab any responses to articles
set sql_query "select distinct a1.content_id, a1.title,
    GREATEST(to_char(a2.posting_time, 'Month dd, yyyy')) \"posting_time\"
    from articles a1, articles a2
    where a1.parent_id is null
    and a2.parent_id is not null and a2.posting_user = :display_id
    and a2.parent_id = a1.content_id"
db_foreach get_article_info $sql_query {
    append s_responses "<li><a href=\"content/article-show.tcl?content_id=$content_id\">$title</a> (Response posted on $posting_time)"
} if_no_rows {
    if { [ string equal $s_articles "No postings" ] == 1 } {
	set s_articles "" ;# reset, leaving $s_responses == ""
    } else {
	set s_responses "No responses" ;# we have articles but no responses
    }
}

if { [string equal "$s_questions$s_answers$s_articles$s_responses" "" ] == 1 } {
    append page_content "<h3 >No Contibutions to Content</h3>"
} else {
    if { [string equal "$s_questions$s_answers" "" ] != 1 } {
	append page_content "<h3>Questions</h3><ul>$s_questions</ul><ul>$s_answers</ul>"
    }
    if { [string equal "$s_articles$s_responses" "" ] != 1 } {
	append page_content "<h3>Articles</h3><ul>$s_articles</ul><ul>$s_responses</ul>"
    }
}

append page_content "
</td></tr>
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"

db_release_unused_handles

ns_return 200 text/html $page_content

