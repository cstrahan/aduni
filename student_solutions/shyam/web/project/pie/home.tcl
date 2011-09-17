# Written by todd sjoblom 18-20 April 2001 from admin.tcl as a basis
# home.tcl - target of login.tcl, relogin.tcl

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "relogin.tcl" }

# test next for a valid user_id
set viewer_id [get_viewer_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "login.tcl" }

# we need the viewer's name and 2nd-to-last login.  Let's do both in one query, rather
# than also calling the library [get_user_name ...]
set sql_query "select u.name_prefix, u.first_names, u.last_name, u.name_suffix,
 trunc(sysdate - u.second_to_last_visit + 0.1, 1) as days_gone_by 
 from users u where u.user_id = :viewer_id"
if { [db_0or1row get_user $sql_query] == 1 } {
    set viewer_name "$name_prefix $first_names $last_name $name_suffix"
} else {
    ns_return 200 text/plain "There is no user with id $viewer_id."
    # stop the execution of this script
    return
}    

# We are going to filter the page to display recent information first.
if { [ns_queryexists days_ago] } {
    set days_ago [ns_queryget days_ago]
} else {
    set days_ago 7 ;# this is the default
}	
set i_time_args [list $days_gone_by 7 -1 ] ;# this could give "8 7 -1" where 8 is what's new to user
set s_time_names {"new" "recent" "all"}
if { $days_ago == 7 } {
    set s_time "Recent"
} elseif { $days_ago == -1 } {
    set s_time "All"
} else {
    set s_time "New"
}


# build the links to redisplay the time choices
set time_actions "[html_action $i_time_args $s_time_names "home.tcl?days_ago=" $days_ago ]"

# no need to test for user status - anyone can see their own home page

# what groups is viewer_id in, in any role?
set viewer_is_admin 0 ;# preset as not in group 'admin'
set viewer_is_leader 0 ;# preset as not in role 'leader'
set i_groups {} ;# the distinct groups viewer is mapped to

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

set search_box "<form method=post action=\"search.tcl\">
    <input type=text size=20 name=search>
    <input type=submit value=\"Search\"></form>"

set page_content "
[html_page_header 0 $viewer_name "Home" $time_actions $search_box]

<!-- Stuff for top of page -->
<table width=90%>
<tr><td>
"

if { $viewer_is_admin == 1 } {
    append page_content "\[<b><a href=\"admin/admin.tcl?days_ago=$days_ago\">Administration</a> | "
} else {
    append page_content "\[<b>"
}

append page_content " <a href=\"display_user_info.tcl?display_id=$viewer_id\">My info</a>
 | <a href=\"edit_user_info.tcl?edit_id=$viewer_id\">Edit my info</a>
 | <a href=\"password.tcl?pwd_id=$viewer_id\">Change password</a>
 | <a href=\"content/discussion.tcl?forum=Menopause\">Menopause Q and A</a>
"
if { $viewer_is_admin == 1 || $viewer_is_leader == 1 } {
    append page_content "| <a href=\"content/discussion.tcl?forum=Physician\">Physician Q and A</a>"
}

append page_content "
 | <a href=\"content/article.tcl\">Articles</a>
 </b> \]
<hr size=1>
</td></tr>
<tr><td>
"

#Contents -- Responses

set sql_query "select count(*) from discussions
 where posting_user = :viewer_id and parent_id is null"

if { 0 ==  [ db_string my_count $sql_query ] } {
    set s_other "$s_time" ;# used in next section
} else {
    if { $s_time == "All" } { set s_other "All other" } else { set s_other "Other [string tolower $s_time]" }
    ;# at least 1 pending question, even if very old

    append page_content "<font size=+1><b>$s_time responses to my questions</b></font>
    <ul>"

    set s_posting_time ""
    if { $days_ago >= 0 } { set s_posting_time " and d.posting_time >= sysdate - :days_ago" }

    set sql_query "select myd.content_id, myd.title, count(*)-1 as my_count
     from discussions d, discussions myd where myd.posting_user = :viewer_id
     and myd.parent_id is null and ((d.parent_id = myd.content_id $s_posting_time)
     or d.content_id = myd.content_id)
     group by myd.content_id, myd.title
     order by myd.content_id desc"
    set i_row 0 ;# use parity for color of rows

    db_foreach get_discuscssions $sql_query {
	append page_content "<li><a href=\"content/discussion-show.tcl?content_id=$content_id\">$title</a> ($my_count response"
	if { $my_count != 1 } { append page_content "s" } ;# zero or plural
	append page_content ")"
    } if_no_rows {
	append page_content "Nothing found"
    }
} ;# responses to my questions

append page_content "</ul>
<font size=+1><b>$s_other questions</b></font>
 - \[ <b><a href=\"content/discussion-new.tcl\">Ask a Question</a></b> \]
<ul>"

set sql_query "select d.title, d.content_id, 
 u.user_id, u.name_prefix, u.first_names, u.last_name, u.name_suffix
 from discussions d, users u where u.user_id = d.posting_user
 and d.posting_user != :viewer_id
 and d.parent_id is null"

if { $days_ago >= 0 } { append sql_query " and (d.posting_time >= sysdate - :days_ago)" }
append sql_query " order by d.posting_time desc"

db_foreach get_discussions $sql_query {
    append page_content "<li><a href=\"content/discussion-show.tcl?content_id=$content_id\">$title</a> 
by <a href=\"display_user_info.tcl?display_id=$user_id\">
      $name_prefix $first_names $last_name $name_suffix</a>"
} if_no_rows {
    append page_content "Nothing found."
}

#ns_log Warning "about to do articles"
#Contents -- Articles

append page_content "</ul>
<font size=+1><b>$s_time articles "
if { $viewer_is_leader == 0 } {
    append page_content " by my group leaders"
}

append page_content "</b></font>"

if { $viewer_is_leader == 0 } {
    append page_content " - \[ <b><a href=\"content/article.tcl\">All Articles</a></b> \]"
} else {
    append page_content " - \[ <b><a href=\"content/article-new.tcl\">Post an Article</a></b> \]"
}

append page_content "<ul>"

set sql_query "select a.content_id, a.title,
 u.user_id, u.name_prefix, u.first_names, u.last_name, u.name_suffix
 from articles a, users u
 where u.user_id = a.posting_user and a.parent_id is null"

if { $viewer_is_leader == 0 } {
    append sql_query " and a.posting_user in 
     (select m2.user_id from user_group_map m1, user_group_map m2
     where m1.user_id = :viewer_id and m2.role = 'leader'
     and m1.group_id = m2.group_id)"
}

if { $days_ago >= 0 } { append sql_query " and (a.posting_time >= sysdate - :days_ago)" }
append sql_query " order by a.posting_time desc"

db_foreach get_articles $sql_query {

    append page_content "<li><a href=\"content/article-show.tcl?content_id=$content_id\">$title</a>
 by <a href=\"display_user_info.tcl?display_id=$user_id\">
    $name_prefix $first_names $last_name $name_suffix</a>"

} if_no_rows {
    append page_content "Nothing found."
}

#end of Content

# Groups
#Groups
append page_content "</ul>
<font size=+1><b>My groups</b></font>
<ul>"

#ns_log Warning "about to do group"
set n_active 0 ;# preset number of active groups

foreach i_group $i_groups {
    #ns_log Warning "about to do group 2"

    set sql_query "select g.group_id, g.status, 
     g.short_name, g.pretty_name, g.meeting_time, g.meeting_note  
     from groups g, user_group_map m where
     g.group_id = :i_group and g.group_id = m.group_id
     and m.user_id = :viewer_id 
     and (g.status = 'active' or :viewer_is_admin = 1)"

    #ns_log Warning "about to do group 3"

    if { [db_0or1row get_groups $sql_query] == 1 } {

	append page_content "
	 <li><a href=\"display_group_info.tcl?display_id=$group_id\">$pretty_name $short_name</a>"
	if { $status == "active" } {
	    if { ![string equal $meeting_time ""] } {
		append page_content " (Next meeting: <b>$meeting_time</b>)"
	    }
	} else { ;# viewer_is_admin
	    append page_content "
	     <a href=\"edit_group_info.tcl?edit_id=$group_id\">Activate</a>"
	}
	if { ![string equal $meeting_note ""] } {	
	    append page_content "&nbsp; &nbsp; \[Note: $meeting_note\]"	
	}
	incr n_active
    }
}

if { $n_active == 0 } {
    append page_content "You don't seem to be in any active groups."
}

append page_content "
</ul>
<td></tr>
</table>

[html_page_footer 0]    
"

db_release_unused_handles

ns_return 200 text/html $page_content




