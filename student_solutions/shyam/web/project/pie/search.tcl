# PS-2 Written by Shyam Visweswaran at ADUni, 16 April 2001
# added to by todd sjoblom 18 April, 2001
# search.tcl - target of home.tcl and admin/admin.tcl

# test first for a valid session_id
set session_id [get_session_id]
if { $session_id == 0 } { ns_returnredirect "login.tcl" }

# test next for a valid viewer_id
set viewer_id [get_user_id $session_id]
if { $viewer_id == 0 } { ns_returnredirect "login.tcl" }

# test for admin status
set viewer_is_admin 0 ;# preset
if { [is_user_in_group $viewer_id admin] == 1 } {
    set viewer_is_admin 1
}

set_the_usual_form_variables
# we expect one of: users groups contents discussions articles search

# now set the search term s_search for any 1 of 3 tables

set is_users 0 ;# preset
set is_discussions 0
set is_articles 0
set is_groups 0
if { [info exists users] } {
    set is_users 1
    set s_search $users
} elseif { [info exists discussions] } {
    set is_discussions 1
    set s_search $discussions
} elseif { [info exists articles] } {
    set is_articles 1
    set s_search $articles
} elseif { [info exists contents] } { ;# both types
    set is_discussions 1
    set is_articles 1
    set s_search $contents
} elseif { [info exists groups] } {
    set is_groups 1
    set s_search $groups
} elseif { [info exists search] } {
    set is_users 1
    set is_discussions 1
    set is_articles 1
    set is_groups 1
    set s_search $search
} else { 
    ns_return 200 text/plain "Go back and enter a search term."
    # stop the execution of this script
    return
}

regsub -all {[^'\w]} $s_search " " s_search ;# remove all punctuation except like O'Reilly and Lab_01
# the internal apostrophe of O'Reily will later be doubled by ns_dbquotevalue
regsub -all {\s+} $s_search " " s_search ;# elide
set s_search [string trim $s_search ]
if { [string equal $s_search ""] } {
    ns_return 200 text/plain "Go back and enter a search term." ;# this does not redraw old page
    # stop the execution of this script
    return
}
set s_search_preserve_case $s_search ;# to display to the user, for possible further refinement
set s_search [string toupper $s_search ]
set s_searches $s_search
split $s_searches ;# the user may have entered multiple terms
#ns_log Warning $s_searches

# we want a contains target like '($dogs AND $cat)' which can actually find dog intersect cats
set s_stems [ ns_dbquotevalue $s_searches ] ;# force apostrophes for O''Reily.  dog''s is OK.
regsub  {^'} $s_stems "'($" s_stems ;# insert ( $-stem after initial apostrophe out of ns_dbquotevalue
regsub  {'$} $s_stems ")'" s_stems ;# insert ) before final apostrophe.  Note, '(dog)' is OK.
regsub  -all {\s} $s_stems " AND $" s_stems ;# insert boolean operator and $ for stemming
#ns_log Warning $s_stems ;# now s_stems has all the terms for up to 2 Intermedia searches on 2 clobs.
# In the foreach below, we'll do a final check to avoid Intermedia keywords.

# But we still need to match any of the other varchar fields.
# Unfortunately, these matches will be exact, and do not allow stemming.  Asymmetric.
# We prepare some variable for our later search on discussions and/or articles.  We assume we do both. 
set s_big_search "" ;# for the difficult search on the 2 content tables 
set upper_target "upper(title || ' ' || category)" ;# target for an exact match in either content
set s_keywords [list AND OR NOT ACCUM MINUS EQUIV NEAR WITHIN ABOUT ] ;# Intermedia keywords throw an error
set i_row 0
foreach s_search $s_searches {  # 
    if { [ lsearch $s_keywords $s_search ] != -1 } {
	# the s_stems that we built above would fail.
	ns_return 200 text/plain "Go back and enter a search without the keyword $s_search."
	# this does not redraw old page
	# stop the execution of this script
	return
    }
    set s_search [ ns_dbquotevalue %$s_search% ]
    set s_stem_search [ ns_dbquotevalue %$s_search% ]
    if { $i_row > 0 } { append  s_big_search " and " } 
    append  s_big_search "$upper_target like $s_search" ;# note that we don't use :binding.
    ;# There are no ;'s to endanger us.
    incr i_row
}
set s_big_search "(($s_big_search) or contains(content, $s_stems, 1) > 0)"
#ns_log Warning $s_big_search
set s_no_match "" ;# preset



set page_content "
[html_head "Search results"]
[html_body_start]
"

set page_actions "<form method=post action=\"search.tcl\">
                  <input type=text size=20 name=search value=\"$s_search_preserve_case\">
                  <input type=submit value=\"Search\"></form>"

append page_content "
[html_body_top "Search results for '$s_search_preserve_case'" "<a href=home.tcl>Home</a> > Search results" "#bbbbbb" $page_actions]

<!-- stuff for body of page -->

<table width=90%>
"

set search_questions "" ;# preset
if { $is_discussions == 1 } {
    append search_questions "<tr><td>&nbsp;</td></tr>"
    append search_questions "<tr><td colspan=3><h2>Questions & Answers</h2></td></tr>"

    set sql_search "
     select title, content_id from discussions, 
     (select content_id as c_id from discussions
       where parent_id is null and $s_big_search 
      union
      select parent_id as c_id from discussions
       where parent_id is not null and $s_big_search
     ) c where content_id = c_id order by content_id desc"
    set i_row 0 ;# use parity for color of rows
    db_foreach get_contents $sql_search {
	set s_color [ row_color $i_row ]
	append search_questions "<tr>
	  <td $s_color colspan=3><a href=\"content/discussion-show.tcl?content_id=$content_id\">
	   $title</a> </td>
	  </tr>"
	incr i_row
    } if_no_rows {
	set search_questions "" ;# reset
	append s_no_match  "<tr><td colspan=3>No match in Questions & Answers.</td></tr>"
    }
}

# Articles -- similar to above paragraph; could be collapsed in a foreach loop
set search_articles "" ;# preset
if { $is_articles == 1 } {
    append search_articles "<tr><td>&nbsp;</td></tr>"
    append search_articles "<tr><td colspan=3><h2>Articles & Responses</h2></td></tr>"
    set sql_search "
     select title, content_id from articles, 
     (select content_id as c_id from articles
       where parent_id is null and $s_big_search 
      union
      select parent_id as c_id from articles
       where parent_id is not null and $s_big_search
     ) c where content_id = c_id order by content_id desc"
    set i_row 0 ;# use parity for color of rows
    db_foreach get_contents $sql_search {
	set s_color [ row_color $i_row ]
	append search_articles "<tr>
	  <td $s_color colspan=3><a href=\"content/article-show.tcl?content_id=$content_id\">
	   $title</a> </td>
	  </tr>"
	incr i_row
    } if_no_rows {
	set search_articles "" ;# reset
	append s_no_match "<tr><td colspan=3>No match in Articles & Responses.</td></tr>"
    }
}

if { [string equal $s_no_match "" ] != 1 } {
    append s_no_match  "<tr><td colspan=3>&nbsp;&nbsp;(Titles were all searched for an <i>exact</i> match,
         but text is searched by root word, with the indexing for today's new text done tonight.)</td></tr>"
}

set search_groups "" ;# preset
if { $is_groups == 1 } {
    append search_groups "<tr><td>&nbsp;</td></tr>"
    append search_groups "<p><tr><td colspan=3<h2>Groups</h2></td></tr>"
    set sql_search "select group_id, short_name, pretty_name, description,
     meeting_time, meeting_place, meeting_note
     from groups"
    set upper_target "upper(short_name || ' ' || pretty_name || ' ' || description || ' ' ||
      meeting_time || ' ' || meeting_place || ' ' || meeting_note)"
    set i_row 0
    foreach s_search $s_searches {
	set s_search [ ns_dbquotevalue %$s_search% ]
        if { $i_row == 0 } { append sql_search " where " } else { append sql_search " and " } 
	append sql_search "$upper_target like $s_search" ;# note that we don't use :binding.  There are no ;'s.
	incr i_row
    }
    append sql_search " order by pretty_name"
    set i_row 0 ;# use parity for color of rows
    db_foreach get_groups $sql_search {
	set s_color [ row_color $i_row ]
	set s_meeting $meeting_time
	if { [string equal $meeting_place ""] == 0 } {
	    if { [string equal $s_meeting ""] == 0 } { append s_meeting " - " }
	    append s_meeting $meeting_place
	}
	if { [string equal $meeting_note ""] == 0 } {
	    if { [string equal $s_meeting ""] == 0 } { append s_meeting " - " }
	    append s_meeting $meeting_note
	}
	append search_groups "<tr>
	  <td $s_color> <a href=\"display_group_info.tcl?display_id=$group_id\">
	   $pretty_name $short_name</a> </td>
	<td $s_color>$description </td><td $s_color>$s_meeting</td></tr>"
	incr i_row
    } if_no_rows {
	set search_groups ""
	append s_no_match "<tr><td colspan=2>No exact match in Groups (type, name, or any meeting information).</td></tr>"
    }
}

set search_users "" ;# preset
if { $is_users == 1 } {
    append search_users "<tr><td>&nbsp;</td></tr>"
    append search_users "<tr><td colspan=3><h2>Match in Users</h2></td></tr>"
    set sql_search "select user_id,
     name_prefix, first_names, last_name, name_suffix
     from users"
    set upper_target "upper(name_prefix || ' ' || first_names || ' ' || last_name || ' ' || name_suffix)"
    set i_row 0
    foreach s_search $s_searches {
	set s_search [ ns_dbquotevalue %$s_search% ]
        if { $i_row == 0 } { append sql_search " where " } else { append sql_search " and " } 
	append sql_search "$upper_target like $s_search" ;# note that we don't use :binding.  There are no ;'s.
	incr i_row
    }
    append sql_search " order by first_names, last_name"
    set i_row 0 ;# use parity for color of rows
    db_foreach get_hits $sql_search {
	set s_color [ row_color $i_row ]
	append search_users "<tr>
	  <td $s_color colspan=3> <a href=\"display_user_info.tcl?display_id=$user_id\">
	  $name_prefix $first_names  $last_name $name_suffix</a> </td>
	  </tr>"
	incr i_row
    } if_no_rows {
	set search_users ""
	append s_no_match  "<tr><td colspan=2>No exact match in Users (names and titles).</td></tr>"
    }
}

append page_content "$search_questions  $search_articles  $search_groups  $search_users"
if { [string equal $s_no_match "" ] != 1 } {
    set num_spaces [ regsub -all {\s} $s_search_preserve_case "+" s_plus_search ]  ;# show + character to symbolize and
    set s_plus_search "'$s_plus_search'"
    if { $num_spaces > 0 } { set s_plus_search "<i>all</i> the words in $s_plus_search" }
    # only give a message if theree are NO hits.
    # With a little recoding, we can give a message about each miss, and why, or
    # we can put it into a help file and help link
    if { [string equal "$search_questions$search_articles$search_groups$search_users" "" ] == 1 } {
	append page_content "
	<tr><td colspan=3>No match for $s_plus_search</td></tr>"
	# $s_no_match"
    }
}

db_release_unused_handles

append page_content "
</table>

[html_body_bottom]
[html_body_end]
[html_foot]
"

ns_return 200 text/html $page_content




