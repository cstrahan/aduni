# util.tcl - shyam's tcl utilities file
# 12 April 2001
# and todd sjoblom 18apr 2001

proc html_page_header { color page_title {nav_bar ""} {action_list ""} {search_box ""} } {
    set body_color [list "#eeeeee" "#ffffcc" "#edeaea" "#ccffff"]
    set nav_color [list "#bbbbbb" "#99cc99" "#cccc99" "#3399ff"]

    set html "<html>
    <head>
    <link rel=stylesheet type=text/css href=main.css title=main>
    <title>$page_title</title>
    </head>

    <body bgcolor=[lindex $body_color $color]>
    <blockquote>

    <!-- Stuff for top of page -->
    <table width=90%>
    <tr><td>
    <span class=logo>Partners in Education</span>
    </td><td align=right valign=bottom>
    $search_box
    </td>
    
    <tr><td>
    <span class=title>$page_title</span>
    <td align=right valign=bottom>
    <span class=action>$action_list</span>
    </td></tr>

    <tr bgcolor=[lindex $nav_color $color]><td colspan=2>
    <span class=navigation>$nav_bar</span>
    </td></tr>
    </table>"

    return $html
}

proc html_page_footer { color } {
    set copyright_color [list "#bbbbbb" "#99cc99" "#cccc99" "#3399ff"]

    set html " <!-- stuff for bottom of page -->
    
    <table width=90%>
    <tr bgcolor=[lindex $copyright_color $color]><td>
    <span class=copyright><center>Copyright &copy; Partners in Education</center></span>
    </td></tr>
    <tr><td>
    <address><a href=\"mailto:shyam@jipmer.org\">shyam@massmed.org</a></address>
    </table>
    
    </blockquote>
    </body>
    </html>
    "
    return $html
}




proc row_color { row_number } {
    if { [expr $row_number % 2] == 0 } {
	return "bgcolor=#eeeeee"
    } else {
	return
    }
}

proc html_head { page_title {stylesheet main} } {
    set html "<html>
    <head>
    <link rel=stylesheet type=text/css href=$stylesheet.css title=$stylesheet>
    <title>$page_title</title>
    </head>
    "
    return $html
}

proc html_body_start { {body_color #f6f6f6} } {
    set html "<body bgcolor=$body_color>
    <blockquote>
    "
    return $html
}

proc html_body_top { page_title nav {nav_color #bbbbbb} {link ""} } {
    set html "
    <!-- Stuff for top of page -->
    <table width=90%>
    <tr><td>
    <span class=logo>Partners in Education</span>
    <br>
    <span class=title>$page_title</span>
    </td>
    
    <td align=right valign=bottom>
    $link
    </td></tr>

    <tr bgcolor=$nav_color><td colspan=2>
    <span class=navigation>$nav</span>
    </td></tr>
    </table>
    "
    return $html
}

proc html_body_bottom { {copyright_color #bbbbbb } } {
    set html " <!-- stuff for bottom of page -->
    
    <table width=90%>
    <tr bgcolor=$copyright_color><td>
    <span class=copyright><center>Copyright &copy; Partners in Education</center></span>
    </td></tr>
    <tr><td>
    <address><a href=\"mailto:shyam@jipmer.org\">shyam@massmed.org</a></address>
    </table>
    "
    return $html
}

proc html_body_end { } {
    set html "</blockquote>
    </body>
    "
    return $html
}

proc html_foot { } {
 return "</html>"
}



# Return some generic HTML header; takes title as argument
proc html_header {page_title} {
    set html "<html>
    <head>
    <link rel=stylesheet type=text/css href=main.css title=main>
    <title>$page_title</title>
    </head>

    <body bgcolor=\"#f6f6f6\">
    <blockquote>
    <div class=logo>Partners in Education</div>
    <p>
"
return $html
}

# Return some generic HTML footer
proc html_footer {} {
    set html "<hr size=1>
    <address><a href=\"mailto:shyam@massmed.org\">shyam@massmed.org</a></address>
    </blockquote>
    </body>
    </html>
"
return $html
}

# A cookie is sent with session_id that is the current time concatenated with a random number
# Expiration time is 2 hours (=7200 seconds) from current_time
proc make_new_session {user_id} {
    set current_time [clock seconds]
    set rand_number [expr {round(1000000*rand())} ]
    set session_id [concat $current_time$rand_number]
    set expiration_time [expr $current_time + 7200]

    set sql_insert "insert into sessions (user_id, session_id, expiration_time)
                    values (:user_id, :session_id, :expiration_time)"
    db_dml insert_session $sql_insert

    set headers [ns_conn outputheaders]    
    ns_set put $headers Set-Cookie "session_id=$session_id; path=/"
    return
}

# Get a session_id from cookie; return 0 if no session_id found
proc get_session_id { } {
    if { [ns_set get [ns_conn headers] Cookie] == "" } {
	set return_val 0
    } else {
	regexp {session_id=([0-9\|]+)} [ns_set get [ns_conn headers] Cookie] match session_id
	if { [string equal $session_id ""] || $session_id == 0 } {
	    set return_val 0
	} else {
	    set return_val $session_id
	}
    }
    return $return_val
}

# Gets user_id given a session_id; if expired returns 0
proc get_user_id {session_id} {
    set sql_query "select user_id from sessions where session_id = :session_id and expiration_time > [clock seconds]"
    db_foreach get_user_sessions $sql_query {
	set return_val $user_id
    } if_no_rows {
	set return_val 0
    }
    return $return_val
}


# sets user_id given a session_id and if time has not expired
# returns 1 if successful, 0 if no such user or if time has expired
proc get_viewer_id {session_id} {
    set sql_query "select user_id from sessions where session_id = :session_id and rownum <= 1 and expiration_time > [clock seconds]"
    if { [db_0or1row get_viewer_id $sql_query] == 1 } {
	set return_val $user_id
    } else {
	set return_val 0
    }
    return $return_val
}


# library proc to return all the fields pertaining to NAME from USERS
proc get_user_name {user_id} {
    set name ""
    if { [db_0or1row get_success "select * from users where user_id = :user_id"] == 1 } { 
	append name "$name_prefix $first_names $last_name $name_suffix"
	set name [string trim $name]
    }
    if {[string equal $name ""] == 1} { set name  "User $user_id not found" }
    return $name
} ;# get_user_name

proc is_user_in_group {user_id group_short_name} {
    set sql_query "select count(*) from groups g, user_group_map m where g.short_name = :group_short_name and g.group_id = m.group_id and m.user_id = :user_id"
    if { [db_string get_count $sql_query] == 0 } {
	return 0
    } else {
	return 1
    }
}

# proc to build a list of links to display possible actions as links and 
# impossible ones as gray
# todd sjoblom 18apr 2001
proc html_action { s_args s_names s_href s_arg_to_match } {
    set s_actions ""
    set i_temp 0
    foreach s_arg $s_args {
	if { $i_temp > 0 } { append s_actions " - " } 
	set s_name [lindex $s_names $i_temp] ;# reset
	if { $s_arg == $s_arg_to_match } {
	    append s_actions "$s_name"
	} else {
	    append s_actions "<a href=\"$s_href$s_arg\">$s_name</a>"
	}
	set i_temp [expr $i_temp + 1]
    }
    return $s_actions
}


