# quotations-1.tcl - outputs the search results page
# Written by shyam Visweswaran at ADUni

# grab 'killed' quotation ids from cookie to personlize the search results
# by restricting the search results to only the 'non-killed' quotations

if { [ns_set get [ns_conn headers] Cookie] != "" } {
    set cookie_value [ns_set get [ns_conn headers] Cookie]
    regexp {kill_id=([0-9\|]+)} $cookie_value match kill_id
    regsub -all {\|} $kill_id "," kill_id_sql
    set sql_kill "where quotation_id not in ($kill_id_sql)"
} else {
    set kill_id ""
    set sql_kill ""
}

# grab the search term from the form by getting the value for variable search

if { [ns_queryexists "search"] && [string length [ns_queryget "search"]] > 0 } {
    set search_word [string trim [ns_queryget "search"]]
    if { [string length $sql_kill] == 0 } {
	set sql_search " where upper(quote) like upper('%$search_word%') or upper(author_name) like upper('%$search_word%')"
    } else {
	set sql_search " and upper(quote) like upper('%$search_word%') or upper(author_name) like upper('%$search_word%')"
    }
} else {
   set sql_search ""
}

# figure out  ordering of the quotes by date or category

if { [ns_queryexists "order_by"] && [ns_queryget "order_by"] == "date" } {
	set sql_query "select * from quotations $sql_kill $sql_search order by insertion_date"
	# sadly we don't have abstract URLs working on a raw AOLserver so we 
	# need to link to an explicit .tcl extension
	set options " <a href=\"quotations-1.tcl?order_by=category&search=$search_word\">Sort by category</a>"
} else {
	set sql_query "select * from quotations $sql_kill $sql_search order by category"
	set options " <a href=\"quotations-1.tcl?order_by=date&search=$search_word\">Sort by insertion date</a>"
}

# start html page content

set page_content "
<html>
<head>
<title>Quotations</title>
</head>
<body bgcolor=white text=black>
<blockquote>

<table width=90%>
<tr><td> &nbsp </td>
<td align=right> <a href=\"quotations.tcl\">Quotations Home</a> | <a href=../>Home</a> </td></tr>
<tr><td> <h2>Quotations search results</h2> </td>
<td align=right> 
<form method=post action=\"quotations-1.tcl\">
 <input type=text size=30 name=search value=\"$search_word\">
 <input type=submit value=\"Search\">
</form>
</td></tr></table>

<hr>

<font face=helvetica,arial>
<ul>
"
# This next command is part of the Arsdigita Database Access API.
# db_foreach performs the query specified by $sql_query.  The name
# "get_quotes" is a logical name for the sql_query.  For each row that
# is returned by the query, db_foreach will perform the commands
# within the first code block.  If no rows are returned by the query,
# then the commands within the code block following "if_no_rows".  See
# http://philip.greenspun.com/teaching/manuals/db-api/ for a brief
# overview of the database API, and
# http://www.arsdigita.com/doc/core-arch-guide/database-access-api for
# even more of the gory details.

db_foreach get_quotes $sql_query {
    if { [string equal $kill_id ""] } {
	append page_content "<li>$author_name: \"$quote\"\n"
    } else {
	append page_content "<li>$author_name: \"$quote\"\n"
    }
} if_no_rows {
    append page_content "No quotations matching your search could be found."
    set options ""
}

# db_release_unused_handles releases any unused database handles.  The
# database API automatically allocates database handles when they are
# needed.  This procedure will be called automatically when the page is
# finished executing, but on a high-volume site you always want to
# release explicitly after the last time you access the database on a
# page.  Why?  You don't want to be holding onto a database connection,
# a scarce resource, while writing bytes out to the browser.  Suppose
# that the user is in Croatia on a 9600 baud modem.  It might take 15
# minutes to deliver a big page to him or her.  If you have configured 4
# database connections and you have four users like this you'll have
# launched a denial of service attack on your own server!  In other
# words nobody else will be able to get a page if that page requires the
# RDBMS.

append page_content "
</ul>
</font>

<p>
$options

<hr>

<address><a href=\"mailto:shyam@massmed.org\">shyam@massmed.org</a></address>

</blockquote>
</body>
</html>
"
db_release_unused_handles

ns_return 200 text/html $page_content






