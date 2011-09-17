# /www/internet-application-workbook/examples-basics/quotation-add-pseudo-code.txt
# This is the target program for the form in /basics/quotations
# modified by Shyam Visweswaran at ADUni
# we expect author_name, category, quote

set exception_count 0 
set exception_text ""

# It's always a good idea to do error checking on information that is
# typed in by users.

if { ![ns_queryexists author_name] || [empty_string_p [ns_queryget author_name]] } {
    incr exception_count
    append exception_text "<li>Posting stuff on the Internet without attribution is rude.  Please enter an author name.\n"
}

if { $exception_count > 0 } {
    # you'll have to implement a procedure that returns a standard 
    # site-wide error page to the user
    return_complaint $exception_count $exception_text
    # terminate execution of this thread (a goto!)
    return
}

# At this point the user's input is reasonably good.

# Now we'll do the insertion in the quotations table.  We use a db_dml
# command that locks the table to protect against simultaneous updates
# from multiple users of this service.  One almost never needs
# explicit "lock table" commands in Oracle.  This is a rare situation
# where we are reading information from the database and then
# performing an update relying on the information we read not to have
# changed.

db_transaction {

    db_dml lock_table "lock table quotations in exclusive mode"

    # the command db_string takes a query name and an sql query
    # as arguments and returns the results of that query as a string

    set new_key [db_string max_quote_id "select nvl(max(quotation_id)+1,1) from quotations"]
    
    # we set the query variables as local variables
    set author_name [ns_queryget author_name]
    set new_category [string trim [ns_queryget new_category]]
    if { [string length $new_category] == 0 } {
	set category [ns_queryget category]
    } else {
	set category [ns_queryget new_category]
    }

    set quote [ns_queryget quote]

    # db_dml is smart enough to fill the SQL bind variables (the ones
    # with ":" in front of them) with values from local variables of
    # the same name
    db_dml insert_quote "insert into quotations
      (quotation_id, insertion_date, author_name, category, quote)
      values
      (:new_key, sysdate, :author_name, :category, :quote)
     "
}

# instead of cluttering up the user interface with an extra page, we 
# simply redirect to the first page for this service.  The user 
# will be able to see that the quote got in
    
ns_returnredirect "quotations.tcl"











