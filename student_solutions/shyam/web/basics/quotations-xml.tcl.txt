# PS-1 Exercise 10
# Written by Shyam Visweswaran at AdUni

set xml_content "<quotations>"

# query the quotations table and generate XML formatting
# for each row

db_foreach get_quotes "select * from quotations" {
    append xml_content "
    <onequote>
        <quotation_id>$quotation_id</quotation_id>
        <insertion_date>$insertion_date</insertion_date>
        <author_name>$author_name</author_name>
        <category>$category</category>
        <quote>$quote</quote>
    </onequote>"
} if_no_rows { }

append xml_content "\n</quotations>"

# return string with status code of 200 (normal) and a MIME
# type of "application/xml"

ns_return 200 application/xml $xml_content
