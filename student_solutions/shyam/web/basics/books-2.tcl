# PS-1 Exercise 4: books-2.tcl - target for books.html
# Written by Shyam Visweswaran at ADUni

# get hold of the isbn from books.html

set_the_usual_form_variables

# input error checking - check that input string exists
# and is not empty

if { ![info exists isbn] || [string compare $isbn ""] == 0 } {
    ns_return 200 text/plain "Please go back and enter an ISBN number"
    # stop execution of script
    return
}

# more error checking - check if input string is less than 10 digits
# the other case of more than 10 digits is taken care of by the fact
# that the input box is 10 spaces long

if { [string length $isbn] < 10 } {
    ns_return 200 text/plain "Please go back and enter an ISBN number of 10 digits"
    # stop execution of script
    return
}

# Given url, price_pattern, stock_pattern, store_name, grabs the html
# from the url and returns a table row with store name, price and availablity

proc book_info {url price_pattern stock_pattern store_name} {
    set price "Information not available"
    set stock "Information not available"

    catch {ns_httpget $url} html
    regexp $price_pattern $html match price

    if [regexp $stock_pattern $html match] {
	set stock "Available"
    } else {
	set stock "Unavailable"
    }
    return [append "" "<tr> <td><a href=\"$url\">$store_name</a></td> <td>$price</td> <td>$stock</td> </tr>"]
}

# query Barnes and Noble
set bandn_url "http://shop.barnesandnoble.com/bookSearch/isbnInquiry.asp?isbn=$isbn"
set bandn_price "Our Price:.*?(\\$\[0-9\]*\\.\[0-9\]\[0-9\])"
set bandn_stock "In-Stock"
set bandn_info [book_info $bandn_url $bandn_price $bandn_stock "Barnes and Noble"]

# query 1bookstore
set bookstreet_url "http://www.1bookstreet.com:80/product.asp?sku=$isbn"
set bookstreet_price "Our Price:.*?(\\$\[0-9\]*\\.\[0-9\]\[0-9\])"
set bookstreet_stock "leaves our warehouse"
set bookstreet_info [book_info $bookstreet_url $bookstreet_price $bookstreet_stock "1bookstreet"]

# return the results page
ns_return 200 text/html "<html>
<head><title>Exercise 4</title></head>

<body bgcolor=#ffffff>

<blockquote>
<font face=helvetica,arial>

<table width=90%>
<tr><td> <h2>Here are the results of your request for ISBN $isbn</h2> </td>
<td align=right> <a href=../>Home</a> </td></tr>
</td></tr></table>

<hr>

<table>
<tr> <th>Bookstore</th> <th>Price</th> <th>Availabililty</th> </tr>
$bandn_info
$bookstreet_info
</table>

<hr>
<address><a href=\"mailto:shyam@massmed.org\">shyam@massmed.org</a></address>
</blockquote>

</body>
</html>"


