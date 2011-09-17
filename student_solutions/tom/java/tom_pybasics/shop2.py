import sys, os, string, cgi
import Ns
import htmllib, re

html_st = '''<html><head><title>Pset 1, Exercise 4</title></head>
<body bgcolor=white>'''

errmsg = '''<h3>Error</h3><i>%s</i>
<form><input type=button value=Back onclick="window.history.back()"></form>'''

resmsg = '<h3>Your Results for %s</h3><i>%s</i>'

html_end = '''<hr>
<address><a href="mailto:tom_hickerson@hotmail.com">
tom_hickerson@hotmail.com</a></address>
</body></html>'''

header = 'Content-type: text/html\n'

formmsg = '''<h3>%s</h3>
<form action=shop2.py>Enter ISBN here:
<input type=text name=isbn size=15>
<input type=submit value="Press here">
</form>'''

bnurl = 'http://shop.barnesandnoble.com/bookSearch/isbnInquiry.asp?userid=18SIDNMBI8&mscssid=60ARD0KLBFBR8GQL6NNWR1J6XWSU5MRF&isbn='

amurlst = 'http://www.amazon.com/exec/obidos/ASIN/'
amurlend = '/qid%3D987169895/002-9633788-6416819'

bookst ='http://www.1bookstreet.com:80/search_results5.asp?QSearch=Y&QuickSearchList=ISBN&QuickSearch='

def errorpage(err):
    print header
    print html_st
    print errmsg % (err)
    print html_end

def respage(isbn,res):
    print header
    print html_st
    print resmsg % (isbn,res)
    print formmsg % ('Try Another')
    print html_end

def process():   
    error = ''
    results = ''
    form=cgi.FieldStorage()
    conn = Ns.GetConn()    
    if not form.has_key('isbn'):
       print header
       print html_st
       print formmsg % ('ISBN Searcher')
       print html_end
    elif (len(form['isbn'].value) < 10) or (len(form['isbn'].value) > 10):
       error = error + 'Please enter a valid ISBN number<br>'
    else:
       isbn = form['isbn'].value
       barnes = Ns.FetchURL(bnurl + isbn)
       results = results + 'Searching a URL match in barnesandnoble.com<br>' 
       matchprice = re.compile(r'Our Price[^\$]+\$\d+\.\d\d',re.DOTALL).search(barnes)       
       if matchprice:
          results = results + matchprice.group() + '</font>'
       else:
          results = results + 'No matching ISBN exists'

       amazon = Ns.FetchURL(amurlst + isbn + amurlend)
       results = results + '<br>Searching a URL match in amazon.com<br>'
       matchprice2 = re.compile(r'Our Price[^\$]+\$\d+\.\d\d',re.DOTALL).search(amazon)
       if matchprice2:
          results = results + matchprice2.group() + '</font>'
       else:
          results = results + 'No matching ISBN exists'

       onebook = Ns.FetchURL(bookst + isbn)
       results = results + '<br>Searching a URL match in 1bookstreet.com<br>'
       matchprice3 = re.compile(r'Our Price[^\$]+\$\d+\.\d\d',re.DOTALL).search(onebook)
       if matchprice3:
          results = results + matchprice3.group() + '</font>'
       else:
          results = results + 'No matching ISBN exists'
    if error:
       errorpage(error)

    if results:
       respage(isbn,results)


if __name__ == '__main__':
   process()

#http://shop.barnesandnoble.com/bookSearch/isbnInquiry.asp?userid=18SIDNMBI8&mscssid=WVMW55P61EPB8L8AFNBDDSP0AVJT7PA0&isbn=9999999999

#http://www.amazon.com/exec/obidos/ASIN/9999999999/qid%3D987169895/002-9633788-6416819

#http://www.1bookstreet.com:80/search_results5.asp?QSearch=Y&QuickSearchList=ISBN&QuickSearch=


#   print '<html><body bgcolor=white>
#   print 'You did not specify an ISBN number.'
#   print 'Click on the "back" button and enter a number.'
#   print'<form><input type=button value=Back onclick="window.history.back()"></form>'
#   print '<hr></body></html>'
#   sys.exit()
#else:
#   isbn = string.atoi(form['isbn_num'].value)

