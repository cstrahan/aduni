import Ns

conn = Ns.GetConn()

wml_form='''<?xml version="1.0"?> 
<!DOCTYPE wml PUBLIC "-//WAPFORUM//DTD WML 1.1//EN" "http://www.wapforum.org/DTD/wml_1.1.xml">
  
<wml>
<head>
<meta http-equiv="Cache-Control" content="max-age=0"/>
</head>

<card>
<p>Secondary Sources.com</p>
<p>Most recent submissions</p>
<p>Log in</p>
<p><anchor>Rutgers University BU<go href="sswml.py"/></anchor><br/></p>
<p><anchor>Texas University RF<go href="sswml.py"/></anchor><br/></p>
<p><anchor>Wake Forest YY<go href="sswml.py"/></anchor><br/></p>
<p><anchor>Gonzaga University TE<go href="sswml.py"/></anchor><br/></p>
<p><anchor>Boston College TX<go href="sswml.py"/></anchor><br/></p>
</card>
</wml>'''

conn.ReturnData(200,wml_form,'text/vnd.wap.wml')