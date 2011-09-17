import Ns
import zz

################# /wap/index.py, by Tom Hickerson, 4/27/01
### an attempt to add WAP and WML capabilities to the SS site.
### first try will be to summon the five most recent postings and
### assemble a WAP deck that summons this information.
###
 

conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

teams_sql = '''
SELECT
	s.school_name,
	t.team_name,
	c.list_id
FROM teams t, content c, schools s
WHERE s.school_id = t.school_id
	and t.team_id = c.team_id
ORDER BY c.date_posted
LIMIT 5;'''


wml_form='''<?xml version="1.0"?> 
<!DOCTYPE wml PUBLIC "-//WAPFORUM//DTD WML 1.1//EN" "http://www.wapforum.org/DTD/wml_1.1.xml">
  
<wml>
<head>
<meta http-equiv="Cache-Control" content="max-age=0"/>
</head>

<template>
<do type="prev" label="Back">
<prev/>
</do>
<do type="options" label="Sec. Sources home">
<go href="index.py"/>
</do>
</template>

<card>
<p>Secondary Sources.com</p>
<p><anchor>Search Teams<go href="search-teams.py"/></anchor></p>
<p>Most recent submissions</p>
%s
</card>
</wml>'''

wml_line = '<p><anchor>%s %s<go href="one-post.py" method="post"><postfield name="list_id" value="%s"/></go></anchor><br/></p>'

wml_list = ''

try:
	results = db.Select(teams_sql)
	while db.GetRow(results) == Ns.OK:
		wml_list = wml_list + wml_line % (results[0],results[1],results[2])
except:
	wml_list = ''

wml = wml_form % (wml_list)

conn.ReturnData(200,wml,'text/vnd.wap.wml')
del db



