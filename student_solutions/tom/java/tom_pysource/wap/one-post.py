import Ns
import zz


conn = Ns.GetConn()
db = Ns.DbHandle(Ns.DbPoolDefault(conn.Server()))

post_sql = '''
SELECT
	c.title,
	p.first_names,
	p.last_name,
	p.email,
	c.message,
	s.school_name,
	t.team_name,
	c.mod_total
FROM
	content c, teams t, persons p, schools s
WHERE
	c.list_id = %s
	and c.team_id = t.team_id
	and c.person_id = p.person_id
	and t.school_id = s.school_id;'''

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
<p>Most recent submission: %s %s</p>
<p>%s</p>
<p>By %s %s (%s)</p>
<p>%s</p>
<p>Mod total: %s</p>
</card>
</wml>'''

wml = ''

try:
	query = conn.GetQuery()
except:
	wml = ''

post_sql2 = post_sql % (query.Get('list_id'))

try:
	results = db.Select(post_sql2)
	while db.GetRow(results) == Ns.OK:
		wml = wml_form % (results[5],results[6],results[0],results[1],results[2],results[3],results[4],results[7])
except:
	wml = '<card><p>No results found</p></card>'

conn.ReturnData(200,wml,'text/vnd.wap.wml')
del db




