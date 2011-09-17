import Ns, string

conn = Ns.GetConn()

try:
	agent = conn.Headers()["Accept"]
	
except:
	agent = ''

Ns.Log(Ns.Notice,'Agent detected: '+agent)
if (string.find(agent,'text/vnd.wap.wml') > 0):
	conn.ReturnRedirect('http://10.11.0.129/project/wap/index.py')
else:
	conn.ReturnRedirect('http://10.11.0.129/project/front.py')