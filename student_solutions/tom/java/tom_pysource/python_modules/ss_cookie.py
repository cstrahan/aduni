import Ns
import time

def set_cookie(value):
    conn = Ns.GetConn()
    cookie_time_format = '%A, %d-%b-%Y %H:%M:%S GMT'
    ### this cookie has an expiration date of one year after the set time
    exp_time = time.strftime(cookie_time_format,time.gmtime(time.time() + 31536000))
    out_header = conn.OutputHeaders()
    out_header.Put('Set-Cookie','password=%s;expires=%s;path=/' % (value,exp_time))

def cancel_cookie():
    conn = Ns.GetConn()
    cookie_time_format = '%A, %d-%b-%Y %H:%M:%S GMT'
    ### this cookie has an expiration date of ten seconds prior to the set time
    exp_time = time.strftime(cookie_time_format,time.gmtime(time.time() - 10))
    out_header = conn.OutputHeaders()
    out_header.Put('Set-Cookie','password=;expires=%s;path=/' % (exp_time))

def get_cookie():
    conn = Ns.GetConn()
    header = conn.Headers()
    cookie = header.IGet('Cookie')
    return cookie

def get_cookie_password():
    conn = Ns.GetConn()
    header = conn.Headers()
    cookie = header.IGet('Cookie')
    split_cookie = cookie.split('=')
    password = split_cookie[1]
    return password
