########################################
###zz.py
###Zvi wrote most of this, Bryon Gill 4/24/01 updated it with some authentication and cookie management code. 

import Ns

###############################
# COOKIES

def set_cookie(key, value, persistent_p = 1):
    page_headers = Ns.GetConn().OutputHeaders()
    persist_text = ''
    if (persistent_p == 1):
        persist_text = "expires=Fri, 01-Jan-2010 01:00:00 GMT"
    if (value == None):
        persist_text = "expires=Fri, 01-Jan-2000 01:00:00 GMT"
    cookie_text = """%s=%s; path=/; %s""" % (str(key),str(value),persist_text)
    page_headers["Set-Cookie"] = cookie_text

def get_cookie(key):
    try:
        cookie = Ns.GetConn().Headers()["Cookie"]
    except:
        cookie = ""
    list_of_cookies = cookie.split(";")
    for cookie_pair in list_of_cookies:
        cookie_pair = cookie_pair.strip()
        if (cookie_pair.find("%s=" % (key)) == 0):
            return cookie_pair.replace("%s=" % (key),"",1)
    return None

###############################
# FORM VARS

def error_form_vars(var_specs):
    html = ""
    for spec in var_specs:
        html = html + "<li> Variable <b>%s</b> is required.</li>" % (spec[0])
    return  """<html><head><title>Mistakes in your input</title></head>
    <body>
    <h3>Mistakes in your input</h3>
    
    <ul>
    %s
    </ul>
    
    </body></html>""" % (html)

def get_form_variables ( required = 1 ):
    conn = Ns.GetConn()
    try:
        query = conn.GetQuery()
    except (RuntimeError):
        if required:
            conn.ReturnBadRequest("You are Missing Form Data")
        else:
            query = {}
    return query

def get_vars( var_dict ):
    # Example usage of get_vars:
    # --------------------------
    # query = zz.get_vars ( { 'map_spec_string' : '' ,
    #                        'last_p' : 'off',
    #                        'image_url' : '''http://cnn.com/bla.gif'''
    #  } )
    query = get_form_variables(0)
    error_specs = []
    for key,value in var_dict.items():
        if (value == None) and (not query.has_key(key)):
            error_specs.append([key,value])
        else:
            if not query.has_key(key):
                query[key] = value
    if len(error_specs) > 0:
        Ns.GetConn().ReturnHtml(500, error_form_vars(error_specs))
    return query

##############################################
#MANAGE LOGIN COOKIES/FORM DATA
def valid_user_p(db_handle):
    conn = Ns.GetConn
    my_form = get_vars({'password' : '',
                           'logout'   : '',
                        'email' : ''})
    if my_form['logout'] == "Log me out":
        set_cookie('valid_code',None) #delete the cookie
        return 0
    if get_cookie('valid_code'):
        return 1
    Ns.Log(Ns.Notice,"the value of the email is %s" % my_form['email'])
    if validate_login(db_handle, my_form['email'],my_form['password']):
        set_cookie('valid_code',validate_login(db_handle,my_form['email'],my_form['password']))
        return 1
    else:
        return 0

##############################################
#VALIDATE USER PASSWORDS AND EMAIL ADDRESSES
validate_query = """
SELECT
    schools.school_id
FROM
    schools,
    persons,
    person_school_map
WHERE
---test: email is registered and affiliated with the password's school
    persons.email = '%s'
    AND schools.school_password = '%s'
    AND person_school_map.person_id = persons.person_id
    AND person_school_map.school_id = schools.school_id
"""



def validate_login(my_db_handle, email, password):
    conn = Ns.GetConn()
    my_results = my_db_handle.Select(validate_query % (email, password))
    if (my_db_handle.GetRow(my_results) == Ns.OK):
        return my_results['school_id']
    else:
        return None
        
    ###query the db for at least one match of the email and school password.
    ###get the school_id associated with the email.
    ###ensure the password is correct for the school.
    ###return the school id if it's correct
    ###send back "none" if it's wrong
    ###cookieing school_id simplifies the email dropdown generation process


############################################################
# UNIX SPECIFIC:
# takes a command that can receive standard input and returns
# standard output, and an input, and returns  the value of
# the entire standard output.  This should only be used with
# commands that terminate quickly; i.e. stdout.read() should
# not take a long time.
def unix_filter( input , command ):
    from popen2 import popen2
    stdin,stdout = popen2(command)
    stdin.write(input)
    stdin.close()
    output = stdout.read()
    stdout.close()
    return output

















