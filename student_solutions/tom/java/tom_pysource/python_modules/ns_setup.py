# $Id: ns_setup.py,v 1.21 2001/01/28 17:19:15 mhagger Exp $

import sys, os, string, types
import Ns

PyWX_Version = '0.6' # also set in ns_pywx.cc

# first things first... Break link and clear environment (except PATH):
orig_environ = os.environ
os.environ = {'PATH':os.environ['PATH']}


def setup_env_strings():
    conn = Ns.GetConn()

    # os.environ is set (on first import) to be a new dictionary, which
    # breaks the connection between os.environ and the UNIX environment
    #
    # Here, we empty it of all variables but PATH and refill it; this
    # prevents a situation where interpreter reuse could lead to a
    # process getting an environment with leftover elements in it.
    #
    # Note that we don't want to reassign os.environ to a new dictionary,
    # because then modules that are using os.environ already will not
    # get the modifications; we want to modify in place.

    env = os.environ                    # nickname
    env.clear()
    # I don't understand the reason for this change (MRH): ###
    env['PATH'] = Ns.PageRoot(conn.Server()) 
    #env['PATH'] = orig_environ['PATH']  # keep PATH

    # The default search path now is whatever AOLserver set it to.
    Ns.Log(Ns.Debug,
           'PyWX: os.environ[\'PATH\'] unchanged (%s)' % (env['PATH'],))

    # Set some variables in the namespace for potential script use
    env['PYWX_VERSION'] = PyWX_Version
    env['DOCUMENT_ROOT'] = Ns.PageRoot(conn.Server())

    # Include info about our PyWX module
    env['SERVER_SOFTWARE'] = '%s/%s-PyWX/%s' % (
        Ns.InfoServerName(),
        Ns.InfoServerVersion(),
        PyWX_Version,
        )

    # Server Build date
    env['SERVER_BUILDDATE'] = Ns.InfoBuildDate()

    # Script Name:
    try:
        path_trans = Ns.UrlToFile(conn.Server(), conn.request.url)
        env['PATH_TRANSLATED'] = path_trans
        offset = string.find(path_trans, ".py")+3
        remove = Ns.PageRoot(conn.Server())
        env['SCRIPT_NAME'] = string.replace(path_trans[:offset],remove,'')
        env['PATH_INFO'] = (env['PATH_TRANSLATED'])[offset:]
        Ns.Log(Ns.Debug, "Path Info = " + env['PATH_INFO'])
    except RuntimeError:
        env['SCRIPT_NAME'] = ''
        env['SCRIPT'] = ''
        env['PATH_INFO'] = ''
        env['PATH_TRANSLATED'] = ''

    # Set the HTTP_* header environment variables:
    headers = conn.Headers()
    for i in range(headers.Size()):
        key = string.replace(headers.Key(i), '-', '_')
        key = 'HTTP_' + string.upper(key)
        env[key] = headers.Value(i)

    # Connection Protocol:
    env['SERVER_PROTOCOL'] = 'HTTP/%4.2f' % (conn.request.version,)
    env['AUTH_TYPE'] = 'Basic'
    env['GATEWAY_INTERFACE'] = 'CGI/1.1'

    # Content Length/Content Type:
    content = headers.IGet('Content-type')
    if content is None:
        if conn.request.method == 'POST':
            content = 'application/x-www-form-urlencoded'
        else:
            content = ''
    env['CONTENT_TYPE'] = content

    if conn.ContentLength() <= 0:
        env['CONTENT_LENGTH'] = ''
    else:
        env['CONTENT_LENGTH'] = '%u' % (conn.ContentLength(),)

    env['REQUEST_METHOD'] = conn.request.method
    env['QUERY_STRING'] = conn.request.query or ''

    # Remote Address:
    connData = conn.Peer()
    if connData is not None:
        env['REMOTE_ADDR'] = connData
        env['REMOTE_HOST'] = Ns.GetHostByAddr(connData) or connData

    env['SERVER_NAME'] = conn.Host()

    # Server port (from AOLserver nscgi module logic)
    connData = conn.Location()
    port = None
    if connData is not None:
        try:
            port = Ns.ParseUrl(connData)[2]
        except RuntimeError:
            pass
    # If no port found, use default of '80':
    env['SERVER_PORT'] = port or '80'

    # Remote User
    if conn.authUser is not None:
        env['REMOTE_USER'] = conn.authUser

    # Unimplemented values:
    env['PATH_INFO'] = ''
    env['PATH_TRANSLATED'] = ''


#
#
# N.B.
#
# It seems that multiple threads share sys.argv but multiple
# interpreters do not, so there is no problem with sys.argv
# being set.
#

def setup_cmd_ln_args(filename):
    """Break up the query string and use it to set sys.argv."""

    import sys

    sys.argv = [filename]

    arg = Ns.GetConn().request.query

    Ns.Log(Ns.Debug, "PyWX: request.query is %s (%s)"
           % (arg, Ns.GetConn().request.method))

    # If there are no parameters in the query, and this is not a FORM
    # submission, do not set any further arguments.
    if not arg:
        return

    sys.argv.append(arg)

    Ns.Log(Ns.Debug, "PyWX: CGI argument is %s" % (arg))

def setup_cgi_env(filename):
    setup_env_strings()
    setup_cmd_ln_args(filename)


