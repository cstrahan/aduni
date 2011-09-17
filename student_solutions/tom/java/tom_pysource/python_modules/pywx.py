# $Id: pywx.py,v 1.26 2001/03/01 16:55:53 mhagger Exp $

###
### See included file COPYRIGHT for copyright and license information.
###

# Should we use a custom version of execfile() that caches the
# compiled code in memory?  Warning: This may not be thread safe!!!
UseCachingExec = 0

import sys, traceback, os, string, stat
import Ns, ns_setup

Ns.Log(Ns.Debug, '**** importing pywx.py')


if UseCachingExec:
    Ns.Log(Ns.Debug, 'Using the caching version of execfile.')
    import ns_codecache
    execfile = ns_codecache.execfile
else:
    Ns.Log(Ns.Debug, 'Using the standard (non-caching) version of execfile.')


# This default can be overridden in the AOLserver configuration file;
# see INSTALL:
default_exception_html = """\
<html>
<title>Internal Server Error</title>
<body bgcolor=white>
<h2>Internal Server Error</h2>
There was an error processing your request.  Our web administrators
have been notified.  Sorry for the inconvenience.
</body>
</html>
"""

# This default can be overridden in the AOLserver configuration file;
# see INSTALL:
default_exception_log = """\
Python exception: Unhandled exception running %(url)s.  Traceback:
%(exc_text)s"""


class ScriptError(Exception):
    """An error that gets special handling by PyWX.

    Any exceptions derived from this class get special handling by
    PyWX.  If such an exception is caught by PyWX (implying that the
    application script didn't handle it internally), then PyWX calls
    the exception's __call__ method with the arguments (conn, url).
    Derived classes should usually override this method do do whatever
    custom actions they want--for example, redirecting the client to
    an error page, printing a customized error page, etc.

    In some cases it might be enough for a derived class to override
    just the format strings.

    """

    # These format strings are taken from the AOLserver configuration
    # file if available; otherwise the defaults defined above are
    # used.

    # format string for the output to the client:
    exception_html = (
        Ns.ConfigGetValue('ns/environment/python', 'ExceptionHTMLFmt')
        or default_exception_html
        )

    # format string for output to the nsd log:
    exception_log = (
        Ns.ConfigGetValue('ns/environment/python', 'ExceptionLogFmt')
        or default_exception_log
        )

    def __call__(self, conn, url):
        fmt_dict = {
            'url' : url,
            # Don't store sys.exc_info() to local variables because it
            # causes a reference loop:
            'exc_text' : string.join(
                apply(traceback.format_exception, sys.exc_info()),
                '',
                ),
            }
        conn.ReturnHtml(500, self.exception_html % fmt_dict)
        Ns.Log(Ns.Error, self.exception_log % fmt_dict)


def runscript(conn):
    """Run a python script."""

    url = conn.request.url

    # see if file exists:
    script = Ns.UrlToFile(conn.Server(), url)
    try:
        os.stat(script)
    except OSError:
        conn.ReturnNotFound()
    else:
        # __persistdict__ is in __builtins__ and therefore accessable.
        # Fool script into thinking that it is running as __main__:
        execfile(script, {'__name__' : '__main__'})


def cgi_runscript(conn):
    """Run a python script in a CGI environment.

    This function does not attach stdio; that has to be done at the
    C++ level (i.e., via the AttachStdio config directive).

    """

    url = conn.request.url
    pageroot = Ns.PageRoot(conn.Server())

    #  Bah!  It's not that easy.  To wit, from the
    #  AOLserver nscgi sources we learn:
    #
    #  1. Path is UrlToFile up to the URL prefix.
    #  2. SCRIPTNAME is the URL prefix
    #  3. PATH_INFO is everything past SCRIPT_NAME
    ns_setup.setup_cgi_env(url)
    Ns.Log(Ns.Debug, '++++ start python: ' + os.environ['SCRIPT_NAME'])
    try:
        # see if file exists:
        os.stat(pageroot + os.environ['SCRIPT_NAME'])
    except OSError:
        conn.ReturnNotFound()
    else:
        # Error handling is done in the function who called us.
        execfile(pageroot + os.environ['SCRIPT_NAME'])

    Ns.Log(Ns.Debug, '++++ done python: ' + os.environ['SCRIPT_NAME'])


def fulfill_request():
    conn = Ns.GetConn()
    url = conn.request.url
    Ns.Log(Ns.Debug, '++++ start python: ' + url)

    # Execute request, catching any exceptions:
    try:
        try:
            runscript(conn)
        except SystemExit:
            # This indicates a normal exit() call--needn't print exception.
            pass
        except ScriptError, e:
            e(conn, url)
    except:
        # This outside try block catches unrecognized exceptions,
        # including any exceptions raised within the user's custom
        # error handler.  Use ScriptError to log:
        ScriptError()(conn, url)

    Ns.Log(Ns.Debug, '++++ done python: ' + url)


def run_map(setup_strings, callable):
    conn = Ns.GetConn()
    url = conn.request.url
    Ns.Log(Ns.Debug, '++++ start python: ' + url)

    # Execute request, catching any exceptions:
    try:
        try:
            d = {} # Dictionary to use
            for s in setup_strings:
                Ns.Log(Ns.Debug, 'Executing setup string "%s"' % s)
                exec s in d
            eval(callable, d)(conn)
        except SystemExit:
            # This indicates a normal exit() call--needn't print exception.
            pass
        except ScriptError, e:
            e(conn, url)
    except:
        # This outside try block catches unrecognized exceptions,
        # including any exceptions raised within the user's custom
        # error handler.  Use ScriptError to log:
        ScriptError()(conn, url)

    Ns.Log(Ns.Debug, '++++ done python: ' + url)


# Right now there is no difference between these two functions:
thread_run_map = run_map


# This routine is currently non-functional.  It should probably be
# rewritten to be a substitute for runscript(). ###
def fulfill_cgi_request():
    conn = Ns.GetConn()
    url = conn.request.url
    pageroot = Ns.PageRoot(conn.Server())

    #
    #  Bah!  It's not that easy.  To wit, from the
    #  AOLserver nscgi sources we learn:
    #
    #  1. Path is UrlToFile up to the URL prefix.
    #  2. SCRIPTNAME is the URL prefix
    #  3. PATH_INFO is everything past SCRIPT_NAME
    ns_setup.setup_cgi_env(url)
    Ns.Log(Ns.Debug, '++++ start python: ' + os.environ['SCRIPT_NAME'])
    try:
        # see if file exists:
        os.stat(pageroot + os.environ['SCRIPT_NAME'])
    except OSError:
        conn.ReturnNotFound()
    else:
        # let python do its own error handling
        prepare_and_runscript(conn, pageroot + os.environ['SCRIPT_NAME'], url)

    Ns.Log(Ns.Debug, '++++ done python: ' + os.environ['SCRIPT_NAME'])


