### $Header: /cvsroot/PyWX/PyWX/ns_python.py,v 1.24 2000/10/27 08:23:33 titus Exp $

import sys, string, cPickle, cStringIO
import Ns

def ns_write(s):
    Ns.GetConn().WriteConn(s)

def ns_eval_tcl(s):
    """Evaluate a tcl string in a Tcl interpreter that will remain
    constant for the duration of the connection."""

    return Ns.GetConn().TclEval(s)


def ns_tclvar(varname):
    # get the connection (might raise exception) and its associated
    # interpreter:
    interp = Ns.GetConn().GetConnInterp()

    # get the variable from within the interpreter
    value = Ns.Tcl_GetVar(interp, varname, 0)

    if value is None:
        Ns.Log(Ns.Debug, 'ns_tclvar: variable "%s" undef' % (varname,))
    else:
        Ns.Log(Ns.Debug, 'ns_tclvar: variable %s = "%s"' % (varname, value,))

    return value


def tcl_escape(s):
    """Convert s to string then escape special characters for use
    within double-quotes in Tcl."""

    s = str(s)
    s = string.replace(s, '\\', '\\\\')
    s = string.replace(s, '"',  '\\"')
    s = string.replace(s, '$',  '\\$')
    s = string.replace(s, '[', '\\[')
    s = string.replace(s, ']', '\\]')
    return s

# Create an ns_log function for convenience.  Allow the severity to be
# specified either as a constant (e.g., Ns.Error) or as a string
# (e.g., 'Error') so that the Tcl people will be happy.
_severity_translation = {}
for i in ['Notice', 'Warning', 'Error', 'Fatal', 'Bug', 'Debug']:
    v = getattr(Ns, i)
    _severity_translation[i] = v
    _severity_translation[v] = v
del i,v

def ns_log(log, str):
    Ns.Log(_severity_translation[log], str)


#########################
### class TclFunction ###
#########################
class TclFunction:
    """A callable object representing a Tcl function that may take arguments.

    Allows recursive dot-notation for subfunctions; e.g.,
    self.ns_sema.create(2) -> 'ns_sema create "2"'.

    """

    def __init__(self, interp, prefix):
        """Create a tcl command that begins with the given prefix.

        prefix is not quoted.

        """

        Ns.Log(Ns.Debug,
                      'creating tcl function with prefix "%s".' % prefix)
        self._interp = interp
        self._prefix = prefix

    def __repr__(self):
        return '<ns_python.TclFunction instance; prefix="%s">' % self._prefix

    def __getattr__(self, name):
        """Allow subfunctions (recursively).

        Note that name is not quoted.

        """

        return TclFunction(self._interp, self._prefix + ' ' + name)

    def __call__(self, *args):
        """Calls tcl function, passing arguments as quoted strings."""

        tcl_args = [self._prefix]
        for arg in args:
            try:
                tcl_args.append('"' + tcl_escape(arg) + '"')
            except:
                # hack in some bogus tolerance...
                pass
        return self._interp(string.join(tcl_args))


#########################
### class TclInstance ###
#########################
class TclInstance:
    """A callable object that represents a Tcl object instance.

    Of course tcl is not object oriented, but still it is often the
    case that a whole family of functions is invoked with the same
    pattern:

        % $tcltype $method $objid $args...

    For example:

        % ns_db select $dbhandle $query

    This is actually like a method function called on the object with
    the given objid, or in python notation:

        >>> obj.method(args...)

    This class allows this notation by representing obj in a way that
    allows ``bound methods'' to be created using dot-notation.  The
    above example could be programmed using:

        >>> obj = TclInstance('ns_db', dbhandle)
        >>> result = obj.select(query)

    """

    def __init__(self, interp, tcltype, objid):
        Ns.Log(Ns.Debug, 'creating tcl object "%s" of type "%s".'
               % (objid,tcltype,))
        self._interp = interp
        self._tcltype = tcltype
        self._objid = objid

    def __repr__(self):
        return '<ns_python.TclInstance instance; tcltype="%s", objid="%s">' \
               % (self._tcltype, self._objid)

    def __getattr__(self, method):
        """Return an object that can be used as a bound method."""

        return TclFunction(self._interp,
                           '%s %s %s' % (self._tcltype, method, self._objid,))

    def __call__(self, method, *args):
        """Calls method, passing arguments as quoted strings."""

        tcl_args = [self._tcltype, method, self._objid]
        for arg in args:
            try:
                tcl_args.append('"' + tcl_escape(arg) + '"')
            except:
                # hack in some bogus tolerance...
                pass
        return self._interp(string.join(tcl_args))


############################
### class TclInterpreter ###
############################
class TclInterpreter:
    """Represents a tcl interpreter.

    Instances of this class provide a python interface to the tcl
    interpreter.  Although all instances actually refer to a single
    interpreter, we pass the interpreter around as if it were
    meaningful.  This might be useful if someday multiple interpreters
    could exist simultaneously.

    Methods:

        self(cmd) -- pass an arbitrary command to the interpreter
            and return the result as a string.  String is passed
            literally; no quoting is performed.

        self.functionname -- return a callable object representing the
            tcl function.

        self.wrap_instance(tcltype, objid) -- wraps a tcl object
            instance in a TclInstance.  objid should be a string that
            does not need to be quoted.

        self[varname] = value -- set the value of a variable.

        self[varname] -- get the value of a variable.

        self.keys() -- get the names of all global variables.

    Note: we need to correctly raise exceptions upon failure of
    various accesses

    """

    def __call__(self, cmd):
        return ns_eval_tcl(cmd)

    def __repr__(self):
        return '<ns_python.TclInterpreter instance>'

    def __getattr__(self, name):
        """Return a callable object that calls the Tcl function 'name'.

        For example,

            f = tclinterpreter.ns_db
            dbhandle = f('gethandle')

        Or since the returned object also supports __getattr__, you
        can simply do

            dbhandle = tclinterpreter.ns_db.gethandle()

        It is not checked whether the function actually exists.

        """

        return TclFunction(self, name)

    def wrap_instance(self, tcltype, objid):
        return TclInstance(self, tcltype, objid)

    def __getitem__(self, varname):
        """Get a variable from the Tcl namespace."""

        return ns_tclvar(varname);

    def __setitem__(self, varname, value):
        """Set a variable in the Tcl namespace."""

        ns_eval_tcl('set %s "%s"' % (varname, tcl_escape(value)))

    def keys(self):
        """Get a list of the variables currently defined.

        This routine will fail if there are any variables with funky
        characters in their names.

        """

        return string.split(self('info globals'))


# now declare one ;)
ns_tcl = TclInterpreter()


##############################
### class NsInfoDictionary ###
##############################
class NsInfoDictionary:
    """A class to get information from the AOLserver."""

    def __init__(self):
        # Will raise exception if there is no current connection:
        self.conn = Ns.GetConn()

    # get information about a tag from the AOLserver
    def __call__(self, index):
        return getattr(self, index)()

    # Functions that are directly equivalent to Ns functions
    # taking no arguments are mapped to the equivalent function.
    # Functions that are not implemented are mapped to None.  Other
    # functions are defined explicitly below.  This map is used by
    # __getattr__.
    _fnmap = {
        'pageroot'      : Ns.PageRoot,
        'name'          : Ns.InfoServerName,
        'config'        : Ns.InfoConfigFile,
        'platform'      : Ns.InfoPlatform,
        'hostname'      : Ns.InfoHostname,
        'address'       : Ns.InfoAddress,
        'uptime'        : Ns.InfoUptime,
        'boottime'      : Ns.InfoBootTime,
        'version'       : Ns.InfoServerVersion,
        #'home'          : Ns.InfoHome,
        'tcllib'        : Ns.TclLibrary,
        'label'         : Ns.InfoLabel,
        'builddate'     : Ns.InfoBuildDate,
                        
        'callbacks'     : None,
        'sockcallbacks' : None,
        'scheduled'     : None,
        'locks'         : None,
        'threads'       : None,
        }

    def __repr__(self):
        return '<ns_python.NsInfoDictionary instance>'

    def __getattr__(self, a):
        try:
            fn = self._fnmap[a]
        except KeyError:
            raise AttributeError(a)
        if fn is None:
            raise NotImplementedError(a)
        else:
            return fn

    def server(self):
        return Ns.nsServer

    servers = server

    def log(self):
        l = Ns.InfoErrorLog()
        return l or "STDOUT"

    def python(self):
        return sys.version

    def winnt(self):
        return sys.platform == 'win'


def ns_info(index):
    return NsInfoDictionary()(index)


def UpdateNsSet_FromDict(set, dict):
    """Insert every item from a python dictionary into an Ns.Set."""

    for (k,v) in dict.items():
        set.Put(k,v)


def PyDict_From_NsSet(set):
    """Produce a Python dictionary with the same contents as an Ns.Set."""
    return set.dict()


#
# Good lord.  More functionality??
#
class ns_pickleJar:
   def __init__(self, db, tablename='pickleJar', indexname='i', objname='obj'):
       self.db = db
       self.d = {}
       self.d['tablename'] = tablename
       self.d['indexname'] = indexname
       self.d['objname'] = objname

   def pickle(self, i, o):
       db = self.db
       d = self.d.copy()
       d['i'] = i                       # don't want to modify self.d

       #
       # pickle the object into an SQL-quotable string
       #
       f = cStringIO.StringIO()
       cPickle.dump(o, f)
       d['obj'] = string.replace(f.getvalue(), "'", "''")

       #
       # dump it into the database
       #
       db.dml('BEGIN TRANSACTION')
       # delete old entry if it exists
       db.dml('DELETE FROM %(tablename)s WHERE %(indexname)s=%(i)d' % d)
       db.dml("INSERT INTO %(tablename)s VALUES (%(i)d, '%(obj)s')" % d)
       db.dml('END TRANSACTION')

   def unpickle(self, i):
       db = self.db
       d = self.d.copy()
       d['i'] = i

       if db.only1row(
           'SELECT %(objname)s FROM %(tablename)s '
           'WHERE %(indexname)s=%(i)d'
           % d):
           l = cStringIO.StringIO(db['obj'])
           return cPickle.load(l)
       else:
           return None


#############################
### class OutputToNsWrite ###
#############################
class OutputToNsWrite:
    """A stream class that writes to ns_write.

    To assign to stdout, to allow overriding of the output
    function."""

    def __init__(self):
        # get the hooks from the persistent dictionary, and define
        # function 'write' as, simply, ns_write
        self.write = ns_write

    # seems like this is needed:
    def writelines(self, lines):
        for line in lines:
            self.write(line)

    # should probably overload other methods from stdout as well...
    def flush(self):
        pass


#
# Redirect stdout to ns_write:
#

def ns_redirect():
    Ns.Log(Ns.Debug, "Redirecting stdout to ns_write.")

    sys.stdout = OutputToNsWrite()
