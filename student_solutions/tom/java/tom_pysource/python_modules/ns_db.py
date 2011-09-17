# $Id: ns_db.py,v 1.3 2000/07/06 19:30:52 mhagger Exp $

# how are errors returned from ns_db?
 
import string
import ns_python


class Error(Exception):
    """Exception class for ns_db module."""

    pass


class _NsSet(ns_python.TclInstance):
    """Represents an ns_set.

    The analogy to a python dictionary is not perfect.  For example,
    an ns_set can have multiple items with the same key, and the keys
    have an implied order.

    The referred-to ns_set is freed when the instance is deleted.

    """

    def __init__(self, interp, objid):
        ns_python.TclInstance.__init__(self, interp, 'ns_set', objid)

    def __del__(self):
        self.free()

    def __len__(self):
        return int(self('size'))

    def __getitem__(self, i):
        if type(i) is type(1):
            return self.value(i)
        else:
            return self.get(i)

    def __setitem__(self, i, value):
        if type(i) is type(1):
            i = self.key(i)
        self.update(i, value)

    def __delitem__(self, i):
        if type(i) is type(1):
            self.delete(i)
        else:
            self.delkey(i)

    def __nonzero__(self):
        """A set is `nonzero' if it is not empty."""
        return len(self) != 0

    def keys(self):
        """Return a list of the keys, in order by index number.

        Note that keys may appear multiple times."""

        retval = []
        for i in range(len(self)):
            retval.append(self.key(i))
        return retval

    def values(self):
        """Return a list of the values, in order by index number."""

        retval = []
        for i in range(len(self)):
            retval.append(self.value(i))
        return retval

    def items(self):
        """Return a list of (key,value) pairs, in order by index number."""

        retval = []
        for i in range(len(self)):
            retval.append( (self.key(i), self.value(i),) )
        return retval

    def dict(self):
        """Return the set as a python dictionary.

        Raise an exception if the set's keys are not unique.

        """

        retval = {}
        for (key,value) in self.items():
            if retval.has_key(key):
                raise Error('ns_set keys are not unique')
            retval[key] = value
        return retval


class _NsResultSet(_NsSet):
    """Represents a result row from a database query.

    Like a normal set, except it represents the results of a multi-row
    select.  That means the set can be scanned through the result rows
    one by one by using self.getrow().

    Once the last row has been read (or flush() has been called), the
    connection to the database is severed since calling getrow() again
    would result in an error.  The function is_valid() tests whether
    the connection is still intact.

    """

    def __init__(self, interp, objid, dbhandle):
        """Extends _NsSet.__init__ to hold a dbhandle."""

        _NsSet.__init__(self, interp, objid)
        self._dbhandle = dbhandle

    def is_valid(self):
        """Is the connection to the database is still intact?"""
        return hasattr(self, _dbhandle)

    def getrow(self):
        retval = int(self._dbhandle.getrow(self._objid))
        if retval == 0:
            # There are no more rows available; calling getrow again
            # would result in an error.
            del self._dbhandle
        return retval

    def flush(self):
        self._dbhandle.flush()
        del self._dbhandle


class _NsDBHandle(ns_python.TclInstance):
    """Represents a dbhandle and implements the method-like ns_db functions.

    Some of the following functions are handled through the
    __getattr__ method of the TclInstance class.  In any case, this
    class automatically handles the following functions and does what
    you would expect from the tcl ns_db documentation:

        self.bindrow() -- return a set to hold results from an exec.

        self.bouncepool() -- mark database handles as stale (cause
            connection to be reset when it is put back into the pool).

        self.cancel() -- cancel current operation.

        self.close() -- close connection.  Only use if connection was
            open using open() function.

        self.connected() -- is there a connection to the database pool?

        self.datasource() -- return data source for the database pool.

        self.dbtype() -- return the db type of the database pool.

        self.dml(sql) -- execute a data manipulation language
            command.
        
        self.driver() -- return the name of the handle's driver.

        self.exception() -- return the most recent exception for the
            pool.

        self.exec_(sql) -- execute an SQL command.  Renamed because
            'exec' is a python keyword.

        self.flush() -- flush results of last select.

        self.poolname() -- return the db pool from which this handle
            came.

        self.password() -- return the password of the user for the
            database pool.

        self.releasehandle() -- release the db handle back to the pool
            (invalidates the class instance).

        self.only1row(sql) -- return exactly one row as a set.
            (Renamed to satisfy python rules.)

        self.only0or1row(sql) -- return one row if available,
            otherwise None.  (Renamed to satisfy python rules.)

        self.select(sql) -- return a ResultSet from the sql query.

        self.user() -- return the user for the database pool.

        self.verbose(bool) -- change the verbose setting for the
            database pool.

    Functions implemented some other way:

        gethandle -- use global gethandle() function.

        getrow -- implemented within _NsResultSet.

        open -- use global open() function.

        pools -- use global pools() function.

    Unsupported functions:

        setexception
        sp_exec
        sp_getparams
        sp_returncode
        sp_setparam
        sp_start

    """

    def __init__(self, objid, interp=ns_python.ns_tcl, closable=0):
        ns_python.TclInstance.__init__(self, interp, 'ns_db', objid)
        self._closable = closable

    def is_valid(self):
        return hasattr(self, '_objid')

    def __del__(self):
        if self.is_valid():
            if self._closable:
                self.close()
            else:
                self.releasehandle()

    def releasehandle(self):
        """Drop lock on this pool, release handle.

        Should be called whenever we finish a db operation.  This
        method makes the instance invalid for future use.

        """

        if self._closable:
            raise Error('Cannot run releasehandle on handles '
                        'obtained using open')
        else:
            self('releasehandle')
            # Force an error if this instance is used again:
            del self._objid

    def __repr__(self):
        if self.is_valid():
            return '<class _NsDBHandle {poolname:%s, objid:%s}>' \
                   % (self.poolname(), self._objid,)
        else:
            return '<class _NsDBHandle (invalid)>'

    def select(self, query):
        if not self.is_valid():
            raise Error('handle is no longer valid!')

        # do the select.  Let exceptions fall thru.
        return _NsResultSet(self._interp, self('select', query), self)

    def bindrow(self):
        return _NsResultSet(self._interp, self('bindrow'), self)

    # guaranteed to be a select that returns at most 1 row
    def only1row(self, query):
        return _NsSet(self._interp, self('1row', query))

    def only0or1row(self, query):
        setid = self('0or1row', query)
        if setid:
            return _NsSet(self._interp, setid)
        else:
            return None

    def exec_(self, query):
        return self('exec', query)

    def close(self):
        if self._closable:
            self('close')
            del self._objid
        else:
            raise Error('Cannot close dbhandle unless '
                        'it was obtained using open')


def gethandles(poolname=None, timeout=None, nhandles=None):
    """Get database handlers from the specified pool.

    Return as a list.

    """

    args = []
    if timeout is not None:
        args.append('-timeout')
        args.append(timeout)
    if poolname is None:
        if nhandles is not None:
            # @@MRH: We could probably determine the default pool and
            # insert it here rather than barfing.
            raise Error('nhandles cannot be specified without poolname')
    else:
        args.append(poolname)
        if nhandles is not None:
            args.append(nhandles)
    handles = apply(ns_python.ns_tcl.ns_db.gethandle, args)
    return map(_NsDBHandle, tuple(string.split(handles)))


def gethandle(poolname=None, timeout=None):
    """Get a single database handler from the specified pool."""

    return gethandles(poolname, timeout)[0]


def pools():
    """Return a list of all database pools.

    This routine will fail if any pool names contain funky characters.

    """

    p = ns_python.ns_tcl.ns_db.pools()
    return string.split(p)


def open(driver, datasource, user, password):
    """Return a handle at a lower level, circumventing the pools."""

    objid = ns_python.ns_tcl.ns_db.open(driver, datasource, user, password)
    return _NsDBHandle(objid, closable=1)


