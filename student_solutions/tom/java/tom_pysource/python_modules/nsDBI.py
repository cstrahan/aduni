### $Header: /cvsroot/PyWX/PyWX/nsDBI.py,v 1.3 2000/11/05 23:47:26 mhagger Exp $
###
### See included file COPYRIGHT for copyright and license information.
###


class nsDBI:
    """ A DBI API v2.0 compliant interface to the generic ns_db routines. """

    def __init__(self):
        apilevel = 2.0                  # v2.0 compliant
        threadsafety = 0                # not thread-safe at all!
        paramstyle = pyformat           # what the heck
        
    def connect(self, poolname, connections):
        # implement
        pass

class nsDBIConnection:
    """ A Connection object for generic ns_db access to databases. """
    
    def close(self):
        # implement
        pass

    def commit(self):
        pass

    # rollback not implemented

    def cursor(self):
        # implement
        pass

class nsDBICursor:
    """ A Cursor object for generic ns_db access to databases. """

    def __init__(self):
        description = None              # None until an execute is called
        rowcount = -1                   # -1 until an execute is called
        arraysize = 1                   # default

    # callproc, nextset unimplemented

    def close(self):
        # implement
        pass

    def execute(self, operation, *args):
        # implement
        pass

    def executemany(self, operation, *args):
        for i in args:
            # flatten list
            arglist = []
            arglist.append(operation)
            
            for j in i:
                arglist.append(j)
                
            apply(self.execute, arglist)

    def fetchone(self):
        # implement
        pass

    def fetchmany(self, size=arraysize):
        array = []
        for i in range(0, size):
            n = self.fetchone()
            if n:
                array.append(n)
            else:
                break                   # break out of for loop

    def fetchall(self):
        array = []
        while 1:
            n = self.fetchone()
            if n:
                array.append(n)
            else:
                break                   # break out of while loop

    def setinputsizes(self, sizes):
        pass

    def setoutputsize(self, size):
        pass

# Error exceptions
class Error(StandardError):
    pass

class Warning(StandardError):
    pass

class InterfaceError(Error):
    pass

class DatabaseError(Error):
    pass

class InternalError(DatabaseError):
    pass

class OperationalError(DatabaseError):
    pass

class ProgrammingError(DatabaseError):
    pass
     
class IntegrityError(DatabaseError):
    pass

class DataError(DatabaseError):
    pass

class NotSupportedError(DatabaseError):
    pass    
