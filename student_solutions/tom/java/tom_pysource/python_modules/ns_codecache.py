# $Id: ns_codecache.py,v 1.2 2000/07/29 23:12:30 mhagger Exp $

###
### See included file COPYRIGHT for copyright and license information.
###

"""Implements a version of execfile() that caches the code in memory."""


import sys, os, stat, threading, string

import Ns

Ns.Log(Ns.Debug, 'importing ns_codecache.py')

#
# a simple code caching mechanism.
#

class CodeCache:

    # __init__ primarily acquires the code cache dictionary and
    # increments a counter so that it doesn't get deleted. __del__
    # decrements the counter.
    
    def __init__(self):
        Ns.Log(Ns.Debug, 'Creating CodeCache object')
        # Move lack of thread safety into creation of this counter + lock
        # (creation will later be moved into single-thread C function)

        # create counter lock if it doesn't exist. (@CTB --> initialize code)
        if not __persistdict__.has_key('_codeCacheCounter'):
            __persistdict__['_codeCacheCounterLock'] = threading.Lock()
            __persistdict__['_codeCacheCounter'] = 0

        # increment counter so no-one else flushes/cleans cache.
        self._incrementCounter()

        # create read/update lock if it doesn't exist:
        if not __persistdict__.has_key('_codeCacheLock'):
            __persistdict__['_codeCacheLock'] = threading.Lock()

        # now, get the dictionary.
        
        # Storing the dictionary turns out to be much easier than storing
        # an instance of the CodeCache class...
        if __persistdict__.has_key('_code'):
            # @@MRH: Potential problem here if cache is deleted by
            # another thread
            self._code = __persistdict__['_code']
        else:
            # @@MRH: Potential problem here if cache is initialized by
            # another thread
            self._code = {}
            __persistdict__['_code'] = self._code

    def __del__(self):
        # decrement the code cache counter
        Ns.Log(Ns.Debug, 'Decrementing CodeCache counter')
        self._decrementCounter()

    # save (mtime, code) into cache keyed by 'filename'
    def save(self, filename, code):
        Ns.Log(Ns.Debug, 'saving "%s" into CodeCache' % filename)
        try:
            mtime = os.stat(filename)[stat.ST_MTIME]
        except:
            return

        # atomic -- no locking necessary
        self._code[filename] = (mtime, code)

    # retrieve code (if up-to-date with filename) from cache
    def retrieve(self, filename):
        
        # @@CTB    __persistdict__['_codeCacheLock'].acquire()
        
        if self._code.has_key(filename): # in cache
            # @@MRH: Potential problem here if cache or cache entry is
            # deleted by another thread

            # retrieve cached mtime & code
            (old_mtime, code) = self._code[filename]

            # get new mtime
            try:
                new_mtime = os.stat(filename)[stat.ST_MTIME]
            except:
                return None             # file doesn't exist, etc.

            # compare
            if new_mtime != old_mtime:
                # @@MRH: Potential problem here if cache or cache entry
                # is deleted by another thread:
                del(self._code[filename]) # clear entry
                return None

            Ns.Log(Ns.Debug, 'retrieving "%s" from CodeCache' % filename)

            return code
            
        else:
            return None

    def _incrementCounter(self):
        __persistdict__['_codeCacheCounterLock'].acquire()
        __persistdict__['_codeCacheCounter'] = \
                __persistdict__['_codeCacheCounter'] + 1
        __persistdict__['_codeCacheCounterLock'].release()

    def _decrementCounter(self):
        __persistdict__['_codeCacheCounterLock'].acquire()
        __persistdict__['_codeCacheCounter'] = \
                __persistdict__['_codeCacheCounter'] - 1
        __persistdict__['_codeCacheCounterLock'].release()


#
# a function to replace standard Python execfile with something that makes
# use of code caching.
#

def execfile(filename, globaldict=None, localdict=None):
    Ns.Log(Ns.Debug, 'in pywx.execfile')

    codeCache = CodeCache()             # @@CTB

    code = codeCache.retrieve(filename)

    if not code:                        # ...then we want to compile it.
        Ns.Log(Ns.Debug, 'not in cache or mtime changed - forcing reload')

        f = open(filename, 'r')
        c = []
        for l in f.readlines():     # probably a faster way to do this...
            c.append(string.rstrip(l) + '\n')

        f.close()

        c = string.join(c, '')
        code = compile(c, filename, 'exec')

        codeCache.save(filename, code)

    if globaldict is None:
        globaldict = globals()
        localdict = locals()
    elif localdict is None:
        localdict = globaldict

    exec(code, globaldict, localdict)


#
# initialize
#

def initializeCachingExecFile():
    Ns.Log(Ns.Debug, 'Running initializeCachingExecFile().')
    ### MRH: Not thread safe (?)
    try:                                # already initialized?
        tst = __persistdict__['cachingExecFile']
    except:
        __persistdict__['cachingExecFile'] = 1

        # save a copy of the original
        __persistdict__['oldExecFile'] = sys.modules['__builtin__'].execfile
        
    # now replace the 'execfile' function:
    sys.modules['__builtin__'].execfile = execfile

#
# uninitialize
#

def uninitializeCachingExecFile():
    Ns.Log(Ns.Debug, 'Running uninitializeCachingExecFile().')
    try:                                # may not have been replaced!
        sys.modules['__builtin__'].execfile = __persistdict__['oldExecFile']
    except:
        pass


