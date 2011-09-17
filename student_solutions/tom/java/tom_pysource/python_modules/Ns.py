# This file was created automatically by SWIG.
import Nsc

import types,string


def DbPoolGetMultipleHandles(pool, nwant):
    retval = map(DbHandlePtr,
                 Ns._DbPoolGetMultipleHandles_Py(pool, nwant))
    for i in retval:
        i.thisown = 1
    return retval


def DbPoolTimedGetMultipleHandles(pool, nwant, wait):
    retval = map(
        DbHandlePtr,
        Ns._DbPoolTimedGetMultipleHandles_Py(pool, nwant, wait)
        )
    for i in retval:
        i.thisown = 1
    return retval


def ConfigGetPath(server=None, module=None, *path):
    retval = ['ns']
    if server is not None:
        retval.extend(['server', server])
    if module is not None:
        retval.extend(['module', module])
    retval.extend(list(path))
    return string.join(retval, '/')


def ConfigGetSections():
    retval = Nsc._ConfigGetSections_Py()
    for i in range(len(retval)):
        retval[i] = SetPtr(retval[i])
    return retval

class Set:
    def __init__(self,*args):
        self.this = apply(Nsc.new_Set,args)
        self.thisown = 1

    def __del__(self,Nsc=Nsc):
        if self.thisown == 1 :
            Nsc.delete_Set(self)
    def Update(*args):
        val = apply(Nsc.Set_Update,args)
        return val
    def Put(*args):
        val = apply(Nsc.Set_Put,args)
        return val
    def Unique(*args):
        val = apply(Nsc.Set_Unique,args)
        return val
    def IUnique(*args):
        val = apply(Nsc.Set_IUnique,args)
        return val
    def Find(*args):
        val = apply(Nsc.Set_Find,args)
        return val
    def IFind(*args):
        val = apply(Nsc.Set_IFind,args)
        return val
    def Get(*args):
        val = apply(Nsc.Set_Get,args)
        return val
    def IGet(*args):
        val = apply(Nsc.Set_IGet,args)
        return val
    def Trunc(*args):
        val = apply(Nsc.Set_Trunc,args)
        return val
    def Delete(*args):
        val = apply(Nsc.Set_Delete,args)
        return val
    def PutValue(*args):
        val = apply(Nsc.Set_PutValue,args)
        return val
    def DeleteKey(*args):
        val = apply(Nsc.Set_DeleteKey,args)
        return val
    def IDeleteKey(*args):
        val = apply(Nsc.Set_IDeleteKey,args)
        return val
    def Merge(*args):
        val = apply(Nsc.Set_Merge,args)
        return val
    def Copy(*args):
        val = apply(Nsc.Set_Copy,args)
        if val: val = SetPtr(val) ; val.thisown = 1
        return val
    def Move(*args):
        val = apply(Nsc.Set_Move,args)
        return val
    def Print(*args):
        val = apply(Nsc.Set_Print,args)
        return val
    def Size(*args):
        val = apply(Nsc.Set_Size,args)
        return val
    def Name(*args):
        val = apply(Nsc.Set_Name,args)
        return val
    def Key(*args):
        val = apply(Nsc.Set_Key,args)
        return val
    def Value(*args):
        val = apply(Nsc.Set_Value,args)
        return val
    def Last(*args):
        val = apply(Nsc.Set_Last,args)
        return val
    def __len__(*args):
        val = apply(Nsc.Set___len__,args)
        return val
    def __nonzero__(*args):
        val = apply(Nsc.Set___nonzero__,args)
        return val
    def __repr__(self):
        return "<C Set instance at %s>" % (self.this,)
    def __getitem__(self, i):
        if type(i) is types.IntType:
            if 0 <= i < self.Size():
                return self.Value(i)
            else:
                raise KeyError(`i`)
        else:
            retval = self.IGet(i)
            if retval is None:
                raise KeyError(`i`)
            else:
                return retval
        
    def __setitem__(self, i, value):
        if type(i) is types.IntType:
            if 0 <= i < self.Size():
                self.PutValue(i, value)
            else:
                raise KeyError(`i`)
        else:
            n = self.IFind(i)
            if n is not None:
                self.Delete(n)
            self.Put(i, value)
        
    def __delitem__(self, i):
        if type(i) is types.IntType:
            if 0 <= i < self.Size():
                self.Delete(i)
            else:
                raise KeyError(`i`)
        else:
            n = self.IFind(i)
            if n is None:
                raise KeyError(`i`)
            else:
                self.Delete(i)
        
    def has_key(self, i):
        if type(i) is types.IntType:
            return 0 <= i < self.Size()
        else:
            return self.IFind(i) is not None
        
    def keys(self):
        '''Return a list of the keys, in order by index number.

        Note that keys may appear multiple times.'''

        retval = []
        for i in range(self.Size()):
            retval.append(self.Key(i))
        return retval
        
    def values(self):
        '''Return a list of the values, in order by index number.'''

        retval = []
        for i in range(self.Size()):
            retval.append(self.Value(i))
        return retval
        
    def items(self):
        '''Return a list of (key,value) pairs, in order by index number.

        Note that keys may appear multiple times.'''

        retval = []
        for i in range(self.Size()):
            retval.append( (self.Key(i), self.Value(i),) )
        return retval
        
    def dict(self):
        '''Return the set as a python dictionary.

        Raise a KeyError if the set\'s keys are not unique.'''

        retval = {}
        for (key,value) in self.items():
            if retval.has_key(key):
                raise KeyError('Set keys are not unique')
            retval[key] = value
        return retval
        
    def get(self, i, default=None):
        '''Like usual dictionary get method.'''

        if type(i) is types.IntType:
            if 0 <= i < self.Size():
                return self.Value(i)
            else:
                return default
        else:
            retval = self.IGet(i)
            if retval is None:
                return default
            else:
                return retval
        
class SetPtr(Set):
    def __init__(self,this):
        self.this = this
        self.thisown = 0
        self.__class__ = Set



class Request:
    def __init__(self,this):
        self.this = this

    def __del__(self,Nsc=Nsc):
        if self.thisown == 1 :
            Nsc.delete_Request(self)
    def SkipUrl(*args):
        val = apply(Nsc.Request_SkipUrl,args)
        return val
    def SetRequestUrl(*args):
        val = apply(Nsc.Request_SetRequestUrl,args)
        return val
    __setmethods__ = {
        "line" : Nsc.Request_line_set,
        "method" : Nsc.Request_method_set,
        "protocol" : Nsc.Request_protocol_set,
        "host" : Nsc.Request_host_set,
        "port" : Nsc.Request_port_set,
        "url" : Nsc.Request_url_set,
        "query" : Nsc.Request_query_set,
        "urlc" : Nsc.Request_urlc_set,
        "urlv" : Nsc.Request_urlv_set,
        "version" : Nsc.Request_version_set,
    }
    def __setattr__(self,name,value):
        if (name == "this") or (name == "thisown"): self.__dict__[name] = value; return
        method = Request.__setmethods__.get(name,None)
        if method: return method(self,value)
        self.__dict__[name] = value
    __getmethods__ = {
        "line" : Nsc.Request_line_get,
        "method" : Nsc.Request_method_get,
        "protocol" : Nsc.Request_protocol_get,
        "host" : Nsc.Request_host_get,
        "port" : Nsc.Request_port_get,
        "url" : Nsc.Request_url_get,
        "query" : Nsc.Request_query_get,
        "urlc" : Nsc.Request_urlc_get,
        "urlv" : Nsc.Request_urlv_get,
        "version" : Nsc.Request_version_get,
    }
    def __getattr__(self,name):
        method = Request.__getmethods__.get(name,None)
        if method: return method(self)
        raise AttributeError,name
    def __repr__(self):
        return "<C Request instance at %s>" % (self.this,)
class RequestPtr(Request):
    def __init__(self,this):
        self.this = this
        self.thisown = 0
        self.__class__ = Request



class Conn:
    def __init__(self,this):
        self.this = this

    def Close(*args):
        val = apply(Nsc.Conn_Close,args)
        return val
    def Read(*args):
        val = apply(Nsc.Conn_Read,args)
        return val
    def Write(*args):
        val = apply(Nsc.Conn_Write,args)
        return val
    def ReadLine(*args):
        val = apply(Nsc.Conn_ReadLine,args)
        return val
    def WriteConn(*args):
        val = apply(Nsc.Conn_WriteConn,args)
        return val
    def SendFp(*args):
        val = apply(Nsc.Conn_SendFp,args)
        return val
    def SendFd(*args):
        val = apply(Nsc.Conn_SendFd,args)
        return val
    def CopyToDString(*args):
        val = apply(Nsc.Conn_CopyToDString,args)
        return val
    def CopyToFile(*args):
        val = apply(Nsc.Conn_CopyToFile,args)
        return val
    def CopyToFd(*args):
        val = apply(Nsc.Conn_CopyToFd,args)
        return val
    def FlushContent(*args):
        val = apply(Nsc.Conn_FlushContent,args)
        return val
    def ModifiedSince(*args):
        val = apply(Nsc.Conn_ModifiedSince,args)
        return val
    def ReadHeaders(*args):
        val = apply(Nsc.Conn_ReadHeaders,args)
        return val
    def GetQuery(*args):
        val = apply(Nsc.Conn_GetQuery,args)
        if val: val = SetPtr(val) 
        return val
    def Headers(*args):
        val = apply(Nsc.Conn_Headers,args)
        if val: val = SetPtr(val) 
        return val
    def OutputHeaders(*args):
        val = apply(Nsc.Conn_OutputHeaders,args)
        if val: val = SetPtr(val) 
        return val
    def AuthUser(*args):
        val = apply(Nsc.Conn_AuthUser,args)
        return val
    def AuthPasswd(*args):
        val = apply(Nsc.Conn_AuthPasswd,args)
        return val
    def ContentLength(*args):
        val = apply(Nsc.Conn_ContentLength,args)
        return val
    def Server(*args):
        val = apply(Nsc.Conn_Server,args)
        return val
    def ResponseStatus(*args):
        val = apply(Nsc.Conn_ResponseStatus,args)
        return val
    def ContentSent(*args):
        val = apply(Nsc.Conn_ContentSent,args)
        return val
    def ResponseLength(*args):
        val = apply(Nsc.Conn_ResponseLength,args)
        return val
    def Peer(*args):
        val = apply(Nsc.Conn_Peer,args)
        return val
    def PeerPort(*args):
        val = apply(Nsc.Conn_PeerPort,args)
        return val
    def Location(*args):
        val = apply(Nsc.Conn_Location,args)
        return val
    def Host(*args):
        val = apply(Nsc.Conn_Host,args)
        return val
    def Port(*args):
        val = apply(Nsc.Conn_Port,args)
        return val
    def Sock(*args):
        val = apply(Nsc.Conn_Sock,args)
        return val
    def DriverName(*args):
        val = apply(Nsc.Conn_DriverName,args)
        return val
    def DriverContext(*args):
        val = apply(Nsc.Conn_DriverContext,args)
        return val
    def RunRequest(*args):
        val = apply(Nsc.Conn_RunRequest,args)
        return val
    def Redirect(*args):
        val = apply(Nsc.Conn_Redirect,args)
        return val
    def ConstructHeaders(*args):
        val = apply(Nsc.Conn_ConstructHeaders,args)
        return val
    def FlushHeaders(*args):
        val = apply(Nsc.Conn_FlushHeaders,args)
        return val
    def SetHeaders(*args):
        val = apply(Nsc.Conn_SetHeaders,args)
        return val
    def CondSetHeaders(*args):
        val = apply(Nsc.Conn_CondSetHeaders,args)
        return val
    def ReplaceHeaders(*args):
        val = apply(Nsc.Conn_ReplaceHeaders,args)
        return val
    def SetRequiredHeaders(*args):
        val = apply(Nsc.Conn_SetRequiredHeaders,args)
        return val
    def SetTypeHeader(*args):
        val = apply(Nsc.Conn_SetTypeHeader,args)
        return val
    def SetLengthHeader(*args):
        val = apply(Nsc.Conn_SetLengthHeader,args)
        return val
    def SetLastModifiedHeader(*args):
        val = apply(Nsc.Conn_SetLastModifiedHeader,args)
        return val
    def SetExpiresHeader(*args):
        val = apply(Nsc.Conn_SetExpiresHeader,args)
        return val
    def PrintfHeader(*args):
        val = apply(Nsc.Conn_PrintfHeader,args)
        return val
    def ReturnAdminNotice(*args):
        val = apply(Nsc.Conn_ReturnAdminNotice,args)
        return val
    def ReturnNotice(*args):
        val = apply(Nsc.Conn_ReturnNotice,args)
        return val
    def ReturnData(*args):
        val = apply(Nsc.Conn_ReturnData,args)
        return val
    def ReturnHtml(*args):
        val = apply(Nsc.Conn_ReturnHtml,args)
        return val
    def ReturnOk(*args):
        val = apply(Nsc.Conn_ReturnOk,args)
        return val
    def ReturnNoResponse(*args):
        val = apply(Nsc.Conn_ReturnNoResponse,args)
        return val
    def ReturnRedirect(*args):
        val = apply(Nsc.Conn_ReturnRedirect,args)
        return val
    def ReturnBadRequest(*args):
        val = apply(Nsc.Conn_ReturnBadRequest,args)
        return val
    def ReturnUnauthorized(*args):
        val = apply(Nsc.Conn_ReturnUnauthorized,args)
        return val
    def ReturnForbidden(*args):
        val = apply(Nsc.Conn_ReturnForbidden,args)
        return val
    def ReturnNotFound(*args):
        val = apply(Nsc.Conn_ReturnNotFound,args)
        return val
    def ReturnNotModified(*args):
        val = apply(Nsc.Conn_ReturnNotModified,args)
        return val
    def ReturnNotImplemented(*args):
        val = apply(Nsc.Conn_ReturnNotImplemented,args)
        return val
    def ReturnInternalError(*args):
        val = apply(Nsc.Conn_ReturnInternalError,args)
        return val
    def ReturnStatus(*args):
        val = apply(Nsc.Conn_ReturnStatus,args)
        return val
    def ReturnOpenFile(*args):
        val = apply(Nsc.Conn_ReturnOpenFile,args)
        return val
    def ReturnOpenFd(*args):
        val = apply(Nsc.Conn_ReturnOpenFd,args)
        return val
    def ReturnFile(*args):
        val = apply(Nsc.Conn_ReturnFile,args)
        return val
    def GetConnInterp(*args):
        val = apply(Nsc.Conn_GetConnInterp,args)
        return val
    def TclEval(*args):
        val = apply(Nsc.Conn_TclEval,args)
        return val
    __setmethods__ = {
        "request" : Nsc.Conn_request_set,
        "headers" : Nsc.Conn_headers_set,
        "outputheaders" : Nsc.Conn_outputheaders_set,
        "authUser" : Nsc.Conn_authUser_set,
        "authPasswd" : Nsc.Conn_authPasswd_set,
        "contentLength" : Nsc.Conn_contentLength_set,
        "flags" : Nsc.Conn_flags_set,
    }
    def __setattr__(self,name,value):
        if (name == "this") or (name == "thisown"): self.__dict__[name] = value; return
        method = Conn.__setmethods__.get(name,None)
        if method: return method(self,value)
        self.__dict__[name] = value
    __getmethods__ = {
        "request" : lambda x : RequestPtr(Nsc.Conn_request_get(x)),
        "headers" : lambda x : SetPtr(Nsc.Conn_headers_get(x)),
        "outputheaders" : lambda x : SetPtr(Nsc.Conn_outputheaders_get(x)),
        "authUser" : Nsc.Conn_authUser_get,
        "authPasswd" : Nsc.Conn_authPasswd_get,
        "contentLength" : Nsc.Conn_contentLength_get,
        "flags" : Nsc.Conn_flags_get,
    }
    def __getattr__(self,name):
        method = Conn.__getmethods__.get(name,None)
        if method: return method(self)
        raise AttributeError,name
    def __repr__(self):
        return "<C Conn instance at %s>" % (self.this,)
class ConnPtr(Conn):
    def __init__(self,this):
        self.this = this
        self.thisown = 0
        self.__class__ = Conn



class DbHandle:
    def __init__(self,*args):
        self.this = apply(Nsc.new_DbHandle,args)
        self.thisown = 1

    def __del__(self,Nsc=Nsc):
        if self.thisown == 1 :
            Nsc.delete_DbHandle(self)
    def DriverName(*args):
        val = apply(Nsc.DbHandle_DriverName,args)
        return val
    def DriverDbType(*args):
        val = apply(Nsc.DbHandle_DriverDbType,args)
        return val
    def DML(*args):
        val = apply(Nsc.DbHandle_DML,args)
        return val
    def Select(*args):
        val = apply(Nsc.DbHandle_Select,args)
        if val: val = SetPtr(val) 
        return val
    def Exec(*args):
        val = apply(Nsc.DbHandle_Exec,args)
        return val
    def BindRow(*args):
        val = apply(Nsc.DbHandle_BindRow,args)
        if val: val = SetPtr(val) 
        return val
    def GetRow(*args):
        val = apply(Nsc.DbHandle_GetRow,args)
        return val
    def Flush(*args):
        val = apply(Nsc.DbHandle_Flush,args)
        return val
    def Cancel(*args):
        val = apply(Nsc.DbHandle_Cancel,args)
        return val
    def ResetHandle(*args):
        val = apply(Nsc.DbHandle_ResetHandle,args)
        return val
    def SpStart(*args):
        val = apply(Nsc.DbHandle_SpStart,args)
        return val
    def SpSetParam(*args):
        val = apply(Nsc.DbHandle_SpSetParam,args)
        return val
    def SpExec(*args):
        val = apply(Nsc.DbHandle_SpExec,args)
        return val
    def SpGetParams(*args):
        val = apply(Nsc.DbHandle_SpGetParams,args)
        if val: val = SetPtr(val) ; val.thisown = 1
        return val
    def _Db0or1Row_Py(*args):
        val = apply(Nsc.DbHandle__Db0or1Row_Py,args)
        return val
    def Db1Row(*args):
        val = apply(Nsc.DbHandle_Db1Row,args)
        if val: val = SetPtr(val) ; val.thisown = 1
        return val
    def InterpretSqlFile(*args):
        val = apply(Nsc.DbHandle_InterpretSqlFile,args)
        return val
    def SetException(*args):
        val = apply(Nsc.DbHandle_SetException,args)
        return val
    __setmethods__ = {
        "driver" : Nsc.DbHandle_driver_set,
        "datasource" : Nsc.DbHandle_datasource_set,
        "user" : Nsc.DbHandle_user_set,
        "password" : Nsc.DbHandle_password_set,
        "connection" : Nsc.DbHandle_connection_set,
        "poolname" : Nsc.DbHandle_poolname_set,
        "connected" : Nsc.DbHandle_connected_set,
        "verbose" : Nsc.DbHandle_verbose_set,
        "row" : Nsc.DbHandle_row_set,
        "dsExceptionMsg" : Nsc.DbHandle_dsExceptionMsg_set,
        "context" : Nsc.DbHandle_context_set,
        "statement" : Nsc.DbHandle_statement_set,
        "fetchingRows" : Nsc.DbHandle_fetchingRows_set,
    }
    def __setattr__(self,name,value):
        if (name == "this") or (name == "thisown"): self.__dict__[name] = value; return
        method = DbHandle.__setmethods__.get(name,None)
        if method: return method(self,value)
        self.__dict__[name] = value
    __getmethods__ = {
        "driver" : Nsc.DbHandle_driver_get,
        "datasource" : Nsc.DbHandle_datasource_get,
        "user" : Nsc.DbHandle_user_get,
        "password" : Nsc.DbHandle_password_get,
        "connection" : Nsc.DbHandle_connection_get,
        "poolname" : Nsc.DbHandle_poolname_get,
        "connected" : Nsc.DbHandle_connected_get,
        "verbose" : Nsc.DbHandle_verbose_get,
        "row" : lambda x : SetPtr(Nsc.DbHandle_row_get(x)),
        "dsExceptionMsg" : Nsc.DbHandle_dsExceptionMsg_get,
        "context" : Nsc.DbHandle_context_get,
        "statement" : Nsc.DbHandle_statement_get,
        "fetchingRows" : Nsc.DbHandle_fetchingRows_get,
    }
    def __getattr__(self,name):
        method = DbHandle.__getmethods__.get(name,None)
        if method: return method(self)
        raise AttributeError,name
    def __repr__(self):
        return "<C DbHandle instance at %s>" % (self.this,)
    def Db0or1Row(self, sql):
        retval = Nsc.DbHandle__Db0or1Row_Py(self, sql)
        retval = (DbHandlePtr(retval[0]), retval[1])
        retval[0].thisown = 1
        return retval
        
class DbHandlePtr(DbHandle):
    def __init__(self,this):
        self.this = this
        self.thisown = 0
        self.__class__ = DbHandle





#-------------- FUNCTION WRAPPERS ------------------

ParseRequest = Nsc.ParseRequest

def ParseHeader(*args, **kwargs):
    val = apply(Nsc.ParseHeader,args,kwargs)
    if val: val = SetPtr(val); val.thisown = 1
    return val

def QueryToSet(*args, **kwargs):
    val = apply(Nsc.QueryToSet,args,kwargs)
    if val: val = SetPtr(val); val.thisown = 1
    return val

RegisterReturn = Nsc.RegisterReturn

DbPoolDescription = Nsc.DbPoolDescription

DbPoolDefault = Nsc.DbPoolDefault

DbPoolList = Nsc.DbPoolList

DbPoolAllowable = Nsc.DbPoolAllowable

def DbPoolTimedGetHandle(*args, **kwargs):
    val = apply(Nsc.DbPoolTimedGetHandle,args,kwargs)
    if val: val = DbHandlePtr(val); val.thisown = 1
    return val

def DbPoolGetHandle(*args, **kwargs):
    val = apply(Nsc.DbPoolGetHandle,args,kwargs)
    if val: val = DbHandlePtr(val); val.thisown = 1
    return val

_DbPoolGetMultipleHandles_Py = Nsc._DbPoolGetMultipleHandles_Py

_DbPoolTimedGetMultipleHandles_Py = Nsc._DbPoolTimedGetMultipleHandles_Py

DbBouncePool = Nsc.DbBouncePool

DbQuoteValue = Nsc.DbQuoteValue

ConfigGetValue = Nsc.ConfigGetValue

ConfigGetValueExact = Nsc.ConfigGetValueExact

ConfigGetInt = Nsc.ConfigGetInt

ConfigGetBool = Nsc.ConfigGetBool

_ConfigGetSections_Py = Nsc._ConfigGetSections_Py

def ConfigGetSection(*args, **kwargs):
    val = apply(Nsc.ConfigGetSection,args,kwargs)
    if val: val = SetPtr(val)
    return val

Encrypt = Nsc.Encrypt

GetHostByAddr = Nsc.GetHostByAddr

GetAddrByHost = Nsc.GetAddrByHost

PageRoot = Nsc.PageRoot

UrlToFile = Nsc.UrlToFile

UrlIsFile = Nsc.UrlIsFile

UrlIsDir = Nsc.UrlIsDir

GetUserHome = Nsc.GetUserHome

GetGid = Nsc.GetGid

GetUserGid = Nsc.GetUserGid

GetUid = Nsc.GetUid

InfoErrorLog = Nsc.InfoErrorLog

LogRoll = Nsc.LogRoll

RollFile = Nsc.RollFile

PurgeFiles = Nsc.PurgeFiles

RollFileByDate = Nsc.RollFileByDate

Log = Nsc.Log

Ns_Fatal = Nsc.Ns_Fatal

InfoHomePath = Nsc.InfoHomePath

InfoServerName = Nsc.InfoServerName

InfoServerVersion = Nsc.InfoServerVersion

InfoConfigFile = Nsc.InfoConfigFile

InfoPid = Nsc.InfoPid

InfoPlatform = Nsc.InfoPlatform

InfoUptime = Nsc.InfoUptime

InfoBootTime = Nsc.InfoBootTime

InfoHostname = Nsc.InfoHostname

InfoAddress = Nsc.InfoAddress

InfoBuildDate = Nsc.InfoBuildDate

WaitForStartup = Nsc.WaitForStartup

InfoShutdownPending = Nsc.InfoShutdownPending

InfoStarted = Nsc.InfoStarted

InfoServersStarted = Nsc.InfoServersStarted

InfoLabel = Nsc.InfoLabel

StopServer = Nsc.StopServer

GetMimeType = Nsc.GetMimeType

PathIsAbsolute = Nsc.PathIsAbsolute

NormalizePath = Nsc.NormalizePath

MakePath = Nsc.MakePath

LibPath = Nsc.LibPath

HomePath = Nsc.HomePath

ModulePath = Nsc.ModulePath

QuoteHtml = Nsc.QuoteHtml

TclInitModule = Nsc.TclInitModule

TclMarkForDelete = Nsc.TclMarkForDelete

TclDestroyInterp = Nsc.TclDestroyInterp

TclAllocateInterp = Nsc.TclAllocateInterp

TclDeAllocateInterp = Nsc.TclDeAllocateInterp

TclLibrary = Nsc.TclLibrary

TclInterpServer = Nsc.TclInterpServer

def TclGetConn(*args, **kwargs):
    val = apply(Nsc.TclGetConn,args,kwargs)
    if val: val = ConnPtr(val)
    return val

def GetConn(*args, **kwargs):
    val = apply(Nsc.GetConn,args,kwargs)
    if val: val = ConnPtr(val)
    return val

TclEval = Nsc.TclEval

Tcl_GetVar = Nsc.Tcl_GetVar

HttpTime = Nsc.HttpTime

ParseHttpTime = Nsc.ParseHttpTime

RelativeUrl = Nsc.RelativeUrl

ParseUrl = Nsc.ParseUrl

AbsoluteUrl = Nsc.AbsoluteUrl

EncodeUrl = Nsc.EncodeUrl

DecodeUrl = Nsc.DecodeUrl

FetchPage = Nsc.FetchPage

FetchURL = Nsc.FetchURL

CloseOnExec = Nsc.CloseOnExec

NoCloseOnExec = Nsc.NoCloseOnExec

DupHigh = Nsc.DupHigh



#-------------- VARIABLE WRAPPERS ------------------

OK = Nsc.OK
ERROR = Nsc.ERROR
CONN_CLOSED = Nsc.CONN_CLOSED
CONN_SKIPHDRS = Nsc.CONN_SKIPHDRS
CONN_SKIPBODY = Nsc.CONN_SKIPBODY
UNAUTHORIZED = Nsc.UNAUTHORIZED
FORBIDDEN = Nsc.FORBIDDEN
FILTER_BREAK = Nsc.FILTER_BREAK
FILTER_RETURN = Nsc.FILTER_RETURN
SHUTDOWN = Nsc.SHUTDOWN
TRUE = Nsc.TRUE
FALSE = Nsc.FALSE
FILTER_PRE_AUTH = Nsc.FILTER_PRE_AUTH
FILTER_POST_AUTH = Nsc.FILTER_POST_AUTH
FILTER_TRACE = Nsc.FILTER_TRACE
FILTER_VOID_TRACE = Nsc.FILTER_VOID_TRACE
REGISTER_SERVER_TRACE = Nsc.REGISTER_SERVER_TRACE
OP_NOINHERIT = Nsc.OP_NOINHERIT
OP_NODELETE = Nsc.OP_NODELETE
OP_RECURSE = Nsc.OP_RECURSE
SCHED_THREAD = Nsc.SCHED_THREAD
SCHED_ONCE = Nsc.SCHED_ONCE
SCHED_DAILY = Nsc.SCHED_DAILY
SCHED_WEEKLY = Nsc.SCHED_WEEKLY
SCHED_PAUSED = Nsc.SCHED_PAUSED
SCHED_RUNNING = Nsc.SCHED_RUNNING
SOCK_READ = Nsc.SOCK_READ
SOCK_WRITE = Nsc.SOCK_WRITE
SOCK_EXCEPTION = Nsc.SOCK_EXCEPTION
SOCK_EXIT = Nsc.SOCK_EXIT
NO_DATA = Nsc.NO_DATA
END_DATA = Nsc.END_DATA
ROWS = Nsc.ROWS
DML = Nsc.DML
ENCRYPT_BUFSIZE = Nsc.ENCRYPT_BUFSIZE
TCL_SET_TEMPORARY = Nsc.TCL_SET_TEMPORARY
TCL_SET_DYNAMIC = Nsc.TCL_SET_DYNAMIC
TCL_SET_PERSISTENT = Nsc.TCL_SET_PERSISTENT
DSTRING_STATIC_SIZE = Nsc.DSTRING_STATIC_SIZE
INVALID_SOCKET = Nsc.INVALID_SOCKET
SOCKET_ERROR = Nsc.SOCKET_ERROR
Notice = Nsc.Notice
Warning = Nsc.Warning
Error = Nsc.Error
Fatal = Nsc.Fatal
Bug = Nsc.Bug
Debug = Nsc.Debug
Preserve = Nsc.Preserve
ToLower = Nsc.ToLower
ToUpper = Nsc.ToUpper
cvar = Nsc.cvar
