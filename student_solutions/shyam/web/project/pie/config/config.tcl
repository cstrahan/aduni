#
# Translated on Tue Jan 23 18:44:48 EST 2001
# from .ini format with /usr/local/aolserver/bin/translate-ini
#
# AOLServer ACS Virtual Server Configuration File Template
#
# Replace these variables with the correct values...
# Variable to Replace		 Suggestion
# --------------------------+----------------------
set service_database_username shyam 
set service_database_password Sam2bar
set AOL_HOME /usr/local/aolserver
set service_short_name shyam
# service_short_description | a description of your service
# service_email	            | email-address of maintainer
set webroot /opt/web
set service_ip_address 10.11.0.122
# service_ip_name	    | your dns name
set service_port 8080
# service_ip_address_ssl    | disabled by default
# service_ip_name_ssl       | disabled by default
# service_port_ssl	    | disabled by default

ns_section ns/db/drivers
ns_param ora8 ora8.so

ns_section ns/db/pool/main
ns_param MaxIdle 1000000000
ns_param MaxOpen 1000000000
ns_param Driver ora8
ns_param Connections 4
ns_param DataSource {}
ns_param User $service_database_username
ns_param Password $service_database_password
ns_param Verbose On
ns_param ExtendedTableInfo On

ns_section ns/db/pool/subquery
ns_param MaxIdle 1000000000
ns_param MaxOpen 1000000000
ns_param Driver ora8
ns_param Connections 4
ns_param DataSource {}
ns_param User $service_database_username
ns_param Password $service_database_password
ns_param Verbose On
ns_param ExtendedTableInfo On

ns_section ns/db/pool/log
ns_param MaxIdle 1000000000
ns_param MaxOpen 1000000000
ns_param Driver ora8
ns_param Connections 3
ns_param DataSource {}
ns_param User $service_database_username
ns_param Password $service_database_password
ns_param Verbose On
ns_param ExtendedTableInfo On

ns_section ns/db/pools
ns_param main main
ns_param subquery subquery
ns_param log log

ns_section ns/parameters
# This used to handled with a file under acs/parameters, but 
# parameters are now stored in the database.
ns_param User nsadmin
ns_param Group web
ns_param ServerLog $AOL_HOME/log/$service_short_name-error.log
ns_param Home $AOL_HOME
ns_param StackSize 500000
ns_param MaxKeepAlive 0
ns_param MailHost localhost

ns_section ns/threads
# use more than 1 processor (Solaris)
ns_param SystemScope on

ns_section ns/server/$service_short_name
ns_param PageRoot $webroot/$service_short_name/www
ns_param DirectoryFile {index.tcl,index.adp,index.html,index.htm }
ns_param Webmaster $service_short_name@arsdigita.com
ns_param NoticeBgColor {"#ffffff"}
ns_param EnableTclPages On
ns_param NotFoundResponse /global/file-not-found.html
ns_param ServerBusyResponse /global/busy.html
ns_param ServerInternalErrorResponse /global/error.html
ns_param MaxThreads 20
ns_param MaxBusyThreads 15
ns_param MaxWait 2

ns_section ns/server/$service_short_name/db
ns_param Pools main,subquery,log
ns_param DefaultPool main

ns_section ns/server/$service_short_name/adp
ns_param Map /*.adp
ns_param DefaultParser fancy

ns_section ns/server/$service_short_name/module/nslog
ns_param EnableHostnameLookup Off
ns_param File /$AOL_HOME/log/$service_short_name.log
ns_param LogCombined On
ns_param LogRefer Off
ns_param LogUserAgent Off
ns_param MaxBackup 5
ns_param RollDay *
ns_param RollFmt %Y-%m-%d-%H:%M
ns_param RollHour 0
ns_param RollOnSignal On
ns_param RollLog On

ns_section ns/server/$service_short_name/module/nsperm
ns_param model Small
ns_param enablehostnamelookup Off

ns_section ns/server/$service_short_name/module/nssock
ns_param timeout 120
ns_param Address $service_ip_address
#ns_param Hostname lcsweb143.lcs.mit.edu
ns_param Port $service_port
# [ns/server/$service_short_name/module/nsssl]
# Address=18.43.3.143
# Hostname=lcsweb143.lcs.mit.edu
# CertFile=/home/aol32/servers/$service_short_name/cert.pem
# KeyFile=/home/aol32/servers/$service_short_name/key.pem
# Port=443

ns_section ns/server/$service_short_name/modules
ns_param nsperm nsperm.so
ns_param nssock nssock.so
ns_param nslog nslog.so
#ns_param nssha1 nssha1.so
#ns_param nscache nscache.so
# nsssl=nsssle.so

ns_section ns/server/$service_short_name/MimeTypes
ns_param Default text/plain
ns_param NoExtension text/plain
ns_param .pcd image/x-photo-cd
ns_param .prc application/x-pilot

ns_section ns/mimetypes
ns_param .wml text/vnd.wap.wml

ns_section ns/server/$service_short_name/tcl
ns_param Library $webroot/$service_short_name/tcl

ns_section ns/servers
ns_param $service_short_name $service_short_name
