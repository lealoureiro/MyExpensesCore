PROJECT = expenses_gateway
 
DEPS = cowboy jiffy mysql_client
dep_cowboy = pkg://cowboy master
dep_jiffy = https://github.com/davisp/jiffy.git 0.8.5

include erlang.mk
