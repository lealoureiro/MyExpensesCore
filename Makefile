PROJECT = expenses_gateway
 
DEPS = cowboy jiffy cqerl
dep_cowboy = pkg://cowboy master
dep_jiffy = https://github.com/davisp/jiffy.git 0.8.5
dep_cqerl = https://github.com/matehat/cqerl.git master

include erlang.mk
