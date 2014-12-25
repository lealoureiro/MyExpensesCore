PROJECT = expenses_gateway
 
DEPS = lager cowboy jiffy cqerl
dep_lager = https://github.com/basho/lager.git master
dep_cowboy = pkg://cowboy 1.0.1
dep_jiffy = https://github.com/davisp/jiffy.git 0.8.5
dep_cqerl = https://github.com/matehat/cqerl.git master

include erlang.mk
