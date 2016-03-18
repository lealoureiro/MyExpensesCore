PROJECT = expenses_gateway
 
DEPS = re2 lager cowboy jiffy cqerl
dep_re2 = git https://github.com/tuncer/re2.git v1.2.2
dep_lager = git https://github.com/basho/lager.git master
dep_cowboy = git https://github.com/ninenines/cowboy.git 1.1.x
dep_jiffy = git https://github.com/davisp/jiffy.git master
dep_cqerl = git https://github.com/matehat/cqerl.git master

include erlang.mk
