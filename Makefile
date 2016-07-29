PROJECT = expenses_gateway
 
DEPS = re2 lager cowboy jiffy cqerl
dep_re2 = git https://github.com/tuncer/re2.git v1.2.2
dep_lager = git https://github.com/basho/lager.git 3.1.0
dep_cowboy = git https://github.com/ninenines/cowboy.git 1.1.x
dep_jiffy = git https://github.com/davisp/jiffy.git 0.14.7
dep_cqerl = git https://github.com/matehat/cqerl.git v1.0.1

include erlang.mk
