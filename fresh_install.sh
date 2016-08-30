#!/usr/bin/env bash
rm -rf ebin
rm -rf _rel
rm -rf deps
rm -rf .erlang.mk
make
cp ./deploy/sys.config ./_rel/releases/0.1.0/sys.config
