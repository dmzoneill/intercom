#!/bin/sh

#erl -detached -pa deps/*/ebin apps/*/ebin -s intercom_app start
erl -detached -pa deps/*/ebin apps/*/ebin -boot start -s intercom_app
