#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
dir=`dirname $0`
dir=`cd $dir; pwd`
cfgfile=$dir/conf/webcli
exec erl -detached -heart -config $cfgfile -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname webcli_dev \
    -s webcli \
    -s reloader &

