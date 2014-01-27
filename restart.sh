#!/bin/sh

kill `ps x | grep -v grep | grep beam | grep webcli | awk '{print $1}'`
dir=`dirname $0`
dir=`cd $dir; pwd`
$dir/start.sh
