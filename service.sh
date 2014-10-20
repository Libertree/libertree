#!/bin/bash

PREFIX=/opt/libertree
export GEM_HOME=${PREFIX}/gems/

start() {
    cd ${PREFIX}/backend-rb
    su libertree -c 'ruby -I./lib ./bin/server.rb config.yaml database.yaml' &
    su libertree -c 'ruby -I./lib ./bin/job-processor.rb config.yaml database.yaml' &
}

stop() {
    for pid in $(find ./pids/ -type f); do
      kill $(cat $pid) && rm $pid
    done
}

case "$1" in
    start)
        start
        ;;
    stop)
        stop
        ;;
    restart)
        start
        stop
        ;;
    *)
        echo "usage: $0 {start,stop,restart}"
esac
