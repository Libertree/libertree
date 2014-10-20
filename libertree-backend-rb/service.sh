#!/bin/bash

PREFIX=/opt/libertree
GEM_HOME=${PREFIX}/gems/

start() {
    cd ${PREFIX}/backend-rb
    su libertree -c 'ruby -I./lib ./bin/server.rb config.yaml database.yaml' &
    su libertree -c 'ruby -I./lib ./bin/job-processor.rb config.yaml database.yaml' &
}

stop() {
    kill $(cat ./pids/*)
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
