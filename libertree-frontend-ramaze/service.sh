#!/bin/bash

PREFIX=/opt/libertree
export GEM_HOME=${PREFIX}/gems/

start() {
    cd ${PREFIX}/frontend-ramaze
    su libertree -c 'mkdir -p pids/ log/'
    su libertree -c "${GEM_HOME}/bin/unicorn -Ilib -c config/unicorn.conf config.ru" &
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
