#!/bin/bash
if [[ -f libertree-launcher.conf ]]; then
  . libertree-launcher.conf
fi
# All the below variables can be defined in libertree-launcher.conf
# file in the same directory as the libertree-launcher.sh
LIBERTREE_ENV=${LIBERTREE_ENV:-production}
LIBERTREE_BACKEND_PATH=${LIBERTREE_BACKEND_PATH:-../libertree-backend-rb}
LIBERTREE_FRONTEND_PATH=${LIBERTREE_FRONTEND_PATH:-../libertree-frontend-ramaze}
LIBERTREE_FRONTEND_PORT=${LIBERTREE_FRONTEND_PORTH:-8088}
LIBERTREE_RUN_DIR=${LIBERTREE_RUN_DIR:-../run}
LIBERTREE_LOG_PATH=${LIBERTREE_LOG_PATH:-../logs}

start_backend_server(){
  echo "Starting backend server"
  pushd "$LIBERTREE_BACKEND_PATH"
  mkdir -p "$LIBERTREE_RUN_DIR"
  mkdir -p "$LIBERTREE_LOG_PATH"
  bundle exec ruby -Ilib bin/server.rb config.yaml >> $LIBERTREE_LOG_PATH/backend.log &
  local rc=$(echo $?)
  local pid=$(echo $!)
  if [[ $rc == 0 ]]; then
    echo $pid > "$LIBERTREE_RUN_DIR"/backend.pid
    echo "Startup of backend server succeeded with PID $pid"
	else
    echo "Startup failed"
  fi
  popd
}

start_job_server(){
  echo "Starting job server"
  pushd "$LIBERTREE_BACKEND_PATH"
  mkdir -p "$LIBERTREE_RUN_DIR"
  mkdir -p "$LIBERTREE_LOG_PATH"
  bundle exec ruby bin/job-processor.rb config.yaml >> $LIBERTREE_LOG_PATH/backend.log &
  local rc=$(echo $?)
  local pid=$(echo $!)
  if [[ $rc == 0 ]]; then
    echo $pid > "$LIBERTREE_RUN_DIR"/job.pid
    echo "Startup of job server succeeded with PID $pid"
	else
    echo "Startup failed"
  fi
  popd
}

start_frontend_server(){
  echo "Starting frontend server"
  pushd "$LIBERTREE_FRONTEND_PATH"
  mkdir -p "$LIBERTREE_RUN_DIR"
  mkdir -p "$LIBERTREE_LOG_PATH"
  ./css-build.sh
  if [[ -f "$LIBERTREE_FRONTEND_PATH"/unicorn-frontend.conf ]]; then
    bundle exec unicorn -D -c "$LIBERTREE_FRONTEND_PATH"/unicorn-frontend.conf
    local rc=$(echo $?)
    local pid=$(cat $LIBERTREE_RUN_DIR/frontend.pid)
    if [[ $rc == 0 ]]; then
      echo "Startup of frontend server succeeded with PID $pid"
	  else
      echo "Startup failed"
    fi
  else
    bundle exec unicorn -D -p $LIBERTREE_FRONTEND_PORT
    local rc=$(echo $?)
    local pid=$(echo $!)
    if [[ $rc == 0 ]]; then
      echo $pid > "$LIBERTREE_RUN_DIR"/frontend.pid
      echo "Startup of frontend server succeeded with PID $pid"
    else
      echo "Startup failed"
    fi
  fi
  popd
}

start_websocket_server(){
  echo "Starting websocket server"
  pushd "$LIBERTREE_FRONTEND_PATH"
  mkdir -p "$LIBERTREE_RUN_DIR"
  mkdir -p "$LIBERTREE_LOG_PATH"
  bundle exec ruby websocket-server.rb >> $LIBERTREE_LOG_PATH/websocket.log &
  local rc=$(echo $?)
  local pid=$(echo $!)
  if [[ $rc == 0 ]]; then
    echo $pid > "$LIBERTREE_RUN_DIR"/websocket.pid
    echo "Startup of websocket server succeeded with PID $pid"
	else
    echo "Startup failed"
  fi
  popd
}

stop_backend_server(){
  echo "Stopping backend server"
  kill -15 $(cat "$LIBERTREE_RUN_DIR"/backend.pid) &&
	echo "Stopped backend server, cleaning pid file" &&
  rm "$LIBERTREE_RUN_DIR"/backend.pid
}

stop_job_server(){
  echo "Stopping job server"
  kill -15 $(cat "$LIBERTREE_RUN_DIR"/job.pid) &&
	echo "Stopped job server, cleaning pid file" &&
  rm "$LIBERTREE_RUN_DIR"/job.pid
}

stop_frontend_server(){
  echo "Stopping frontend server"
  kill -15 $(cat "$LIBERTREE_RUN_DIR"/frontend.pid) &&
	echo "Stopped frontend server, cleaning pid file" &&
  rm "$LIBERTREE_RUN_DIR"/frontend.pid
}

stop_websocket_server(){
  echo "Stopping websocket server"
  kill -15 $(cat "$LIBERTREE_RUN_DIR"/websocket.pid) &&
	echo "Stopped websocket server, cleaning pid file" &&
  rm "$LIBERTREE_RUN_DIR"/websocket.pid
}

start(){
  start_backend_server
  start_job_server
  start_frontend_server
  start_websocket_server
}

stop(){
  stop_backend_server
  stop_job_server
  stop_frontend_server
  stop_websocket_server
}

$1

