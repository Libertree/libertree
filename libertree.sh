#!/bin/bash

if [[ -f libertree-launcher.conf ]]; then
  . libertree-launcher.conf
fi

# All of the following variables can be defined in a libertree-launcher.conf
# file in the same directory as this script
LIBERTREE_ENV=${LIBERTREE_ENV:-production}
LIBERTREE_BACKEND_PATH=${LIBERTREE_BACKEND_PATH:-../libertree-backend-rb}
LIBERTREE_FRONTEND_PATH=${LIBERTREE_FRONTEND_PATH:-../libertree-frontend-ramaze}
LIBERTREE_FRONTEND_PORT=${LIBERTREE_FRONTEND_PORT:-8088}
LIBERTREE_PID_DIR=${LIBERTREE_PID_DIR:-pid}
LIBERTREE_LOG_PATH=${LIBERTREE_LOG_PATH:-log}
LIBERTREE_USE_RVM=${LIBERTREE_USE_RVM:-1}

if [[ $LIBERTREE_USE_RVM == 1 ]]; then
  if [[ -s "$HOME/.rvm/scripts/rvm" ]]; then
    source "$HOME/.rvm/scripts/rvm"
  fi
fi

ensure_dirs_exist() {
  mkdir -p "$LIBERTREE_PID_DIR"
  mkdir -p "$LIBERTREE_LOG_PATH"
}

start_backend_server(){
  echo "Starting backend..."
  pushd "$LIBERTREE_BACKEND_PATH"
  ensure_dirs_exist
  if [[ $LIBERTREE_USE_RVM == 1 ]]; then
    rvm use 1.9.3@libertree-backend-rb
  fi
  bundle exec ruby -Ilib bin/server.rb config.yaml >> $LIBERTREE_LOG_PATH/backend-startup.log &
  local rc=$(echo $?)
  local pid=$(echo $!)
  if [[ $rc == 0 ]]; then
    echo $pid > "$LIBERTREE_PID_DIR"/backend.pid
    echo "Backend started.  PID: $pid"
	else
    echo "Failed to start backend."
  fi
  popd
}

start_job_server(){
  echo "Starting job processor..."
  pushd "$LIBERTREE_BACKEND_PATH"
  ensure_dirs_exist
  if [[ $LIBERTREE_USE_RVM == 1 ]]; then
    rvm use 1.9.3@libertree-backend-rb
  fi
  bundle exec ruby bin/job-processor.rb config.yaml >> $LIBERTREE_LOG_PATH/backend-startup.log &
  local rc=$(echo $?)
  local pid=$(echo $!)
  if [[ $rc == 0 ]]; then
    echo $pid > "$LIBERTREE_PID_DIR"/job.pid
    echo "Job processor started.  PID: $pid"
	else
    echo "Failed to start job processor."
  fi
  popd
}

start_frontend_server(){
  echo "Starting frontend..."
  pushd "$LIBERTREE_FRONTEND_PATH"
  ensure_dirs_exist
  if [[ $LIBERTREE_USE_RVM == 1 ]]; then
    rvm use 1.9.3@libertree-frontend-ramaze
  fi
  ./css-build.sh
  if [[ -f "$LIBERTREE_FRONTEND_PATH"/unicorn-frontend.conf ]]; then
    bundle exec unicorn -D -c "$LIBERTREE_FRONTEND_PATH"/unicorn-frontend.conf
    local rc=$(echo $?)
    local pid=$(cat $LIBERTREE_PID_DIR/frontend.pid)
    if [[ $rc == 0 ]]; then
      echo "Frontend started.  PID: $pid"
	  else
      echo "Failed to start frontend."
    fi
  else
    bundle exec unicorn -D -p $LIBERTREE_FRONTEND_PORT
    local rc=$(echo $?)
    local pid=$(echo $!)
    if [[ $rc == 0 ]]; then
      echo $pid > "$LIBERTREE_PID_DIR"/frontend.pid
      echo "Frontend started.  PID: $pid"
    else
      echo "Failed to start frontend"
    fi
  fi
  popd
}

start_websocket_server(){
  echo "Starting websocket server"
  pushd "$LIBERTREE_FRONTEND_PATH"
  ensure_dirs_exist
  if [[ $LIBERTREE_USE_RVM == 1 ]]; then
    rvm use 1.9.3@libertree-frontend-ramaze
  fi
  bundle exec ruby websocket-server.rb >> $LIBERTREE_LOG_PATH/websocket.log &
  local rc=$(echo $?)
  local pid=$(echo $!)
  if [[ $rc == 0 ]]; then
    echo $pid > "$LIBERTREE_PID_DIR"/websocket.pid
    echo "Websocket server started.  PID: $pid"
	else
    echo "Failed to start websocket server."
  fi
  popd
}

stop_backend_server(){
  echo "Stopping backend..."
  kill -15 $(cat "$LIBERTREE_PID_DIR"/backend.pid) &&
	echo "Backend stopped." &&
  rm "$LIBERTREE_PID_DIR"/backend.pid
}

stop_job_server(){
  echo "Stopping job processor..."
  kill -15 $(cat "$LIBERTREE_PID_DIR"/job.pid) &&
	echo "Job processor stopped." &&
  rm "$LIBERTREE_PID_DIR"/job.pid
}

stop_frontend_server(){
  echo "Stopping frontend..."
  kill -15 $(cat "$LIBERTREE_PID_DIR"/frontend.pid) &&
	echo "Frontend stopped." &&
  rm "$LIBERTREE_PID_DIR"/frontend.pid
}

stop_websocket_server(){
  echo "Stopping websocket server..."
  kill -15 $(cat "$LIBERTREE_PID_DIR"/websocket.pid) &&
	echo "Websocket server stopped." &&
  rm "$LIBERTREE_PID_DIR"/websocket.pid
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

if [ $# -gt 0 ]; then
  $1
else
  echo "${0} <start|stop>"
fi
