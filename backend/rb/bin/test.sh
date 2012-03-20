#!/bin/bash
set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_DIR="${SCRIPT_DIR}/../../.."
export LIBERTREE_ENV=test
export LIBERTREE_DB=${LIBERTREE_DB:-libertree_test}

dropdb -U postgres ${LIBERTREE_DB}
createdb -U postgres -O libertree ${LIBERTREE_DB}
bundle exec ruby -I${PROJECT_DIR}/db/rb/lib ${PROJECT_DIR}/db/rb/migrate.rb
bundle exec rspec -I${PROJECT_DIR}/db/rb/lib "$@"
