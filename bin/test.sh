#!/bin/bash
set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export LIBERTREE_ENV=test
export LIBERTREE_DB=${LIBERTREE_DB:-libertree_test}

# TODO: Safeguard against dropping production DB.  Check for DB name "libertree".
dropdb -U postgres ${LIBERTREE_DB}
createdb -U postgres -O libertree ${LIBERTREE_DB}
echo 'CREATE EXTENSION "uuid-ossp";' | psql -U postgres ${LIBERTREE_DB}
bundle exec ruby ${SCRIPT_DIR}/../../libertree-db/rb/migrate.rb
bundle exec rspec "$@"
