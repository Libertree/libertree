#!/bin/bash
set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export LIBERTREE_ENV=test
export LIBERTREE_DB=${LIBERTREE_DB:-libertree_test}

# TODO: Safeguard against dropping production DB.  Check for DB name "libertree".
dropdb -U postgres ${LIBERTREE_DB}
createdb -U postgres -O libertree ${LIBERTREE_DB}
${SCRIPT_DIR}/../../libertree-db/migrate.sh
bundle exec rspec --format nested "$@"
