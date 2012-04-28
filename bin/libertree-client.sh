#!/bin/bash
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
bundle exec ruby -I"${SCRIPT_DIR}/../lib" -I"${SCRIPT_DIR}/../../libertree-db/rb/lib" ${SCRIPT_DIR}/libertree-client.rb ${SCRIPT_DIR}/../../libertree-backend-rb/private.key
