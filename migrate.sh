#!/bin/bash
set -o nounset # abort if we try to use an unset variable
set -o errexit # exit if any statement returns a non-true return value

CONFIG_FILE=${1:-database.yaml.example}


function parse_config 
{
  # - ignore commented lines
  # - get lines that belong to the specified section
  # - define variables

  local SECTION=${LIBERTREE_ENV:=development}
  local SAVEIFS=$IFS
  local SECTION_EXISTS="false"
  IFS=$'\n'

  for line in $(sed -ne '/^\s*#/d' -e "/^$SECTION/,/^[a-z]/{/^[a-z]/!p}" $CONFIG_FILE); do
    SECTION_EXISTS="true"
    IFS=" " read key value <<< $(echo $line | sed -e 's/[^a-z ]//')
    eval "libertree_db_$key=$value"
  done
  IFS=$SAVEIFS

  # bogus environment?
  if [ $SECTION_EXISTS = "false" ]; then
    echo "ERROR: the environment \"$SECTION\" is undefined"
    exit 1
  else
    echo "Loaded $SECTION environment"
  fi

  # ensure that all required connection variables are defined
  [ -n ${libertree_db_host:?"Database host undefined."} ]
  [ -n ${libertree_db_username:?"Database username undefined."} ]
  [ -n ${libertree_db_database:?"Database name undefined."} ]

  return 0
}

function ensure_migration_table_exists
{
  execute "SELECT 1 FROM pg_tables WHERE schemaname='public' AND tablename = 'schema_migrations'" | grep -q "1" || \
    execute "CREATE TABLE schema_migrations ( filename VARCHAR(1024) NOT NULL UNIQUE )"
}

function apply_migrations
{
  # apply migrations unless they exist
  for migration in $(find migrations -name \*.sql -printf '%f\n' | sort); do
    if migration_exists $migration; then
      echo "[SKIP] $migration"
    else
      ( psql -v ON_ERROR_STOP=1 --username $libertree_db_username -f "migrations/$migration" --single-transaction $libertree_db_database && execute "INSERT INTO schema_migrations ( filename ) VALUES ('$migration')") || { echo "ERROR: failed to apply migration \"$migration\"."; exit 1; }
    fi
  done
}

function execute
{
  # TODO: use password only if it is defined
  psql --tuples-only --no-align -h $libertree_db_host --username $libertree_db_username --dbname $libertree_db_database -c "$1"
}

function migration_exists
{
  execute "SELECT 1 FROM schema_migrations WHERE filename = '$1'" | grep -q "1"
}


# ---------------------------------------
parse_config
ensure_migration_table_exists
apply_migrations
# ---------------------------------------
