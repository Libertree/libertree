# Libertree Database - Setup and Maintenance

## Requirements

* PostgreSQL 8+

## Setup

Use createuser.sh to create the libertree PostgreSQL user.

Use createdb.sh to create the libertree PostgreSQL database for the development
and test environments, and set them to be owned by the libertree user.

You may need to set the PGHOST environment variable before running these.

Copy database.yaml.example to database.yaml .  Edit, if desired.

## Migration

To migrate your database use the `migrate.sh` script in the root directory.

    LIBERTREE_ENV=development ./migrate.sh

When adding new migrations, please prefix migration filenames with a UTC
timestamp of the format "YYYY-MM-DD-HHMM-", which indicates the approximate
time of the migration's creation.  For example:

    2012-03-19-1440-foobar-table.sql
