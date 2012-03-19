## Libertree Database - Setup and Maintenance

Use createuser.sh to create the libertree PostgreSQL user.  Use createdb.sh to
create the libertree PostgreSQL database and set it to be owned by the
libertree user.

Install Bundler (http://gembundler.com/ `gem install bundler`), then `bundle
install` to get required gems.  Migrate with:

    cd db/rb
    bundle exec ruby -Ilib migrate.rb [path to database.yaml]

### Migration

Use your choice of migration script to migrate your database.  These are found
in the following directories:

* rb/
* (more to come)

When adding new migrations, please prefix migration filenames with a UTC
timestamp of the format "YYYY-MM-DD-HHMM-", which indicates the approximate
time of the migration's creation.  For example:

    2012-03-19-1440-foobar-table.sql

