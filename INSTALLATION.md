The reference implementation of the Libertree components are available
as RPM and DEB packages.  We provide package repositories for Debian
Wheezy for 64-bit systems.  For other systems please see the
instructions in MANUAL-INSTALLATION.md.

# Pre-installation notes

Due to some deficiencies of our experimental packages, a user account
named `libertree` has to be created manually.

~~~
$ su -
$ adduser libertree
$ passwd libertree
~~~

The following steps are written under the assumption that you have
some system administration experience.  We intend to simplify this
process eventually, but some administration skills will always be
required.


# Debian Wheezy (64 bit)

Next, the experimental Debian repository is added to the list of apt
sources.  Finally, the two components (frontend and backend) are
installed.

~~~
$ su -
$ echo "deb http://repo.elephly.net/debian/ wheezy main" >> /etc/apt/sources.list
$ apt-get update
$ apt-get install libertree-{frontend,backend}
~~~


# Post-install configuration

The database will have to be configured next.  After initialising the
postgresql cluster and starting the postgresql server daemon, perform
the following steps:

## Database configuration

To simplify database administration, it is recommended to configure
PostgreSQL to trust local connections.  Please confirm that the
configuration file \`pg_hba.conf' contains the following lines:

    local  all  all       trust
    local  all  postgres  trust

If you see 'peer', 'ident', or 'md5' instead of 'trust', please change
the lines and reload the PostgreSQL server configuration (or restart
the postgresql daemon).  While this change is not required, it is
highly recommended because authentication often gets in the way.

Common locations of the pg_hba.conf file are:

* Debian & Ubuntu Server: /etc/postgresql/9.1/main/pg_hba.conf

* Fedora: /var/lib/pgsql/data/pg_hba.conf

Three simple steps need to be performed initially:

- create a database user "libertree"
- create the "libertree_production" database
- run the migration script

~~~
$ cd /usr/share/libertree/db
$ ./createuser.sh
$ ./createdb.sh
$ cp database.yaml.example database.yaml
$ LIBERTREE_ENV=production ./migrate.sh
~~~

Only the last step is required after updates to the `libertree-db`
package.


## Frontend configuration

To expose the frontend to the Internet, a proxying web server, such as
nginx or Apache, is recommended.  Under
`/usr/share/libertree/frontend-rb/config/` you can find an example
configuration files for an nginx setup (`libertree.nginx.example`) and
an Apache vhost definition (`libertree.apache.example`).


## Connecting the backend to an XMPP server

The Libertree backend contains an XMPP server component which is
responsible for communication with other Libertree installations.  The
Libertree backend should work with any standard XMPP server.  We
recommend Prosody, because it is easy to set up.  If you are using
Prosody, you may want to add the following snippet to the prosody
configuration:

~~~
Component "libertree.myserver.net"
    component_secret = "the-component-secret"
~~~

This will tell the XMPP server that a service listens at the domain
name "libertree.myserver.net" and that it will register with the given
shared secret.  Make sure that this secret is the same as the one
specified in the backend's configuration file.  Also, you need to make
sure that the backend is in fact configured to listen on the specified
domain name.


# Starting it all

Currently, the Libertree components do not come with a convenient
start-up script.  Here is a script that will start all components:

~~~
#!/bin/bash

# complain if run as root
if [ "$EUID" -eq 0 ]; then
  echo "Please do not run this as root."
  exit
fi

PREFIX=/usr/share/libertree
GEM_PATH=$PREFIX/gems

# start backend and job processor
cd $PREFIX/backend-rb
ruby -I./lib ./bin/server.rb config.yaml database.yaml &
ruby -I./lib ./bin/job-processor.rb config.yaml database.yaml &

# start frontend
cd $PREFIX/frontend-ramaze
$GEM_PATH/bin/unicorn -Ilib -c unicorn.rb config.ru &
cd -
~~~

Name this script `/usr/share/libertree/start.sh`, make it executable,
and run it as the `libertree` user to start all three Libertree
components.


# Updating

To update an existing installation just fetch the latest version from
the repository:

~~~
$ su -
$ apt-get update
$ apt-get upgrade
~~~

After upgrading any Libertree component, perform the following steps:

- compare the configuration files against the example files to see if
  they need updating
- run the migration script (`LIBERTREE_ENV=production
  /usr/share/libertree/db/migrate.sh`) to update the database schema.
- kill all Libertree processes and restart them (`su - libertree
  /usr/share/libertree/start.sh`)
