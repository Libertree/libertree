# Libertree Installation

Following are specific, step by step instructions for installing Libertree.
Technology choices are assumed in order to maximize the detail provided in this
documentation, which in turn is done in order to make the instructions as
clear and easy to understand as possible.  Where there is leeway in choice of
technology, or where certain components are optional, this is noted.  Given
sufficient confidence and technical ability, other technologies can be
used instead to achieve a full, functional Libertree installation.

## Using the installer script

We provide an installer script that sets up Libertree according to the
following instructions. To use the installer,
[download the latest version](https://github.com/Libertree/libertree/archive/master.zip)
of this repository, unpack it and then run `./install` in the
`installer` subdirectory.  The installer is well-behaved and asks for
your permission at every important step.

If you are an expert system administrator and don't want to use the
installer, the instructions below are for you.


## System

It is recommended that the server on which Libertree is to be installed has at
least 384 MB of RAM.

It is assumed that certain core packages are already on the system, including
OpenSSL and a shell (bash, zsh, etc.).

The following components are runtime dependencies:

* PostgreSQL 8 or 9
* GraphicsMagick (or ImageMagick)
* Ruby >= 1.9
* an XMPP server (such as Prosody or ejabberd)


### Database configuration

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

Start up the PostgreSQL server daemon before attempting the
installation.


### Optional dependencies

If a web server proxy will be used (recommended), install it as well.  This
could be Apache, or Nginx, or anything equivalent.  Under Gentoo:

    # emerge -1atv nginx
    # # OR
    # emerge -1atv apache

Usage of memcached is recommended for persistent user sessions, but
not required.  (Note that it _is_ required if you intend to run more
than one frontend process.)

    # emerge -1atv memcached



### Ruby dependencies

Libertree uses the system Ruby package per default. If for some reason
you do not wish to use the system package, a Ruby version manager can
be used instead, such as [RVM](https://rvm.io/) or
[rbenv](https://github.com/sstephenson/rbenv).

There are Gemfile files in the repositories.  These are used by
[Bundler](http://gembundler.com/) to install and manage Ruby gems
(Ruby libraries).  Use of Bundler is optional; the required gems can
be managed in other ways, if desired.

Some gems have extensions that are compiled at installation time.
Compilation will abort if certain development headers or development
tools are missing.


### System user

An existing system user can be used, or a distinct user can be created for Libertree.

Create a system user for Libertree, as the root user:

    # useradd -m libertree

If desired, set a password for the libertree user, and configure SSH access,
shell startup scripts, and any other such preparations.

## Repositories

As the libertree user:

    % mkdir ~/git
    % cd ~/git
    % git clone git://github.com/Libertree/libertree-db.git
    % git clone git://github.com/Libertree/libertree-backend-rb.git
    % git clone git://github.com/Libertree/libertree-frontend-ramaze.git

## Database

As the libertree user:

    % cd ~/git/libertree-db
    % ./createuser.sh
    % ./createdb.sh
    % cp database.yaml.example database.yaml
    % LIBERTREE_ENV=production ./migrate.sh

If the above commands do not successfully connect to the database, you may need
to specify that PostgreSQL is listening on the localhost TCP socket.  Try again
after setting this:

    % export PGHOST=localhost

If a password is required for connecting to PostgreSQL, use a pgpass file:

  http://www.postgresql.org/docs/current/static/libpq-pgpass.html

It is recommended to ensure the created database is encoded with UTF-8.  This
can be checked with:

    % psql -l | grep libertree

If this shows an encoding other than what is desired, confirm the language
and locale settings of the shell/environment, or explicitly specify the
encoding when creating the database, like this:

    % createdb -U postgres -O libertree -E UTF8 --lc-collate=en_GB.UTF-8 --lc-ctype=en_GB.UTF-8 -T template0 libertree_production

### First member invitation

The Libertree software currently requires a valid invitation code to sign up.
Members can generate invitation URLs from the Settings page, but the very first
invitation needs to be created by hand:

    % echo 'INSERT INTO invitations DEFAULT VALUES; SELECT code FROM invitations ORDER BY id DESC LIMIT 1;' | psql -U libertree libertree_production

Then use the given code on the signup page.

### Admin account

After your first account is created (later, once all the daemons are running
(see USAGE.md)), set that account to be the admin user, like this:

    % echo "UPDATE accounts SET admin = 'true' WHERE username = 'youraccount';" | psql -U libertree libertree_production

## Backend

As the libertree user:

    % cd ~/git/libertree-backend-rb
    % gem install bundler
    % bundle install
    % ./generate-key-pair.sh
    % cp config.yaml.example config.yaml
    % ${EDITOR} config.yaml

Change every setting in the config.yaml file, providing values appropriate for
the installation.

Run the test suite:

    % bundle exec bin/test.sh

If the suite does not pass, the installation has some issues that need to be
worked out.

### Connecting Libertree to a local XMPP server

The Libertree backend is designed as an XMPP server component and
should work with any popular XMPP server implementation.  To add
Libertree services to your XMPP server refer to the section about
server components in your XMPP server's documentation.  Make sure that
the component secret and domain in the backend configuration file
match the values specified in your XMPP server's component
configuration.

Remember to change the component secret from "secret" to something
else.

This is an example component configuration for Prosody:

    Component "libertree.myserver.net"
    	component_secret = "the-component-secret"



## Frontend

As the libertree user:

    % cd ~/git/libertree-frontend-ramaze
    % gem install bundler
    % bundle install
    % cd config
    % cp database.yaml.example database.yaml
    % $EDITOR database.yaml
    % cp application.yaml.example application.yaml
    % $EDITOR application.yaml
    % mkdir -p public/images/avatars

## Web Server Proxy

Usage of a web server proxy is not required, but is recommended.  Without one,
Libertree members would have to browse to a non-standard port (such as
http://serverdomain.com:8088/ ), or, if the standard HTTP port (80) is used, the
frontend process would need to be run as root.

Any web server proxy will do.  Nginx or Apache would suffice.  Below are
example configuration snippets for them.

You may need to make your distro install specific modules for Nginx or Apache,
such as the proxy pass and proxy balancer modules.

You can find an example configuration file for Nginx at `libertree-frontend-ramaze/config/libertree.nginx.example`.

### Apache

    <VirtualHost *:80>
        ServerName example.com
        ServerAlias example.com
        ErrorLog /var/log/apache2/example.com.errors
        CustomLog /var/log/apache2/example.com.log combined

        <Proxy balancer://libertreecluster>
            BalancerMember http://127.0.0.1:8088
            # If running more than one unicorn process, add more BalancerMembers:
            # BalancerMember http://127.0.0.1:8089
            # BalancerMember http://127.0.0.1:8090
        </Proxy>

        ProxyPreserveHost On
        ProxyPass / balancer://libertreecluster/
        ProxyPassReverse / balancer://libertreecluster/

        <Directory "/path/to/libertree-frontend-ramaze/public">
            Options Indexes FollowSymLinks ExecCGI
            AllowOverride All
            Order allow,deny
            Allow from all
        </Directory>
    </VirtualHost>

You may need this config for proxy balancer, depending on your distro and setup:

    <IfModule mod_proxy_balancer.c>
        <Proxy *>
            Order deny,allow
            Allow from all
        </Proxy>
    </IfModule>
