# Libertree Installation

Following are specific, step by step instructions for installing Libertree.
Technology choices are assumed in order to maximize the detail provided in this
documentation, which in turn is done in order to make the instructions as
clear and easy to understand as possible.  Where there is leeway in choice of
technology, or where certain components are optional, this is noted.  Given
sufficient confidence and technical ability, other technologies can be
used instead to achieve a full, functional Libertree installation.

## System

It is recommended that the server on which Libertree is to be installed has at
least 384 MB of RAM.

It is assumed that certain core packages are already on the system, including
OpenSSL and a shell (bash, zsh, etc.).

Install the required system packages:

* PostgreSQL 8 or 9
* GraphicsMagick (or ImageMagick)
* git

Under Gentoo Linux, this is done with this command as the root user:

    # emerge -1atv postgresql-server graphicsmagick git

Configure PostgreSQL as desired.  The defaults should work, though.
Start up the PostgreSQL server.  Under Gentoo:

    # /etc/init.d/postgresql-9.1 start

If a web server proxy will be used (recommended), install it as well.  This
could be Apache, or Nginx, or anything equivalent.  Under Gentoo:

    # emerge -1atv nginx
    # # OR
    # emerge -1atv apache

Usage of memcached is recommended, but not required:

    # emerge -1atv memcached

### Ruby

While it is possible to use the system Ruby package, it is recommended that a
Ruby version manager be used instead, such as
[RVM](https://rvm.beginrescueend.com/) or
[rbenv](https://github.com/sstephenson/rbenv).

Install RVM using the installation instructions found on
[the RVM website](https://rvm.beginrescueend.com/) (one simple line at the time
of this writing).

Once RVM is installed (including configuration of your shell startup script),
install Ruby 1.9 as follows:

    % rvm install 1.9.3

There are Gemfile files in the repositories.  These are used by
[Bundler](http://gembundler.com/) to install and manage Ruby gems (Ruby
libraries).  Use of Bundler is optional; the required gems can be managed in
other ways, if desired.

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
    % git clone git://github.com/Libertree/libertree-client-rb.git
    % git clone git://github.com/Libertree/libertree-frontend-ramaze.git

## Database

As the libertree user:

    % cd ~/git/libertree-db
    % export PGHOST=localhost
    % ./createuser.sh
    % ./createdb.sh
    % cp database.yaml.example database.yaml
    % cd rb
    % rvm use --create 1.9.3@libertree-db
    % gem install bundler
    % bundle install
    % LIBERTREE_ENV=production ./migrate.sh

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

    % echo 'INSERT INTO invitations DEFAULT VALUES; SELECT code FROM invitations ORDER BY id DESC LIMIT 1;' | psql -U libertree libertree_development

Then use the given code on the signup page.

## Backend

As the libertree user:

    % cd ~/git/libertree-backend-rb
    % rvm use --create 1.9.3@libertree-backend-rb
    % gem install bundler
    % bundle install
    % bundle exec ruby generate-key-pair.rb
    % cp config.yaml.example config.yaml
    % ${EDITOR} config.yaml

Change every setting in the config.yaml file, providing values appropriate for
the installation.  The "forest" setting can be left alone to start with.

Run the test suite:

    % bundle exec bin/test.sh

If the suite does not pass, the installation has some issues that need to be
worked out.

Start the backend processes:

    % bundle exec ruby -Ilib bin/server.rb config.yaml > server.log &
    % bundle exec ruby -Ilib bin/job-processor.rb config.yaml > worker.log &

## Frontend

As the libertree user:

    % cd ~/git/libertree-frontend-ramaze
    % rvm use --create 1.9.3@libertree-frontend-ramaze
    % gem install bundler
    % bundle install
    % ./css-build.sh
    % cd config
    % cp database.yaml.example database.yaml
    % $EDITOR database.yaml
    % cp application.yaml.example application.yaml
    % $EDITOR application.yaml

## Web Server Proxy

Usage of a web server proxy is not required, but is recommended.  Without one,
Libertree members would have to browse to a non-standard port (such as
http://serverdomain.com:8088/ ), or, if the standard HTTP port (80) is used, the
frontend process would need to be run as root.

Any web server proxy will do.  Nginx or Apache would suffice.  Below are
example configuration snippets for them.

### Nginx

    upstream unicorn_cluster {
      server localhost:8088;
      # If running more than one unicorn process, add more servers:
      # server localhost:8089;
      # server localhost:8090;
    }

    server {

      listen       80;
      server_name  serverdomain.com;
      root         /path/to/libertree-frontend-ramaze/public;

      location / {
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Host $http_host;
        proxy_redirect off;
        proxy_read_timeout 90s;

        client_max_body_size 5M;
        client_body_buffer_size 128K;

        if (!-f $request_filename) {
          proxy_pass http://unicorn_cluster;
          break;
        }
      }

    }

### Apache

    <VirtualHost *:80>
        ServerName serverdomain.com
        ServerAlias serverdomain.com
        ErrorLog /var/log/apache2/serverdomain.com.errors
        CustomLog /var/log/apache2/serverdomain.com.log combined

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
