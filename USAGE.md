# Libertree Usage

This documentation assumes the directories and settings given in
INSTALLATION.md were followed.  If not, adapt the instructions below
accordingly.

## Startup

### System services

If necessary, start up the system services.

Start up the PostgreSQL server.  Under Gentoo:

    # /etc/init.d/postgresql-9.1 start

Start up the web server which will proxy requests.  This is either Nginx,
Apache or something equivalent.  Under Gentoo:

    # /etc/init.d/nginx start
    # # OR
    # /etc/init.d/apache2 start

If the Libertree installation is configured to use memcache, start memcached:

    # /etc/init.d/memcached start

### Ruby services

Libertree consists of two main components: the frontend and the backend.  In
addition to these two services, there is a background job processing script
for the backend.  The websocket server is optional.  Some of these
services might be served by more than one process or daemon.

#### Backend

As the libertree user:

    % cd ~/git/libertree-backend-rb
    % rvm use 2.0.0@libertree-backend-rb
    % LIBERTREE_ENV=production bundle exec ruby -Ilib bin/server.rb config.yaml

##### Processing of backend jobs

As the libertree user:

    % cd ~/git/libertree-backend-rb
    % rvm use 2.0.0@libertree-backend-rb
    % LIBERTREE_ENV=production bundle exec ruby bin/job-processor.rb config.yaml

#### Frontend

As the libertree user:

    % cd ~/git/libertree-frontend-ramaze
    % rvm use 2.0.0@libertree-frontend-ramaze
    % ./css-build.sh
    % LIBERTREE_ENV=production bundle exec unicorn -p <port number>


Use any port number desired.  The web server proxy will proxy requests from the
standard HTTP port (80) to this port.  If you run more than one frontend
(unicorn) process, you _must_ use memcached to store sessions, or else members
will not be able to remain logged in.

Optional (recommended) web socket server:

    % cd ~/git/libertree-frontend-ramaze
    % rvm use 2.0.0@libertree-frontend-ramaze
    % LIBERTREE_ENV=production bundle exec ruby websocket-server.rb

Note: to run processes in the background, add nohup and & to all the commands above, e.g.:

    % LIBERTREE_ENV=production nohup bundle exec ruby websocket-server.rb &

## Maintenance

Multiple job processing and frontend processes can be started, but almost all
new installations will not need to scale in this way for some time.

To update the server with new code updates, first update the code of each
repository:

    % cd ~/git/libertree-db
    % git fetch origin && git merge --ff-only @{u}
    % LIBERTREE_ENV=production ./migrate.sh
    % cd ~/git/libertree-backend-rb
    % git fetch origin && git merge --ff-only @{u}
    % cd ~/git/libertree-frontend-ramaze
    % git fetch origin && git merge --ff-only @{u}

Then restart all the services.  They can all be stopped by typing Ctrl-C. Check
for release notes which may describe new settings or migrations.  If there are
migrations, all daemons should be restarted, even if there were no changes in
some non-DB repositories.

If there are changes to a Gemfile.lock in a repo, then `bundle install` will have
to be run before restarting the daemons.

If there are SCSS changes, compile the SCSS to CSS:

    % cd ~/git/libertree-frontend-ramaze/contrib/
    % rvm use 2.0.0@libertree-frontend-ramaze
    % ./css-build.sh

This command is safe to run, even if it is not certain whether there were SCSS
changes.  SCSS files for all themes are automatically compiled whenever the
frontend is started.
