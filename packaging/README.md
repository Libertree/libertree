## Setting up the build environment

### System tools

- Debian: `apt-get install git build-essentials ruby`
- Fedora: `yum install git gcc ruby`

### Development headers

- Debian: `apt-get install libglib2.0-dev libmagic-dev libpq-dev libcurl4-openssl-dev`
- Fedora: `yum install glib2-devel file-devel postgresql-devel libcurl-devel`

Why?

- eventmachine: libssl-dev (Debian?) / openssl-devel (Fedora)
- parkdown: libglib2.0-dev (Debian) / glib2-devel (Fedora)
- ruby-filemagic: libmagic-dev (Debian) / file-devel (Fedora)
- pg: libpq-dev (Debian), postgresql-devel (Fedora)
- curb: libcurl4-openssl-dev (Debian) / libcurl-devel (Fedora)

### Optional headers

These headers are only required once we configure bundler to use
system libraries (e.g. with `bundle config build.nokogiri
--use-system-libraries`):

- libxml2 and libxslt headers for nokogiri
- gpgme headers

### Gems

- gem install fpm mini_portile bundler

Why?

- `fpm` is the tool that allows us to easily build rpms or debs from rubygems.
- `mini_portile` is a build-dependency of ruby-gpgme.
- `bundler` is needed to download all gems and resolve their dependencies.

### User setup

On Debian the packages are owned by the libertree user, which must exist.

    useradd libertree


## Building packages

~~~
cd packaging
./generate-makefile.sh
make PKG_TYPE=deb  # or make PKG_TYPE=rpm
~~~
