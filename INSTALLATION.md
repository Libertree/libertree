The reference implementation of the Libertree components are available
as RPM and DEB packages.  We provide package repositories for Debian
Wheezy for 64-bit systems.  For other systems please see the
instructions in MANUAL-INSTALLATION.md.

# Debian Wheezy (64 bit)

Due to some deficiencies of our experimental packages, a user account
named `libertree` has to be created manually.  Then the experimental
Debian repository is added to the list of apt sources.  Finally, the
two components (frontend and backend) are installed.

~~~
$ su -
$ adduser libertree
$ passwd libertree
$ echo "deb http://repo.elephly.net/debian/ wheezy main" >> /etc/apt/sources.list
$ apt-get update
$ apt-get install libertree-{frontend,backend}
~~~


# CentOS 7 (64 bit)

