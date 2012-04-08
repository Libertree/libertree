# Libertree

The Libertree Project is a community of people committed to empowering the
people of the world to do things on and with computers, without need for or
interference from big business, by way of providing software and other tools
which can be obtained, bought or built with minimal cost and effort.

Libertree is free, libre, open-source software which is intended to provide a
way for people to create their own social network.  Libertree social networks
can be free from commercial influence and manifestation, such as behaviour
tracking, user profiling, advertising, data mining and analysis, and covert
information filtering.

The remainder of the documentation found in this repository refers to the
Libertree social network software, and not The Libertree Project as a whole,
unless otherwise explicitly stated.

## Reference Implementation

There are (will be) several implementations of the various components of
Libertree.  The first implementation, the reference implementation, is
written in Ruby.

### Requirements

* A UNIX-compatible system (such as GNU/Linux or OSX)
* PostgreSQL 9
* Ruby 1.9
* GraphicsMagick (or ImageMagick)
* git

### Recommended

* A web server or other proxy (such as Nginx or Apache)

### Components

The components of Libertree are individual subprojects, each having its own
source code repository.

* libertree-db
* libertree-backend-rb
* libertree-client-rb
* libertree-frontend-ramaze

## Installation

Refer to the INSTALLATION.md file.

## Usage

Refer to the USAGE.md file for information on how to start the Libertree
services and how to maintain a Libertree server.
