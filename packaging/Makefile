# TODO:
#   - add support for frontend
#   - non-gem builds have no dependencies (BUILDPKG and libertree-backend)
#   - directories in /usr/share/libertree are not removed on uninstall
#   - all targets are forced. Try to be smart and only build what's needed

PREFIX = /usr/share/libertree
PREFIX_BACKEND = $(addprefix $(PREFIX)/,backend-rb)
PREFIX_GEMS = $(addprefix $(PREFIX)/,gems)
PKG_PREFIX = libertree

VERSION_BACKEND = 0.0.1
VERSION_MODEL   = 0.0.1
VERSION_CLIENT  = 0.0.2

GEMDIR = vendor/cache
PKG_TYPE = rpm # or deb

# Convert a ruby gem to a deb/rpm package
# The name of the resulting package will be prefixed with "libertree-" (PKG_PREFIX)
# $(1) : the directory containing the source gem
# $(2) : optional deb/rpm dependencies for the built package
MKPACKAGE = fpm -d ruby -d rubygems $(2) \
		--prefix $(PREFIX_GEMS) --gem-package-name-prefix $(PKG_PREFIX) \
		-s gem -t $(PKG_TYPE) $(addprefix $(GEMDIR)/,$(1))

# Create a deb/rpm package from an unpacked ruby gem.
# $(1) : ?
# $(2) : ?
# $(3) : optional deb/rpm dependencies for the built package
WRAPGEMDIR = fpm -d ruby -d rubygems $(3) \
		--prefix $(PREFIX_GEMS) \
		-s dir -t $(PKG_TYPE) -n $(addprefix $(PKG_PREFIX)-,$(2)) $(addprefix $(GEMDIR)/,$(1))

# Build a deb/rpm package from a libertree gem
# Dependencies are set according to the gemspec file.
# $(1) : gem file name
# $(2) : version
BUILDPKG = fpm -d ruby -d rubygems \
		--prefix $(PREFIX_GEMS) \
		--gem-package-name-prefix $(PKG_PREFIX) \
		--maintainer "rekado+libertree@elephly.net" \
		--version $(2) \
		--license AGPL3 \
		-s gem -t $(PKG_TYPE) $(1)

all: gems \
	libertree-client-$(VERSION_CLIENT).gem \
	libertree-model-$(VERSION_MODEL).gem \
	libertree-backend
	mkdir -p ./packages/$(PKG_TYPE)
	mv *.$(PKG_TYPE) ./packages/$(PKG_TYPE)

gems : regular_gems gems_with_extensions
regular_gems : activesupport-3.2.13.gem \
	blather-0.8.5.gem \
	connection_pool-1.0.0.gem \
	epoxy-0.3.1.gem \
	girl_friday-0.11.2.gem \
	i18n-0.6.1.gem \
	m4dbi-0.8.8.gem \
	mail-2.5.4.gem \
	metaid-1.0.gem \
	methlab-0.1.0.gem \
	mime-types-1.23.gem \
	multi_json-1.7.6.gem \
	niceogiri-1.1.2.gem \
	polyglot-0.3.3.gem \
	pony-1.4.1.gem \
	rdbi-6b4103aac8e2 \
	rdbi-driver-postgresql-be571a9909f7 \
	rubinius-actor-0.0.2.gem \
	treetop-1.4.14.gem \
	typelib-0.1.0.gem

gems_with_extensions : bcrypt-ruby-3.0.1.gem \
	eventmachine-0.12.10.gem \
	json-1.8.0.gem \
	nokogiri-1.5.5.gem \
	pg-0.9.0.gem \
	rubinius-core-api-0.0.1.gem

# TODO: share database config file?
# TODO: add built rpms/debs as dependencies
libertree-backend : FORCE
	fpm -d ruby -d rubygems \
		-d $(addprefix $(PKG_PREFIX)-,libertree-model) \
		-d $(addprefix $(PKG_PREFIX)-,libertree-client) \
		--prefix $(PREFIX_BACKEND) \
		--url "http://libertreeproject.org" \
		--maintainer "rekado+libertree@elephly.net" \
		--version $(VERSION_BACKEND) \
		--license AGPL3 \
		--config-files database.yaml.example \
		--config-files config.yaml.example \
		-C ../../libertree-backend-rb \
		-s dir -t $(PKG_TYPE) -n $(addprefix $(PKG_PREFIX)-,$@) \
		lib/ bin/server.rb bin/job-processor.rb generate-key-pair.rb \
		database.yaml.example \
		config.yaml.example

libertree-client-$(VERSION_CLIENT).gem : FORCE
	cd ../../libertree-client-rb; gem build libertree-client.gemspec
	mv $(addprefix ../../libertree-client-rb/,$@) .
	$(call BUILDPKG, $@, $(VERSION_CLIENT))

# TODO: specify database.yaml.example as config file
# TODO: share database.yaml with other packages?
libertree-model-$(VERSION_MODEL).gem : FORCE
	cd ../../libertree-model-rb; gem build libertree-model.gemspec
	mv $(addprefix ../../libertree-model-rb/,$@) .
	$(call BUILDPKG, $@, $(VERSION_MODEL))

# needs OpenSSL library
eventmachine-0.12.10.gem : | prepare
	$(call MKPACKAGE, $@, -d openssl-libs)

# needs libxml2 libxslt
nokogiri-1.5.5.gem : | prepare
	$(call MKPACKAGE, $@, -d libxml2 -d libxslt)

# this is different because we use the git version
# first build the gem, then package it
rdbi-% : | prepare
	cd $(addprefix $(GEMDIR)/,$@); gem build rdbi.gemspec
	$(call MKPACKAGE, $(addprefix $@/,rdbi-0.9.1.gem), -v $*)

# this is different because we use the git version
# first build the gem, then package it
rdbi-driver-postgresql-% : | prepare
	cd $(addprefix $(GEMDIR)/,$@); gem build rdbi-driver-postgresql.gemspec
	$(call MKPACKAGE, $(addprefix $@/,rdbi-driver-postgresql-0.9.1.gem), -v $*)

# TODO: does this depend on a system library?
bcrypt-ruby-3.0.1.gem : | prepare
	$(call MKPACKAGE, $@)

pg-0.9.0.gem : | prepare
ifeq ($(PKG_TYPE), rpm)
	$(call MKPACKAGE, $@, -d postgresql-libs)
else
	$(call MKPACKAGE, $@, -d libpq5)
endif

# standard rule for dependent gems
%.gem : | prepare
	$(call MKPACKAGE, $@)

clean :
	rm -rf ./packages/
	rm -rf *.$(PKG_TYPE)

cleanall : clean
	rm -rf $(GEMDIR)
	rm -rf libertree-client-$(VERSION_CLIENT).gem
	rm -rf libertree-model-$(VERSION_MODEL).gem

nuke : cleanall
	rm -rf vendor

# Download all gems listed in the Gemfile
# and place them in the cache in vendor/cache
prepare :
	bundle install --without development --path vendor
	bundle pack --all

rebuild: cleanall all

.PHONY: prepare clean cleanall nuke rebuild
FORCE :
