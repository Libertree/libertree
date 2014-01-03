# TODO:
#   - add support for frontend
#   - non-gem builds have no dependencies (BUILDPKG and libertree-backend)
#   - directories in /usr/share/libertree are not removed on uninstall

PREFIX = /usr/share/libertree
PREFIX_BACKEND = $(addprefix $(PREFIX)/,backend-rb)
PREFIX_GEMS = $(addprefix $(PREFIX)/,gems)
PKG_PREFIX = libertree

VERSION_BACKEND = 0.0.1
VERSION_MODEL   = 0.5.0
VERSION_CLIENT  = 0.0.2

GEMDIR = vendor/cache
PKG_TYPE = rpm # or deb

# Convert a ruby gem to a deb/rpm package
# The name of the resulting package will be prefixed with "libertree-" (PKG_PREFIX)
# $(1) : the directory containing the source gem
# $(2) : additional parameters to fpm (e.g. package name, version, etc)
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
		-p $(3) \
		-s gem -t $(PKG_TYPE) $(addprefix ./,$(1))


#############
# RULES
#############

# TODO: share database config file?
# TODO: add built rpms/debs as dependencies
# TODO: if config files are included fpm fails
libertree-backend-$(VERSION_BACKEND).$(PKG_TYPE) : gems \
	libertree-client-$(VERSION_CLIENT).$(PKG_TYPE) \
	libertree-model-$(VERSION_MODEL).$(PKG_TYPE)
	fpm -d ruby -d rubygems \
		-d libertree-model \
		-d libertree-client \
		--prefix $(PREFIX_BACKEND) \
		--url "http://libertreeproject.org" \
		--maintainer "rekado+libertree@elephly.net" \
		--version $(VERSION_BACKEND) \
		--license AGPL3 \
		-p $@ \
		-C ../../libertree-backend-rb \
		-s dir -t $(PKG_TYPE) -n $(addprefix $(PKG_PREFIX)-,$@) \
		lib/ bin/server.rb bin/job-processor.rb generate-key-pair.sh \
		database.yaml.example \
		config.yaml.example
#		--config-files database.yaml.example \
#		--config-files config.yaml.example \


gems : prepare regular_gems gems_with_extensions unpacked_gems
regular_gems : $(addprefix $(PKG_PREFIX)-,$(addsuffix .$(PKG_TYPE),activesupport-3.2.13 \
	blather-0.8.5 \
	connection_pool-1.0.0 \
	epoxy-0.3.1 \
	girl_friday-0.11.2 \
	i18n-0.6.1 \
	m4dbi-0.8.8 \
	mail-2.5.4 \
	metaid-1.0 \
	methlab-0.1.0 \
	mime-types-1.23 \
	multi_json-1.7.6 \
	niceogiri-1.1.2 \
	polyglot-0.3.3 \
	pony-1.4.1 \
	rubinius-actor-0.0.2 \
	treetop-1.4.14 \
	typelib-0.1.0))

unpacked_gems : rdbi-6b4103aac8e2 \
	rdbi-driver-postgresql-be571a9909f7

gems_with_extensions : $(addprefix $(PKG_PREFIX)-,$(addsuffix .$(PKG_TYPE),bcrypt-ruby-3.0.1 \
	eventmachine-1.0.3 \
	json-1.8.0 \
	nokogiri-1.5.5 \
	pg-0.9.0 \
	rubinius-core-api-0.0.1))


# TODO: add libertree-client.gemspec as prerequisite?
libertree-client-$(VERSION_CLIENT).gem :
	cd ../../libertree-client-rb; gem build libertree-client.gemspec
	mv $(addprefix ../../libertree-client-rb/,$@) .

libertree-client-$(VERSION_CLIENT).$(PKG_TYPE) : libertree-client-$(VERSION_CLIENT).gem
	$(call BUILDPKG, libertree-client-$(VERSION_CLIENT).gem, $(VERSION_CLIENT), $@)

# TODO: specify database.yaml.example as config file
# TODO: share database.yaml with other packages?
libertree-model-$(VERSION_MODEL).gem :
	cd ../../libertree-model-rb; gem build libertree-model.gemspec
	mv $(addprefix ../../libertree-model-rb/,$@) .

libertree-model-$(VERSION_MODEL).$(PKG_TYPE) : libertree-model-$(VERSION_MODEL).gem
	$(call BUILDPKG, libertree-model-$(VERSION_MODEL).gem, $(VERSION_MODEL), $@)


####################################
# DEPENDENT GEMS WITH EXTENSIONS
####################################

# needs OpenSSL library
$(PKG_PREFIX)-eventmachine-1.0.3.$(PKG_TYPE) : | prepare
	$(call MKPACKAGE, eventmachine-1.0.3.gem, -p $(PKG_PREFIX)-eventmachine.$(PKG_TYPE) -d openssl-libs)

# needs libxml2 libxslt
$(PKG_PREFIX)-nokogiri-1.5.5.$(PKG_TYPE) : | prepare
	$(call MKPACKAGE, nokogiri-1.5.5.gem, -p $(PKG_PREFIX)-nokogiri.$(PKG_TYPE) -d libxml2 -d libxslt)

# this is different because we use the git version
# firstp build the gem, then package it
rdbi-% : | prepare
	cd $(addprefix $(GEMDIR)/,$@); gem build rdbi.gemspec
	$(call MKPACKAGE, $(addprefix $@/,rdbi-0.9.1.gem), -v $*)

# this is different because we use the git version
# first build the gem, then package it
rdbi-driver-postgresql-% : | prepare
	cd $(addprefix $(GEMDIR)/,$@); gem build rdbi-driver-postgresql.gemspec
	$(call MKPACKAGE, $(addprefix $@/,rdbi-driver-postgresql-0.9.1.gem), -v $*)

# TODO: does this depend on a system library?
$(PKG_PREFIX)-bcrypt-ruby-3.0.1.$(PKG_TYPE) : | prepare
	$(call MKPACKAGE, bcrypt-ruby-3.0.1.gem, -p $(PKG_PREFIX)-bcrypt-ruby-3.0.1.$(PKG_TYPE))

$(PKG_PREFIX)-pg-0.9.0.$(PKG_TYPE) : | prepare
ifeq ($(PKG_TYPE), rpm)
	$(call MKPACKAGE, $@, -d postgresql-libs)
else
	$(call MKPACKAGE, $@, -d libpq5)
endif

# standard rule for gems that are converted to rpm/deb
# TODO: adding gem as prerequisite fails
$(PKG_PREFIX)-%.$(PKG_TYPE) :
	$(call MKPACKAGE, $*.gem, -p $@)

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
	bundle pack --all

rebuild: cleanall libertree-backend-$(VERSION_BACKEND).$(PKG_TYPE)

.PHONY: prepare clean cleanall nuke rebuild
