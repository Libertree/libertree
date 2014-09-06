# TODO:
#   - add support for frontend
#   - non-gem builds have no dependencies (BUILDPKG and libertree-backend)
#   - add architecture ("-a all" or "-a native")

PREFIX = /usr/share/libertree
PREFIX_BACKEND = $(addprefix $(PREFIX)/,backend-rb)
PREFIX_GEMS = $(addprefix $(PREFIX)/,gems)
PKG_PREFIX = libertree

VERSION_BACKEND = 0.0.1
VERSION_MODEL   = 0.9.1
VERSION_CLIENT  = 0.0.3

GEMDIR = vendor/cache
PKG_TYPE = rpm
#PKG_TYPE = deb

# Convert a ruby gem to a deb/rpm package
# The name of the resulting package will be prefixed with "libertree-" (PKG_PREFIX)
# $(1) : the directory containing the source gem
# $(2) : additional parameters to fpm (e.g. package name, version, etc)
MKPACKAGE = fpm -d ruby -d rubygems $(2) \
		--prefix $(PREFIX_GEMS) --gem-package-name-prefix $(PKG_PREFIX) \
		--directories . \
		--gem-bin-path $(addprefix $(PREFIX_GEMS)/,bin) \
		-s gem -t $(PKG_TYPE) $(addprefix $(GEMDIR)/,$(1))

# Build a deb/rpm package from a libertree gem
# Dependencies are set according to the gemspec file.
# $(1) : gem file name
# $(2) : version
# $(3) : path of generated package
# $(4) : package name
BUILDPKG = fpm -d ruby -d rubygems \
		--prefix $(PREFIX_GEMS) \
		--gem-package-name-prefix $(PKG_PREFIX) \
		--no-gem-fix-name \
		--gem-fix-dependencies \
		--maintainer "rekado+libertree@elephly.net" \
		--version $(2) \
		--license AGPL3 \
		--directories . \
		--gem-bin-path $(addprefix $(PREFIX_GEMS)/,bin) \
		-p $(3) \
		-n $(4) \
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
		-C ../libertree-backend-rb \
		--directories . \
		-s dir -t $(PKG_TYPE) -n libertree-backend \
		lib/ bin/server.rb bin/job-processor.rb generate-key-pair.sh \
		database.yaml.example \
		config.yaml.example
#		--config-files database.yaml.example \
#		--config-files config.yaml.example \


# generate this list from Gemfile.lock with the following command:
# egrep -e '^    [a-z]+' Gemfile.lock | sed -e 's/^    //' -e 's/ (/-/' -e 's/)$//' | egrep -v -e '^libertree-' 

gems : Gemfile.lock regular_gems gems_with_extensions
regular_gems : $(addprefix $(PKG_PREFIX)-,$(addsuffix .$(PKG_TYPE),activesupport-4.1.5 \
	blather-1.0.0 \
	connection_pool-1.2.0 \
	em-websocket-0.5.1 \
	girl_friday-0.11.2 \
	http_parser.rb-0.6.0 \
	i18n-0.6.11 \
	mail-2.6.1 \
	mail-gpg-0.2.2 \
	mime-types-2.3 \
	mini_portile-0.6.0 \
	minitest-5.4.1 \
	net-ldap-0.5.1 \
	niceogiri-1.1.2 \
	rubinius-actor-0.0.2 \
	ruby-oembed-0.8.10 \
	sequel-4.14.0 \
	thread_safe-0.3.4 \
	timedcache-0.4.0 \
	tzinfo-1.2.2))

gems_with_extensions : $(addprefix $(PKG_PREFIX)-,$(addsuffix .$(PKG_TYPE),bcrypt-ruby-3.0.1 \
	eventmachine-1.0.3 \
	gpgme-2.0.6 \
	json-1.8.1 \
	nokogiri-1.6.3.1 \
	parkdown-1.4.23 \
	pg-0.17.1 \
	rubinius-core-api-0.0.1))


libertree-client-$(VERSION_CLIENT).gem : ../libertree-client-rb/libertree-client.gemspec
	gem build $<

libertree-client-$(VERSION_CLIENT).$(PKG_TYPE) : libertree-client-$(VERSION_CLIENT).gem
	$(call BUILDPKG, $<, $(VERSION_CLIENT), $@, libertree-client)

# TODO: specify database.yaml.example as config file
# TODO: share database.yaml with other packages?
libertree-model-$(VERSION_MODEL).gem : ../libertree-model-rb/libertree-model.gemspec
	gem build $<

libertree-model-$(VERSION_MODEL).$(PKG_TYPE) : libertree-model-$(VERSION_MODEL).gem
	$(call BUILDPKG, $<, $(VERSION_MODEL), $@, libertree-model)


####################################
# DEPENDENT GEMS WITH EXTENSIONS
####################################

# needs OpenSSL library
$(PKG_PREFIX)-eventmachine-%.$(PKG_TYPE) : $(GEMDIR)/eventmachine-%.gem
	$(call MKPACKAGE, eventmachine-$*.gem, -p $@ -d openssl-libs)

# needs libxml2 libxslt
$(PKG_PREFIX)-nokogiri-%.$(PKG_TYPE) : $(GEMDIR)/nokogiri-%.gem
	$(call MKPACKAGE, nokogiri-$*.gem, -p $@ -d libxml2 -d libxslt)

# needs gpgme
$(PKG_PREFIX)-gpgme-%.$(PKG_TYPE) : $(GEMDIR)/gpgme-%.gem
ifeq ($(PKG_TYPE), rpm)
	$(call MKPACKAGE, gpgme-$*.gem, -p $@ -d gpgme)
else
	$(call MKPACKAGE, gpgme-$*.gem, -p $@ -d libgpgme11)
endif

# TODO: does this depend on a system library?
$(PKG_PREFIX)-bcrypt-ruby-%.$(PKG_TYPE) : $(GEMDIR)/bcrypt-ruby-%.gem
	$(call MKPACKAGE, bcrypt-ruby-3.0.1.gem, -p $@)

$(PKG_PREFIX)-pg-%.$(PKG_TYPE) : $(GEMDIR)/pg-%.gem
ifeq ($(PKG_TYPE), rpm)
	$(call MKPACKAGE, pg-$*.gem, -p $@ -d postgresql-libs)
else
	$(call MKPACKAGE, pg-$*.gem, -p $@ -d libpq5)
endif

# standard rule for gems that are converted to rpm/deb
$(PKG_PREFIX)-%.$(PKG_TYPE) : $(GEMDIR)/%.gem
	$(call MKPACKAGE, $*.gem, -p $@)

clean :
	rm -rf ./packages/
	rm -rf *.$(PKG_TYPE)

cleanall : clean
	rm Gemfile*
	rm -rf $(GEMDIR)
	rm -rf libertree-client-$(VERSION_CLIENT).gem
	rm -rf libertree-model-$(VERSION_MODEL).gem

nuke : cleanall
	rm -rf vendor

Gemfile:
	ruby build_gemfile.rb > Gemfile

# Download all gems listed in the Gemfile
# and place them in the cache in vendor/cache
prepare : Gemfile
	bundle pack --all

rebuild: cleanall libertree-backend-$(VERSION_BACKEND).$(PKG_TYPE)

.PHONY: prepare clean cleanall nuke rebuild
