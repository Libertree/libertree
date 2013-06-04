PREFIX = /usr/share/libertree/gems
PKG_PREFIX = libertree
VERSION = 0.0.2

GEMDIR := vendor/cache

MKPACKAGE = fpm -d ruby -d rubygems $(3) \
		--prefix $(PREFIX) --gem-package-name-prefix $(PKG_PREFIX) \
		-s gem -t $(1) $(addprefix $(GEMDIR)/,$(2))

all: libertree-client-$(VERSION).gem gems move

move : gems
	mkdir -p ./packages/{rpm,deb}
	mv *.rpm ./packages/rpm/
	mv *.deb ./packages/deb/

gems : activesupport-3.2.2.gem \
	blather-0.8.1.gem \
	connection_pool-0.9.2.gem \
	girl_friday-0.11.1.gem \
	i18n-0.6.0.gem \
	multi_json-1.1.0.gem \
	niceogiri-1.1.1.gem \
	eventmachine-0.12.10.gem \
	nokogiri-1.5.5.gem

# TODO: add all built gem packages as dependencies
BUILDPKG = fpm -d ruby -d rubygems \
		--prefix $(PREFIX) \
		--gem-package-name-prefix $(PKG_PREFIX) \
		--maintainer "rekado+libertree@elephly.net" \
		--version $(VERSION) \
		--license AGPL3 \
		-s gem -t $(1) $(2)

libertree-client-$(VERSION).gem : FORCE
	gem build libertree-client.gemspec
	$(call BUILDPKG, rpm, $@)
	$(call BUILDPKG, deb, $@)

# standard rule for dependent gems
%.gem : | prepare
	$(call MKPACKAGE, rpm, $@)
	$(call MKPACKAGE, deb, $@)

# needs OpenSSL headers
eventmachine-0.12.10.gem : | prepare
	$(call MKPACKAGE, rpm, $@, -d libssl)
	$(call MKPACKAGE, deb, $@, -d libssl)

# needs libxml2 libxslt
nokogiri-1.5.5.gem : | prepare
	$(call MKPACKAGE, rpm, $@, -d libxml2 -d libxslt)
	$(call MKPACKAGE, deb, $@, -d libxml2 -d libxslt)

clean :
	rm -rf ./packages/
	rm -rf *.rpm *.deb

cleanall : clean
	rm -rf $(GEMDIR)
	rm -rf libertree-client-$(VERSION).gem

prepare :
	bundle install --without development
	bundle pack

rebuild: cleanall all

.PHONY: prepare clean cleanall rebuild
FORCE :
