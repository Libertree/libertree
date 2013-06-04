BACKEND_PREFIX = /usr/share/libertree/backend-rb
GEM_PREFIX = /usr/share/libertree/gems
PKG_PREFIX = libertree
VERSION = 0.0.1

GEMDIR := vendor/cache

MKPACKAGE = fpm -d ruby -d rubygems $(3) \
		--prefix $(GEM_PREFIX) --gem-package-name-prefix $(PKG_PREFIX) \
		-s gem -t $(1) $(addprefix $(GEMDIR)/,$(2))

all: libertree-backend gems move

move : gems
	mkdir -p ./packages/{rpm,deb}
	mv *.rpm ./packages/rpm/
	mv *.deb ./packages/deb/

gems : json-1.8.0.gem \
	mime-types-1.19.gem \
	polyglot-0.3.3.gem \
	treetop-1.4.10.gem \
	mail-2.4.4.gem \
	pony-1.4.gem

# shared dependencies with libertree-client-rb:
#* i18n-0.6.0.gem
#* eventmachine-0.12.10.gem
#* connection_pool-0.9.2.gem
#* girl_friday-0.11.1.gem
#* nokogiri-1.5.5.gem
#* niceogiri-1.1.1.gem
#* blather-0.8.1.gem
#* multi_json-1.5.1.gem     # different version!
#* activesupport-3.2.12.gem # different version!

# shared dependencies with libertree-model-rb:
#* bcrypt-ruby-3.0.1.gem
#* epoxy-0.3.1.gem
#* metaid-1.0.gem
#* methlab-0.1.0.gem
#* typelib-0.1.0.gem
#* m4dbi-0.8.7.gem
#* pg-0.9.0.gem
#* rdbi-*

# TODO: share database config file?
BUILDBACKEND = fpm -d ruby -d rubygems \
		-d $(addprefix $(PKG_PREFIX)-,libertree-model) \
		-d $(addprefix $(PKG_PREFIX)-,libertree-client) \
		--prefix $(BACKEND_PREFIX) \
		--url "http://libertreeproject.org" \
		--maintainer "rekado+libertree@elephly.net" \
		--version $(VERSION) \
		--license AGPL3 \
		--config-files database.yaml.example \
		--config-files config.yaml.example \
		-s dir -t $(1) -n $(addprefix $(PKG_PREFIX)-,$@) \
		lib/ bin/server.rb bin/job-processor.rb generate-key-pair.rb \
		database.yaml.example \
		config.yaml.example

libertree-backend : FORCE
	$(call BUILDBACKEND, rpm)
	$(call BUILDBACKEND, deb)


# standard rule for dependent gems
%.gem : | prepare
	$(call MKPACKAGE, rpm, $@)
	$(call MKPACKAGE, deb, $@)

clean :
	rm -rf ./packages/
	rm -rf *.rpm *.deb

cleanall : clean
	rm -rf $(GEMDIR)

prepare :
	bundle install --without development
	bundle pack

rebuild: cleanall all

.PHONY: prepare clean cleanall rebuild
FORCE :
