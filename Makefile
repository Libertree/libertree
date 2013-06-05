PREFIX = /usr/share/libertree/gems
PKG_PREFIX = libertree
VERSION = 0.0.1

GEMDIR := vendor/cache

MKPACKAGE = fpm -d ruby -d rubygems $(3) \
		--prefix $(PREFIX) --gem-package-name-prefix $(PKG_PREFIX) \
		-s gem -t $(1) $(addprefix $(GEMDIR)/,$(2))

WRAPGEMDIR = fpm -d ruby -d rubygems $(4) \
		--prefix $(PREFIX) \
		-s dir -t $(1) -n $(addprefix $(PKG_PREFIX)-,$(3)) $(addprefix $(GEMDIR)/,$(2))

all: libertree-model-$(VERSION).gem gems move

move : gems
	mkdir -p ./packages/{rpm,deb}
	mv *.rpm ./packages/rpm/
	mv *.deb ./packages/deb/

gems : bcrypt-ruby-3.0.1.gem \
	epoxy-0.3.1.gem \
	m4dbi-0.8.7.gem \
	metaid-1.0.gem \
	methlab-0.1.0.gem \
	pg-0.9.0.gem \
	typelib-0.1.0.gem \
	rdbi-4ca05ac7c355 \
	rdbi-driver-postgresql-be571a9909f7

# TODO: add all built gem packages as dependencies
# TODO: share database.yaml with other packages?
# TODO: specify database.yaml.example as config file
BUILDPKG = fpm -d ruby -d rubygems \
		--prefix $(PREFIX) \
		--gem-package-name-prefix $(PKG_PREFIX) \
		--maintainer "rekado+libertree@elephly.net" \
		--version $(VERSION) \
		--license AGPL3 \
		-s gem -t $(1) $(2)

libertree-model-$(VERSION).gem : FORCE
	gem build libertree-model.gemspec
	$(call BUILDPKG, rpm, $@)
	$(call BUILDPKG, deb, $@)

# standard rule for dependent gems
%.gem : | prepare
	$(call MKPACKAGE, rpm, $@)
	$(call MKPACKAGE, deb, $@)

# directory (because we use git version)
rdbi-4ca05ac7c355 : | prepare
	$(call WRAPGEMDIR, rpm, $@, rdbi, -v 4ca05ac7c355)
	$(call WRAPGEMDIR, deb, $@, rdbi, -v 4ca05ac7c355)

# directory (because we use git version)
rdbi-driver-postgresql-be571a9909f7 : | prepare
	$(call WRAPGEMDIR, rpm, $@, rdbi-driver-postgresql, -v be571a9909f7)
	$(call WRAPGEMDIR, deb, $@, rdbi-driver-postgresql, -v be571a9909f7)

# TODO: does this depend on a system library?
bcrypt-ruby-3.0.1.gem : | prepare
	$(call MKPACKAGE, rpm, $@)
	$(call MKPACKAGE, deb, $@)

pg-0.9.0.gem : | prepare
	$(call MKPACKAGE, rpm, $@, -d postgresql-libs)
	$(call MKPACKAGE, deb, $@, -d libpq5)

clean :
	rm -rf ./packages/
	rm -rf *.rpm *.deb

cleanall : clean
	rm -rf $(GEMDIR)
	rm -rf libertree-model-$(VERSION).gem

prepare :
	bundle install --without development
	bundle pack --all

rebuild: cleanall all

.PHONY: prepare clean cleanall rebuild
FORCE :
