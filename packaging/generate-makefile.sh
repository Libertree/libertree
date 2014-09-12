#!/usr/bin/env bash

# build common Gemfile from sub-project Gemfiles
echo -n "Generating Gemfile... "
ruby build_gemfile.rb > Gemfile
echo "DONE"

# install all dependencies to a local directory and create Gemfile.lock
echo -n "Installing dependent gems... "
rm Gemfile.lock
bundle package --all 2>&1 >bundle.log
echo "DONE"

# generate Makefile
# - get all dependencies + versions from Gemfile.lock
# - remove libertree- dependencies (we build them separately)
# - replace beginning with tab
# - remove version parentheses and triple escape line break (for this
#   sed command and the next)
# - prepend Makefile recipe to first line
# - end last line with two closing parentheses

echo -n "Generating Makefile... "
BEGINNING='gems : $(addprefix $(PKG_PREFIX)-,$(addsuffix .$(PKG_TYPE),'
DEPS=$(grep -E -e '^    [a-z]+' Gemfile.lock | \
    grep -E -v -e '^    libertree-'   | \
    sed -e 's/^    /\t/'                \
        -e 's/ (/-/'                    \
        -e 's/)$/ \\\\\\/'                  \
        -e "1 { s/^\t/$BEGINNING/ }" \
        -e '$ { s/ \\\\\\$/))/ }')

sed -e "s/PLACEHOLDER_FOR_GEM_DEPENDENCIES/$DEPS/" Makefile.template > Makefile
echo "DONE"
