.PHONY: all docs

SCRIBA_SOURCES=$(wildcard docs/scriba/*.scr)
BUILD_DIR=docs/build/dsolv-generic-dependency-resolver/
SIMPLE_DIR=$(BUILD_DIR)/simple
HTML_DIR=$(BUILD_DIR)/html
HTML_FILES=$(HTML_DIR)/overview.html \
           $(HTML_DIR)/get-dsolv.html \
           $(HTML_DIR)/quickstart.html \
           $(HTML_DIR)/a-longer-example.html \
           $(HTML_DIR)/command-reference.html \
           $(HTML_DIR)/architecture.html \
           $(HTML_DIR)/recipes.html \
           $(HTML_DIR)/changelog.html \
           $(HTML_DIR)/roadmap.html \
           $(HTML_DIR)/code-of-conduct.html \
           $(HTML_DIR)/contributing.html \
           $(HTML_DIR)/authors-and-contributions.html \
           $(HTML_DIR)/3rd-party-licenses.html \
           $(HTML_DIR)/api-reference.html
SIMPLE_HTML_FILES=$(SIMPLE_DIR)/overview.html \
                  $(SIMPLE_DIR)/get-dsolv.html \
                  $(SIMPLE_DIR)/quickstart.html \
                  $(SIMPLE_DIR)/a-longer-example.html \
                  $(SIMPLE_DIR)/command-reference.html \
                  $(SIMPLE_DIR)/architecture.html \
                  $(SIMPLE_DIR)/recipes.html \
                  $(SIMPLE_DIR)/changelog.html \
                  $(SIMPLE_DIR)/roadmap.html \
                  $(SIMPLE_DIR)/code-of-conduct.html \
                  $(SIMPLE_DIR)/contributing.html \
                  $(SIMPLE_DIR)/authors-and-contributions.html \
                  $(SIMPLE_DIR)/3rd-party-licenses.html \
                  $(SIMPLE_DIR)/api-reference.html

docs: $(HTML_DIR)/manual.pdf $(HTML_FILES)

docs/manual.scr: $(SCRIBA_SOURCES)
	cat $(SCRIBA_SOURCES) > docs/manual.scr

$(HTML_FILES): docs/manual.scr docs/manifest.lisp
	./docs/build-docs.ros
	mkdir -p $(HTML_DIR)/assets
	cp -r docs/assets/. $(HTML_DIR)/assets/
	cd $(HTML_DIR) && \
		rm -f index.html && \
		ln -s overview.html index.html

$(HTML_DIR)/manual.pdf: $(SIMPLE_HTML_FILES)
	pandoc  -t pdf \
			-f html \
			-o $(HTML_DIR)/manual.pdf \
			--toc \
			--metadata "author=Daniel Jay Haskin" \
			--metadata "title=dsolv: Generic Dependency Resolver" \
			--file-scope \
			--indented-code-classes=lisp \
			-V colorlinks=true \
			-V 'fontsize=12pt' \
			-V 'geometry=margin=1in' \
			$(SIMPLE_HTML_FILES)

$(SIMPLE_DIR)/%.html: $(HTML_DIR)/%.html
	mkdir -p $(SIMPLE_DIR)
	cp -r $(HTML_DIR)/assets $(SIMPLE_DIR)/ 2>/dev/null || true
	xmlstarlet format --omit-decl --recover --html $< | \
		xmlstarlet edit \
		    --pf --omit-decl \
			--rename "//h3" -v "h4" \
			--rename "//h2" -v "h3" \
			--rename "//h1" -v "h2" \
			--rename "//h2[@class='doc-title']" -v "h1" \
			--delete "//aside" \
			--delete '//footer' | \
		sed -e 's|dsolv: Generic Dependency Resolver &#xBB; ||g' \
		> $@
