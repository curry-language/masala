# Generic Makefile for Spicey applications

# Definition of the root of the Curry system to be used:
SYSTEM=/opt/pakcs

# Curry bin directory to be used:
export CURRYBIN=$(SYSTEM)/bin

CURRYOPTIONS=:set -time

# Target directory where the compiled cgi programs, style sheets, etc
# should be stored, e.g.: $(HOME)/public_html
WEBSERVERDIR = $(HOME)/public_html/masala

# Directory containing Masala data:
DATADIR=$(WEBSERVERDIR)/data

# Executable of the Curry Package Manager CPM:
CPM := $(CURRYBIN)/cypm

# Executable of the curry2cgi:
CURRY2CGI := $(shell which curry2cgi)

# Executable of CurryPP (require to compile SQL queries):
CURRYPP := $(shell which currypp)
ifeq ("$(CURRYPP)","")
$(error Executable 'currypp' not found! Install it by 'cypm install currypp' and add '~/.cpm/bin' to your PATH)
endif

# all Curry source files used by the implementation
SOURCES := $(shell find src -name "*.curry") src/Model/Queries.curry

############################################################################

.PHONY: all
all:
	@echo "make: deploy install compile load run clean?"

# Install the packages required by the generated Spicey application:
.PHONY: install
install:
	$(CPM) install

# check presence of tools required for deployment and install them:
.PHONY: checkdeploy
checkdeploy:
	@if [ ! -x "$(CURRY2CGI)" ] ; then \
	   echo "Installing required executable 'curry2cgi'..." ; \
           $(CPM) install html2 ; fi

# Generate pure Curry module Model.Queries with CurryPP:
src/Model/Queries.curry: src/Model/Queries.curry.pp
	@if [ ! -x "$(CURRYPP)" ] ; then \
		echo "REQUIRED EXECUTABLE 'currypp' NOT FOUND!" ; \
		echo "Install it by > cypm install currypp" ; \
		exit 1 ; fi
	rm -f $@ && cd src/Model && ln -s Queries.curry.pp Queries.curry
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Model.Queries :quit
	rm $@ && mv src/Model/Queries.curry.CURRYPP $@

# Compile the generated Spicey application:
.PHONY: compile
compile: $(SOURCES)
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Main :quit

# Load the generated Spicey application into the Curry system so that
# one can evaluate some expressions:
.PHONY: load
load: $(SOURCES)
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Main

# Runs the generated Spicey application by evaluating the main expression.
# This might be useful to test only the initial web page without a web server
.PHONY: run
run: $(SOURCES)
	$(CURRYBIN)/curry $(CURRYOPTIONS) :load Main :eval main :q

# Deploy the generated Spicey application, i.e., install it in the
# web directory WEBSERVERDIR:
.PHONY: deploy
deploy: checkdeploy
	mkdir -p $(WEBSERVERDIR)
	$(MAKE) $(WEBSERVERDIR)/run.cgi
	# copy other files (style sheets, images,...)
	cp -r public/* $(WEBSERVERDIR)
	chmod -R go+rX $(WEBSERVERDIR)
	mkdir -p $(DATADIR)
	cp data/htaccess $(DATADIR)/.htaccess
	chmod 700 $(DATADIR)
	# recreate directory for storing local session data:
	/bin/rm -rf $(WEBSERVERDIR)/sessiondata
	mkdir -p $(WEBSERVERDIR)/sessiondata
	cp data/htaccess $(WEBSERVERDIR)/sessiondata/.htaccess
	chmod 700 $(WEBSERVERDIR)/sessiondata

$(WEBSERVERDIR)/run.cgi: $(SOURCES)
	$(CPM) exec $(CURRY2CGI) --system="$(SYSTEM)" \
	  -i Controller.SpiceySystem \
	  -i Controller.User \
	  -i Controller.Package \
	  -i Controller.Version \
	  -i Controller.Category \
	  -i Controller.CurryModule \
	  -i Controller.ValidationToken \
	  -i Controller.Validation \
	  -i Controller.Registration \
	  -i Controller.Upload \
	  -i Controller.Mail \
	  -i System.Spicey \
	  -o $@ Main.curry

# clean up generated the package directory
.PHONY: clean
clean: 
	$(CPM) clean

# clean everything, including the deployed files
.PHONY: cleanall
cleanall: clean
	/bin/rm -f $(WEBSERVERDIR)/run.cgi*
