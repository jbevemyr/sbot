include ../include.mk

ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug
else
DEBUG_FLAGS =
endif

MODULES=slackbot url

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR))

ERLC_FLAGS+=-W +debug_info $(DEBUG_FLAGS)

#
# Targets
#

all: $(EBIN_FILES)

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES)

install: all
	install -d $(MONARKINSTALLDIR)
	install -d $(EBININSTALLDIR)
	(cd ../ebin; find . -name '*' -print | xargs tar cf - ) | \
		(cd $(EBININSTALLDIR); tar xf - )
	(cd ../docroot; find . -name '*' -print | xargs tar cf -  ) | \
		(cd $(MONARKINSTALLDIR); tar xf - )

