SHELL=/bin/sh

PREFIX=/usr/local

.PHONY : all
all :
	cd src && $(MAKE) PREFIX=$(PREFIX)

.PHONY : install
install :
	cd src && $(MAKE) PREFIX=$(PREFIX) install
	install -D lib/base.cf                 $(PREFIX)/lib/confluence/base.cf
	install -D lib/list.cf                 $(PREFIX)/lib/confluence/list.cf
	install -D lib/base_test/base_test.cf  $(PREFIX)/lib/confluence/base_test/base_test.cf
	install -D lib/base_test/run_test      $(PREFIX)/lib/confluence/base_test/run_test
	install -D lib/common/common.cf        $(PREFIX)/lib/confluence/common/common.cf
	install -D lib/common/sync.cf          $(PREFIX)/lib/confluence/common/sync.cf
	install -D lib/common/unit_test.cf     $(PREFIX)/lib/confluence/common/unit_test.cf

.PHONY : uninstall
uninstall :
	cd src && $(MAKE) PREFIX=$(PREFIX) uninstall
	-rm -r $(PREFIX)/lib/confluence

.PHONY : clean
clean :
	cd src && $(MAKE) PREFIX=$(PREFIX) clean

