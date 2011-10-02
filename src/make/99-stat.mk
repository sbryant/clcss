#-*- mode:makefile-gmake; -*-
locs:
	@$(MAKE) locs-src
	@$(MAKE) locs-tests
	@$(MAKE) locs-total

locs-src:
	@echo "=> Source lines"
	@find $(ROOT)/src -name '*.lisp' | xargs wc -l | grep total

locs-tests:
	@echo "=> Tests lines"
	@find $(ROOT)/tests -name '*.lisp' | xargs wc -l | grep total || echo "  [!!] No total available"

locs-total:
	@echo "=> All the locs"
	@find $(ROOT)/tests $(ROOT)/src -name '*.lisp' | xargs wc -l | grep total
