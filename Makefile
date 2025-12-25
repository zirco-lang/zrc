V ?= 0

ifeq ($(V),1)
Q := 
ECHO := @true
else
Q := @
ECHO := @echo
endif

.PHONY: libzr examples clean

examples: libzr
	$(Q)$(MAKE) -C examples test V=$(V)

libzr:
	$(Q)$(MAKE) -C libzr all-opt V=$(V)

clean:
	$(Q)$(MAKE) -C libzr clean
	$(Q)$(MAKE) -C examples clean
