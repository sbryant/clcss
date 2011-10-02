ASDF_CONF = (:directory \"$(ROOT)/\")
ASDF_CONF_NAME = $(REGISTRYD)/"$(TARGET)-02.conf"

VENDOR_ASDF_CONF = (:tree \"$(ROOT)/vendor/\")
VENDOR_ASDF_CONF_NAME = $(REGISTRYD)/"$(TARGET)-01.conf"

asdf: | $(REGISTRYD) $(ASDF_CONF_NAME) $(VENDOR_ASDF_CONF_NAME)
	@echo "Added $(TARGET) in $(ROOT) to ASDF registry"

$(REGISTRYD):
	@echo "=> Creating ASDF registry configuration directory"
	mkdir -p $(REGISTRYD)

$(ASDF_CONF_NAME):
	@echo "=> Installing: $(ASDF_CONF_NAME)"
	echo "$(ASDF_CONF)" > $(ASDF_CONF_NAME)

$(VENDOR_ASDF_CONF_NAME):
	@echo "=> Installing: $(VENDOR_ASDF_CONF_NAME)"
	echo "$(VENDOR_ASDF_CONF)" > $(VENDOR_ASDF_CONF_NAME)
