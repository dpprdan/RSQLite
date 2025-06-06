# Dependencies
include $(OBJECTS:.o=.dd)
include $(PKG_LIBS:.o=.dd)

# Include only local dependencies for smaller footprint
# Add trailing backslash and sort
# Add a dependency to .Rconfig for each file
%.dd: %.d
	sed -r 's/([^ ]) ([^ \\])/\1 \\\n  \2/g' $< | sed -r '/^  ([/]|vendor|[.][.])/D;$$s/([^\\])$$/\1 \\/' | { echo "# Generated by deps.mk, do not edit by hand and do not add dependencies to system headers"; read -r header; printf '%s\n' "$$header"; LOCALE=C sort; echo "  .Rconfig"; } > $@

# Ensure that all objects are rebuilt if the R configuration (or version, part of that) changes
$(SHLIB): .Rconfig

.Rconfig: FORCE
	@R CMD config --all > .Rconfig.current
	@if ! cmp -s .Rconfig .Rconfig.current; then \
		echo "R config changed; updating .Rconfig"; \
		mv .Rconfig.current .Rconfig; \
	else \
		rm .Rconfig.current; \
	fi

FORCE:
