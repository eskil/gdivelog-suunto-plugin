
libdir = $(plugindir)

INCLUDES = \
	-DPACKAGE_DATA_DIR=\""$(datadir)"\" \
	-DPACKAGE_LOCALE_DIR=\""$(prefix)/$(DATADIRNAME)/locale"\" \
	@PACKAGE_CFLAGS@

AM_CFLAGS =\
	 -Wall\
	 -g

lib_LTLIBRARIES = gdivelog-suunto-plugin.la

gdivelog_suunto_plugin_la_SOURCES = \
	main.c

gdivelog_suunto_plugin_la_LDFLAGS = @PACKAGE_LIBS@ -module -avoid-version -export-symbols-regex "*_plugin"

gdivelog_suunto_plugin_la_LIBADD = 

install-exec-hook:
	if test -d "$(plugindir)"; then \
		cd "$(plugindir)" && rm -f gdivelog-suunto-plugin.{a,la}; \
	fi