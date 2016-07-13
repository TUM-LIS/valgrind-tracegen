include $(top_srcdir)/Makefile.tool.am

EXTRA_DIST = docs/tg-manual.xml
#----------------------------------------------------------------------------
# Headers, etc
#----------------------------------------------------------------------------

noinst_HEADERS = \
	tg_ctf.h \
	tg_ctf-platform-valgrind.h

#----------------------------------------------------------------------------
# tracegen-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = tracegen-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += tracegen-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

TRACEGEN_SOURCES_COMMON = \
	tg_main.c \
	tg_ctf.c \
	tg_ctf-platform-valgrind.c

tracegen_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(TRACEGEN_SOURCES_COMMON)
tracegen_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
tracegen_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
tracegen_@VGCONF_ARCH_PRI@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_PRI_CAPS@)
tracegen_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_PRI_CAPS@)
tracegen_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
tracegen_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_PRI@ \
	$(LINK) \
	$(tracegen_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS) \
	$(tracegen_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS)

if VGCONF_HAVE_PLATFORM_SEC
tracegen_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES      = \
	$(TRACEGEN_SOURCES_COMMON)
tracegen_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
tracegen_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
tracegen_@VGCONF_ARCH_SEC@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_SEC_CAPS@)
tracegen_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
tracegen_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
tracegen_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_SEC@ \
	$(LINK) \
	$(tracegen_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS) \
	$(tracegen_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS)
endif

