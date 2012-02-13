## Process this file with automake to produce Makefile.in

vicare_cityhash_CURRENT	= @vicare_cityhash_VERSION_INTERFACE_CURRENT@
vicare_cityhash_REVISION= @vicare_cityhash_VERSION_INTERFACE_REVISION@
vicare_cityhash_AGE	= @vicare_cityhash_VERSION_INTERFACE_AGE@
vicare_cityhash_VERSION	= $(vicare_cityhash_CURRENT):$(vicare_cityhash_REVISION):$(vicare_cityhash_AGE)

lib_LTLIBRARIES			= libvicare-cityhash.la
libvicare_cityhash_la_LDFLAGS	= -version-info $(vicare_cityhash_VERSION)
libvicare_cityhash_la_SOURCES	= vicare-cityhash.c

### end of file
