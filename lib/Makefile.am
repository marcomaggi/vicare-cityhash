## Process this file with automake to produce Makefile.in

#EXTRA_DIST	= \
#	vicare/crypto/cityhash/constants.sls.in


# These files are hand-written.
nobase_dist_libvicare_DATA = \
	vicare/crypto/cityhash.sls		\
	vicare/crypto/cityhash/unsafe-capi.sls

# These files are created at configuration or make time.
nobase_nodist_libvicare2_DATA = \
	vicare/crypto/cityhash/features.sls
#	vicare/crypto/cityhash/constants.sls

# if WANT_NAUSICAA
# nobase_dist_libvicare_DATA	+= nausicaa/crypto/cityhash.vicare.sls
# endif

VICARE_LIB_INFIX	= crypto/cityhash

include $(top_srcdir)/meta/makefiles/lib.am

### end of file
