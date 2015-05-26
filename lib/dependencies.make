## dependencies.make --
#
# Automatically built.

lib/vicare/crypto/cityhash.fasl: \
		lib/vicare/crypto/cityhash.vicare.sls \
		lib/vicare/crypto/cityhash/unsafe-capi.fasl \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_cityhash_fasldir = $(bundledlibsdir)/vicare/crypto
lib_vicare_crypto_cityhash_vicare_slsdir  = $(bundledlibsdir)/vicare/crypto
nodist_lib_vicare_crypto_cityhash_fasl_DATA = lib/vicare/crypto/cityhash.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_cityhash_vicare_sls_DATA = lib/vicare/crypto/cityhash.vicare.sls
endif
EXTRA_DIST += lib/vicare/crypto/cityhash.vicare.sls
CLEANFILES += lib/vicare/crypto/cityhash.fasl

lib/vicare/crypto/cityhash/unsafe-capi.fasl: \
		lib/vicare/crypto/cityhash/unsafe-capi.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_cityhash_unsafe_capi_fasldir = $(bundledlibsdir)/vicare/crypto/cityhash
lib_vicare_crypto_cityhash_unsafe_capi_vicare_slsdir  = $(bundledlibsdir)/vicare/crypto/cityhash
nodist_lib_vicare_crypto_cityhash_unsafe_capi_fasl_DATA = lib/vicare/crypto/cityhash/unsafe-capi.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_cityhash_unsafe_capi_vicare_sls_DATA = lib/vicare/crypto/cityhash/unsafe-capi.vicare.sls
endif
EXTRA_DIST += lib/vicare/crypto/cityhash/unsafe-capi.vicare.sls
CLEANFILES += lib/vicare/crypto/cityhash/unsafe-capi.fasl

lib/vicare/crypto/cityhash/features.fasl: \
		lib/vicare/crypto/cityhash/features.vicare.sls \
		$(FASL_PREREQUISITES)
	$(VICARE_COMPILE_RUN) --output $@ --compile-library $<

lib_vicare_crypto_cityhash_features_fasldir = $(bundledlibsdir)/vicare/crypto/cityhash
lib_vicare_crypto_cityhash_features_vicare_slsdir  = $(bundledlibsdir)/vicare/crypto/cityhash
nodist_lib_vicare_crypto_cityhash_features_fasl_DATA = lib/vicare/crypto/cityhash/features.fasl
if WANT_INSTALL_SOURCES
dist_lib_vicare_crypto_cityhash_features_vicare_sls_DATA = lib/vicare/crypto/cityhash/features.vicare.sls
endif
CLEANFILES += lib/vicare/crypto/cityhash/features.fasl


### end of file
# Local Variables:
# mode: makefile-automake
# End:
