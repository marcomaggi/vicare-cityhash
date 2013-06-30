dnl @configure_input@
dnl

AC_PREREQ([2.68])
AC_INIT([Vicare/CityHash],[2.0b2],[marco.maggi-ipsu@poste.it],
  [vicare-cityhash],[http://marcomaggi.github.com/vicare.html])
AC_CONFIG_SRCDIR([src])
AC_CONFIG_MACRO_DIR([autotools])
AC_CONFIG_AUX_DIR([autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([-Wall foreign dist-xz no-dist-gzip])
AM_MAINTAINER_MODE

AC_PROG_CC
AC_PROG_CC_C_O
AC_PROG_CXX
AC_PROG_CXX_C_O
AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P
LT_PREREQ([2.4])
LT_INIT([shared disable-static])

VICARE_SCHEME

VICARE_OPTION_DEBUGGING_MODE
VICARE_OPTION_NAUSICAA([no])
AM_CONDITIONAL([WANT_NAUSICAA],[test x$vicare_with_nausicaa = xyes])

AM_CONDITIONAL([WANT_VICARE_DEBUG],[test x$VICARE_DEBUG = xyes])
AM_COND_IF([WANT_VICARE_DEBUG],[AS_VAR_SET(VFLAGS,"$VFLAGS -g")])

AC_LANG([C])
AC_DEFINE([_GNU_SOURCE],[1],[enable everything GNU])

AC_LANG([C])
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T

#page
#### cityhash configuration

AC_LANG_PUSH([C++])

AC_CHECK_HEADERS([city.h],,
  [AC_MSG_FAILURE([missing city.h header],[2])])
AC_CHECK_HEADERS([citycrc.h],,
  [AC_MSG_WARN([missing citycrc.h header])])
AC_CHECK_LIB([cityhash],[main],,
  [AC_MSG_FAILURE([missing Cityhash library],[2])])

AC_LANG_POP([C++])

VICARE_EXTENSION_LIBRARY_VERSION([cityhash],[0],[0],[0])

#page
#### constants and functions

m4_define([VICARE_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_VICARE_H
#  include <vicare.h>
#endif
#ifdef HAVE_CITY_H
#  include <city.h>
#endif
#ifdef HAVE_CITYCRC_H
#  include <citycrc.h>
#endif
])

dnl These are in the Cityhash C++ library.
VICARE_CHECK_CPP_FUNC([CityHash64],
  [const char buf = 1;
   CityHash64(&buf, 1);],
  [VICARE_INCLUDES])

VICARE_CHECK_CPP_FUNC([CityHash64WithSeed],
  [const char buf = 1;
   CityHash64WithSeed(&buf,1,1);],
  [VICARE_INCLUDES])

VICARE_CHECK_CPP_FUNC([CityHash64WithSeeds],
  [const char buf = 1;
   CityHash64WithSeeds(&buf,1,1,1);],
  [VICARE_INCLUDES])

VICARE_CHECK_CPP_FUNC([CityHash128],
  [const char buf = 1;
   CityHash128(&buf, 1);],
  [VICARE_INCLUDES])

VICARE_CHECK_CPP_FUNC([CityHash128WithSeed],
  [const char buf = 1;
   uint128 seed(1, 2);
   CityHash128WithSeed(&buf,1,seed);],
  [VICARE_INCLUDES])

VICARE_CHECK_CPP_FUNC([Hash128to64],
  [uint128 hash(1, 2);
   Hash128to64(hash);],
  [VICARE_INCLUDES])

dnl These are in the C language adapter library
dnl AC_CHECK_FUNCS([cch_cityhash64 cch_cityhash64_with_seed cch_cityhash64_with_seeds])
dnl AC_CHECK_FUNCS([cch_cityhash128 cch_cityhash128_with_seed cch_hash128to64])

#page
#### finish

VICARE_OUTPUT
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile]
        [src/Makefile]
        [doc/Makefile]
        [tests/Makefile]
        [lib/Makefile]
dnl     [lib/vicare/crypto/cityhash/constants.sls]
        [meta/slackware/doinst.sh])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
