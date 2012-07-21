dnl @configure_input@
dnl

AC_PREREQ([2.68])
AC_INIT([Vicare/CityHash],[1.0b3],
  [marco.maggi-ipsu@poste.it],[vicare-cityhash],
  [http://marcomaggi.github.com/vicare.html])
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([-Wall])
dnl AM_MAINTAINER_MODE
AC_CONFIG_SRCDIR([src/])
AC_CONFIG_MACRO_DIR([m4])
AC_CANONICAL_HOST

#page
#### compiler

AC_PROG_CC
AC_PROG_CC_C_O

AC_LANG([C++])
AC_PROG_CXX
AC_PROG_CXX_C_O

AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

LT_PREREQ([2.4])
LT_INIT([shared disable-static])

AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T

#page
#### vicare configuration

AC_CHECK_HEADERS([vicare.h],,[AC_MSG_FAILURE([missing vicare.h header],[2])])
AC_CHECK_PROG([VICARE],[vicare],[vicare],[:])

#page
#### cityhash configuration

AC_CHECK_HEADERS([city.h],,
  [AC_MSG_FAILURE([missing city.h header],[2])])
AC_CHECK_LIB([cityhash],[main],,
  [AC_MSG_FAILURE([missing Cityhash library],[2])])

vicare_cityhash_VERSION_INTERFACE_CURRENT=0
vicare_cityhash_VERSION_INTERFACE_REVISION=0
vicare_cityhash_VERSION_INTERFACE_AGE=0
AC_DEFINE_UNQUOTED([vicare_cityhash_VERSION_INTERFACE_CURRENT],
  [$vicare_cityhash_VERSION_INTERFACE_CURRENT],
  [current interface number])
AC_DEFINE_UNQUOTED([vicare_cityhash_VERSION_INTERFACE_REVISION],
  [$vicare_cityhash_VERSION_INTERFACE_REVISION],
  [current interface implementation number])
AC_DEFINE_UNQUOTED([vicare_cityhash_VERSION_INTERFACE_AGE],
  [$vicare_cityhash_VERSION_INTERFACE_AGE],
  [current interface age number])
AC_DEFINE_UNQUOTED([vicare_cityhash_VERSION_INTERFACE_STRING],
  ["$vicare_cityhash_VERSION_INTERFACE_CURRENT.$vicare_cityhash_VERSION_INTERFACE_REVISION"],
  [library interface version])
AC_SUBST([vicare_cityhash_VERSION_INTERFACE_CURRENT])
AC_SUBST([vicare_cityhash_VERSION_INTERFACE_REVISION])
AC_SUBST([vicare_cityhash_VERSION_INTERFACE_AGE])

#page
#### constants

m4_define([VICARE_INCLUDES],[
AC_INCLUDES_DEFAULT
#ifdef HAVE_VICARE_H
#  include <vicare.h>
#endif
#ifdef HAVE_CITY_H
#  include <city.h>
#endif
])

#page
#### finish

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile]
        [src/Makefile]
        [doc/Makefile]
        [tests/Makefile]
        [lib/Makefile]
        [meta/slackware/doinst.sh])
AC_OUTPUT

### end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^#page"
# End:
