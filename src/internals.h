/*
  Part of: Vicare/CityHash
  Contents: internal header file
  Date: Mon Feb 13, 2012

  Abstract



  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either  version 3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See  the GNU
  General Public License for more details.

  You  should have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef INTERNALS_H
#define INTERNALS_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <limits.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef cch_decl
#  define cch_decl	extern
#endif


/** --------------------------------------------------------------------
 ** Hash functions.
 ** ----------------------------------------------------------------- */

cch_decl uint64_t cch_city_hash_64 (const char *buf, size_t len);


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* INTERNALS_H */

/* end of file */
