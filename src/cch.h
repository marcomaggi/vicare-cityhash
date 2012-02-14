/*
  Part of: C wrapper for CityHash
  Contents: header file
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

#ifndef CCH_H
#define CCH_H 1


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdlib.h>
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

typedef struct cch_uint128_t {
  uint64_t	low;
  uint64_t	high;
} cch_uint128_t;

cch_decl uint64_t cch_cityhash64            (const char *buf, size_t len);
cch_decl uint64_t cch_cityhash64_with_seed  (const char *buf, size_t len,
					     uint64_t seed);
cch_decl uint64_t cch_cityhash64_with_seeds (const char *buf, size_t len,
					     uint64_t seed0, uint64_t seed1);
cch_decl void     cch_cityhash128           (cch_uint128_t * result,
					     const char *s, size_t len);
cch_decl void     cch_cityhash128_with_seed (cch_uint128_t * result,
					     const char *s, size_t len,
					     cch_uint128_t * seme);
cch_decl uint64_t cch_hash128to64 (cch_uint128_t * hash);


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */

#ifdef __cplusplus
} // extern "C"
#endif

#endif /* CCH_H */

/* end of file */
