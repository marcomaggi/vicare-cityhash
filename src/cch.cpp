/*
  Part of: C wrapper for CityHash
  Contents: Cityhash for C language
  Date: Mon Feb 13, 2012

  Abstract

	This file bridges C++ from  CityHash with C language.  This file
	can be considered a C language wrapper for the C++ functions.

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


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <cch.h>
#include <city.h>


/** --------------------------------------------------------------------
 ** Hash functions.
 ** ----------------------------------------------------------------- */

uint64_t
cch_cityhash64 (const char *buf, size_t len)
{
  return CityHash64(buf, len);
}
uint64_t
cch_cityhash64_with_seed (const char *buf, size_t len, uint64_t seed)
{
  return CityHash64WithSeed(buf, len, seed);
}
uint64_t
cch_cityhash64_with_seeds (const char *buf, size_t len, uint64_t seed0, uint64_t seed1)
{
  return CityHash64WithSeeds(buf, len, seed0, seed1);
}

/* ------------------------------------------------------------------ */

void
cch_cityhash128 (cch_uint128_t * result, const char *s, size_t len)
{
  uint128	rv;
  rv = CityHash128(s, len);
  result->low  = Uint128Low64(rv);
  result->high = Uint128High64(rv);
}
void
cch_cityhash128_with_seed (cch_uint128_t * result, const char *s, size_t len, cch_uint128_t * seme)
{
  uint128	rv;
  uint128	seed(seme->low, seme->high);
  rv = CityHash128WithSeed(s, len, seed);
  result->low  = Uint128Low64(rv);
  result->high = Uint128High64(rv);
}
uint64_t
cch_hash128to64 (cch_uint128_t * hash)
{
  uint128	H(hash->low, hash->high);
  return Hash128to64(H);
}

/* end of file */
