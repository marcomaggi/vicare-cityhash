/*
  Part of: C wrapper for CityHash
  Contents: Cityhash for C language
  Date: Mon Feb 13, 2012

  Abstract

	This file bridges C++ from  CityHash with C language.  This file
	can be considered a C language wrapper for the C++ functions.

  Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>

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
#include <stdio.h>
#include <stdlib.h>

/* The macro  CCH_UNUSED indicates that a function,  function argument or
   variable may potentially be unused.	Usage examples:

   static int unused_function (char arg) CCH_UNUSED;
   int foo (char unused_argument CCH_UNUSED);
   int unused_variable CCH_UNUSED;
*/
#ifdef __GNUC__
#  define CCH_UNUSED		__attribute__((unused))
#else
#  define CCH_UNUSED		/* empty */
#endif

static CCH_UNUSED void
feature_failure_ (const char * funcname)
{
  fprintf(stderr, "called unavailable CityHash function, %s\n",
	  funcname);
  exit(EXIT_FAILURE);
}

#define feature_failure(FN)     { feature_failure_(FN); }


/** --------------------------------------------------------------------
 ** Hash functions.
 ** ----------------------------------------------------------------- */

uint64_t
cch_cityhash64 (const char *buf, size_t len)
{
#ifdef HAVE_CITYHASH64
  return CityHash64(buf, len);
#else
  feature_failure(__func__);
#endif
}
uint64_t
cch_cityhash64_with_seed (const char *buf, size_t len, uint64_t seed)
{
#ifdef HAVE_CITYHASH64WITHSEED
  return CityHash64WithSeed(buf, len, seed);
#else
  feature_failure(__func__);
#endif
}
uint64_t
cch_cityhash64_with_seeds (const char *buf, size_t len, uint64_t seed0, uint64_t seed1)
{
#ifdef HAVE_CITYHASH64WITHSEEDS
  return CityHash64WithSeeds(buf, len, seed0, seed1);
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

void
cch_cityhash128 (cch_uint128_t * result, const char *s, size_t len)
{
#ifdef HAVE_CITYHASH128
  uint128	rv;
  rv = CityHash128(s, len);
  result->low  = Uint128Low64(rv);
  result->high = Uint128High64(rv);
#else
  feature_failure(__func__);
#endif
}
void
cch_cityhash128_with_seed (cch_uint128_t * result, const char *s, size_t len, cch_uint128_t * seme)
{
#ifdef HAVE_CITYHASH128WITHSEED
  uint128	rv;
  uint128	seed(seme->low, seme->high);
  rv = CityHash128WithSeed(s, len, seed);
  result->low  = Uint128Low64(rv);
  result->high = Uint128High64(rv);
#else
  feature_failure(__func__);
#endif
}
uint64_t
cch_hash128to64 (cch_uint128_t * hash)
{
#ifdef HAVE_HASH128TO64
  uint128	H(hash->low, hash->high);
  return Hash128to64(H);
#else
  feature_failure(__func__);
#endif
}

/* end of file */
