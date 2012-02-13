/*
  Part of: Vicare/CityHash
  Contents: Cityhash for Vicare
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


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <vicare.h>
#include <internals.h>


/** --------------------------------------------------------------------
 ** Version functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_cityhash_version_interface_current (void)
{
  return IK_FIX(vicare_cityhash_VERSION_INTERFACE_CURRENT);
}
ikptr
ikrt_cityhash_version_interface_revision (void)
{
  return IK_FIX(vicare_cityhash_VERSION_INTERFACE_REVISION);
}
ikptr
ikrt_cityhash_version_interface_age (void)
{
  return IK_FIX(vicare_cityhash_VERSION_INTERFACE_AGE);
}
ikptr
ikrt_cityhash_version (ikpcb * pcb)
{
  return ika_bytevector_from_cstring(pcb, vicare_cityhash_VERSION_INTERFACE_STRING);
}


/** --------------------------------------------------------------------
 ** Hash functions.
 ** ----------------------------------------------------------------- */

ikptr
ikrt_cityhash_cityhash64 (ikptr s_bytevector, ikpcb * pcb)
{
  char *	buf = IK_BYTEVECTOR_DATA_CHARP(s_bytevector);
  size_t	len = IK_BYTEVECTOR_LENGTH(s_bytevector);
  uint64_t	rv;
  rv = cch_cityhash64(buf, len);
  return ika_integer_from_uint64(pcb, rv);
}
ikptr
ikrt_cityhash_cityhash64_with_seed (ikptr s_bytevector, ikptr s_seed, ikpcb * pcb)
{
  char *	buf  = IK_BYTEVECTOR_DATA_CHARP(s_bytevector);
  size_t	len  = IK_BYTEVECTOR_LENGTH(s_bytevector);
  uint64_t	seed = ik_integer_to_uint64(s_seed);
  uint64_t	rv;
  rv = cch_cityhash64_with_seed(buf, len, seed);
  return ika_integer_from_uint64(pcb, rv);
}
ikptr
ikrt_cityhash_cityhash64_with_seeds (ikptr s_bytevector, ikptr s_seed0, ikptr s_seed1, ikpcb * pcb)
{
  char *	buf   = IK_BYTEVECTOR_DATA_CHARP(s_bytevector);
  size_t	len   = IK_BYTEVECTOR_LENGTH(s_bytevector);
  uint64_t	seed0 = ik_integer_to_uint64(s_seed0);
  uint64_t	seed1 = ik_integer_to_uint64(s_seed1);
  uint64_t	rv;
  rv = cch_cityhash64_with_seeds(buf, len, seed0, seed1);
  return ika_integer_from_uint64(pcb, rv);
}

#if 0
cch_decl void     cch_cityhash128           (cch_uint128_t * result,
					     const char *s, size_t len);
cch_decl void     cch_cityhash128_with_seed (cch_uint128_t * result,
					     const char *s, size_t len,
					     cch_uint128_t * seme);
cch_decl uint64_t cch_hash128to64 (cch_uint128_t * hash);
#endif

/* end of file */
