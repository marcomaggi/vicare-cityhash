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
#include <cch.h>


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
ikrt_cityhash_cityhash64 (ikptr s_buffer, ikptr s_length, ikpcb * pcb)
{
  char *	buf;
  size_t	len;
  uint64_t	rv;
  if (ik_is_pointer(s_buffer)) {
    buf = IK_POINTER_DATA_CHARP(s_buffer);
    len = ik_integer_to_size_t(s_length);
  } else {
    buf = IK_BYTEVECTOR_DATA_CHARP(s_buffer);
    len = IK_BYTEVECTOR_LENGTH(s_buffer);
  }
  rv = cch_cityhash64(buf, len);
  return ika_integer_from_uint64(pcb, rv);
}
ikptr
ikrt_cityhash_cityhash64_with_seed (ikptr s_buffer, ikptr s_length, ikptr s_seed, ikpcb * pcb)
{
  char *	buf;
  size_t	len;
  uint64_t	seed = ik_integer_to_uint64(s_seed);
  uint64_t	rv;
  if (ik_is_pointer(s_buffer)) {
    buf = IK_POINTER_DATA_CHARP(s_buffer);
    len = ik_integer_to_size_t(s_length);
  } else {
    buf = IK_BYTEVECTOR_DATA_CHARP(s_buffer);
    len = IK_BYTEVECTOR_LENGTH(s_buffer);
  }
  rv = cch_cityhash64_with_seed(buf, len, seed);
  return ika_integer_from_uint64(pcb, rv);
}
ikptr
ikrt_cityhash_cityhash64_with_seeds (ikptr s_buffer, ikptr s_length,
				     ikptr s_seed0, ikptr s_seed1, ikpcb * pcb)
{
  char *	buf;
  size_t	len;
  uint64_t	seed0 = ik_integer_to_uint64(s_seed0);
  uint64_t	seed1 = ik_integer_to_uint64(s_seed1);
  uint64_t	rv;
  if (ik_is_pointer(s_buffer)) {
    buf = IK_POINTER_DATA_CHARP(s_buffer);
    len = ik_integer_to_size_t(s_length);
  } else {
    buf = IK_BYTEVECTOR_DATA_CHARP(s_buffer);
    len = IK_BYTEVECTOR_LENGTH(s_buffer);
  }
  rv = cch_cityhash64_with_seeds(buf, len, seed0, seed1);
  return ika_integer_from_uint64(pcb, rv);
}

/* ------------------------------------------------------------------ */

ikptr
ikrt_cityhash_cityhash128 (ikptr s_buffer, ikptr s_length, ikpcb * pcb)
{
  char *	buf;
  size_t	len;
  cch_uint128_t	result;
  ikptr		s_pair;
  if (ik_is_pointer(s_buffer)) {
    buf = IK_POINTER_DATA_CHARP(s_buffer);
    len = ik_integer_to_size_t(s_length);
  } else {
    buf = IK_BYTEVECTOR_DATA_CHARP(s_buffer);
    len = IK_BYTEVECTOR_LENGTH(s_buffer);
  }
  cch_cityhash128(&result, buf, len);
  s_pair = ika_pair_alloc(pcb);
  pcb->root0 = &s_pair;
  {
    IK_ASS(IK_CAR(s_pair), ika_integer_from_uint64(pcb, result.low));
    IK_ASS(IK_CDR(s_pair), ika_integer_from_uint64(pcb, result.high));
  }
  pcb->root0 = NULL;
  return s_pair;
}
ikptr
ikrt_cityhash_cityhash128_with_seed (ikptr s_buffer, ikptr s_length,
				     ikptr s_seed_low, ikptr s_seed_high, ikpcb * pcb)
{
  char *	buf;
  size_t	len;
  cch_uint128_t	seed;
  cch_uint128_t	result;
  ikptr		s_pair;
  if (ik_is_pointer(s_buffer)) {
    buf = IK_POINTER_DATA_CHARP(s_buffer);
    len = ik_integer_to_size_t(s_length);
  } else {
    buf = IK_BYTEVECTOR_DATA_CHARP(s_buffer);
    len = IK_BYTEVECTOR_LENGTH(s_buffer);
  }
  seed.low  = ik_integer_to_uint64(s_seed_low);
  seed.high = ik_integer_to_uint64(s_seed_high);
  cch_cityhash128_with_seed(&result, buf, len, &seed);
  s_pair = ika_pair_alloc(pcb);
  pcb->root0 = &s_pair;
  {
    IK_ASS(IK_CAR(s_pair), ika_integer_from_uint64(pcb, result.low));
    IK_ASS(IK_CDR(s_pair), ika_integer_from_uint64(pcb, result.high));
  }
  pcb->root0 = NULL;
  return s_pair;
}


#if 0
cch_decl uint64_t cch_hash128to64 (cch_uint128_t * hash);
#endif

/* end of file */
