;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/CityHash
;;;Contents: Cityhash binding backend
;;;Date: Sat Jan 21, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
#!(load-shared-library "vicare-cityhash")
(library (vicare cityhash)
  (export

    ;; version numbers and strings
    vicare-cityhash-version-interface-current
    vicare-cityhash-version-interface-revision
    vicare-cityhash-version-interface-age
    vicare-cityhash-version

    ;; hash functions
    CityHash64

    )
  (import (vicare)
    (prefix (vicare unsafe-operations)
	    unsafe.)
    (prefix (vicare words)
	    words.)
    (vicare syntactic-extensions))


;;;; arguments validation

(define-argument-validation (false/length who obj)
  (or (not obj) (and (fixnum? obj) (unsafe.fx<= 0 obj)))
  (assertion-violation who "expected false or non-negative fixnum as argument" obj))

(define-argument-validation (uint64 who obj)
  (words.word-u64? obj)
  (assertion-violation who "expected uint64 as argument" obj))

(define-argument-validation (pointer/bytevector who obj)
  (or (pointer? obj) (bytevector? obj))
  (assertion-violation who "expected pointer or bytevector as argument" obj))

(define-argument-validation (bytevector who obj)
  (bytevector? obj)
  (assertion-violation who "expected bytevector as argument" obj))


;;;; version functions

(define-inline (vicare-cityhash-version-interface-current)
  (foreign-call "ikrt_cityhash_version_interface_current"))

(define-inline (vicare-cityhash-version-interface-revision)
  (foreign-call "ikrt_cityhash_version_interface_revision"))

(define-inline (vicare-cityhash-version-interface-age)
  (foreign-call "ikrt_cityhash_version_interface_age"))

(define-inline (vicare-cityhash-version)
  (ascii->string (foreign-call "ikrt_cityhash_version")))


;;;; hash functions

(define-inline (capi.cityhash64 buf len)
  (foreign-call "ikrt_cityhash_cityhash64" buf len))

(define-inline (capi.cityhash64-with-seed buf len seed)
  (foreign-call "ikrt_cityhash_cityhash64_with_seed" buf len seed))

(define-inline (capi.cityhash64-with-seeds buf len seed0 seed1)
  (foreign-call "ikrt_cityhash_cityhash64_with_seeds" buf len seed0 seed1))

(define CityHash64
  (case-lambda
   ((buf)
    (define who 'CityHash64)
    (with-arguments-validation (who)
	((bytevector	buf))
      (capi.cityhash64 buf #f)))
   ((buf len)
    (define who 'CityHash64)
    (with-arguments-validation (who)
	((pointer/bytevector	buf)
	 (false/length		len))
      (capi.cityhash64 buf len)))
   ((buf len seed)
    (define who 'CityHash64)
    (with-arguments-validation (who)
	((pointer/bytevector	buf)
	 (false/length		len)
	 (uint64		seed))
      (capi.cityhash64-with-seed buf len seed)))
   ((buf len seed0 seed1)
    (define who 'CityHash64)
    (with-arguments-validation (who)
	((pointer/bytevector	buf)
	 (false/length		len)
	 (uint64		seed0)
	 (uint64		seed1))
      (capi.cityhash64-with-seeds buf len seed0 seed1)))))


;;;; done

#;(set-rtd-printer! (type-descriptor XML_ParsingStatus) %struct-XML_ParsingStatus-printer)

#;(post-gc-hooks (cons %free-allocated-parser (post-gc-hooks)))

)

;;; end of file
