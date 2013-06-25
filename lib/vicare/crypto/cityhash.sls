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
;;;Copyright (C) 2012, 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (vicare crypto cityhash)
  (export

    ;; version numbers and strings
    vicare-cityhash-version-interface-current
    vicare-cityhash-version-interface-revision
    vicare-cityhash-version-interface-age
    vicare-cityhash-version

    ;; hash functions
    CityHash64 CityHash128 Hash128to64)
  (import (vicare)
    (prefix (vicare crypto cityhash unsafe-capi)
	    capi.)
    (vicare unsafe operations)
    (prefix (vicare platform words)
	    words.)
    (vicare arguments validation)
    #;(vicare language-extensions syntaxes))


;;;; arguments validation

(define-argument-validation (false/length who obj)
  (or (not obj) (and (fixnum? obj) ($fx<= 0 obj)))
  (assertion-violation who "expected false or non-negative fixnum as argument" obj))

(define-argument-validation (pointer/bytevector who obj)
  (or (pointer? obj) (bytevector? obj))
  (assertion-violation who "expected pointer or bytevector as argument" obj))


;;;; helpers

(define (%word-u128? N)
  (define U128MAX (- (expt 2 128) 1))
  (define U128MIN 0)
  (if (fixnum? N)
      ($fx<= 0 N)
    (and (bignum? N)
	 ($bignum-positive? N)
	 ($bnbn<= N U128MAX))))


;;;; version functions

(define (vicare-cityhash-version-interface-current)
  (capi.vicare-cityhash-version-interface-current))

(define (vicare-cityhash-version-interface-revision)
  (capi.vicare-cityhash-version-interface-revision))

(define (vicare-cityhash-version-interface-age)
  (capi.vicare-cityhash-version-interface-age))

(define (vicare-cityhash-version)
  (capi.vicare-cityhash-version))


;;;; hash functions 64-bit

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
      (when (pointer? buf)
	(assert (fixnum? len)))
      (capi.cityhash64 buf len)))
   ((buf len seed)
    (define who 'CityHash64)
    (with-arguments-validation (who)
	((pointer/bytevector	buf)
	 (false/length		len)
	 (word-u64		seed))
      (when (pointer? buf)
	(assert (fixnum? len)))
      (capi.cityhash64-with-seed buf len seed)))
   ((buf len seed0 seed1)
    (define who 'CityHash64)
    (with-arguments-validation (who)
	((pointer/bytevector	buf)
	 (false/length		len)
	 (word-u64		seed0)
	 (word-u64		seed1))
      (when (pointer? buf)
	(assert (fixnum? len)))
      (capi.cityhash64-with-seeds buf len seed0 seed1)))))


;;;; hash functions 128-bit

(define CityHash128
  (case-lambda
   ((buf)
    (CityHash128 buf #f))
   ((buf len)
    (define who 'CityHash128)
    (with-arguments-validation (who)
	((pointer/bytevector	buf)
	 (false/length		len))
      (when (pointer? buf)
	(assert (fixnum? len)))
      (let* ((rv (capi.cityhash128 buf len))
	     (lo (car rv))
	     (hi (cdr rv)))
	(bitwise-ior lo (bitwise-arithmetic-shift-left hi 64)))))
   ((buf len seed)
    (define who 'CityHash128)
    (with-arguments-validation (who)
	((pointer/bytevector	buf)
	 (false/length		len)
         (word-u128		seed))
      (when (pointer? buf)
	(assert (fixnum? len)))
      (let* ((seed-low	(bitwise-and seed #xFFFFFFFFFFFFFFFF))
	     (seed-high	(bitwise-and (bitwise-arithmetic-shift-right seed 64) #xFFFFFFFFFFFFFFFF))
	     (rv	(capi.cityhash128-with-seed buf len seed-low seed-high))
	     (lo	(car rv))
	     (hi	(cdr rv)))
	(bitwise-ior lo (bitwise-arithmetic-shift-left hi 64)))))))

(define (Hash128to64 hash)
  (define who 'Hash128to64)
  (with-arguments-validation (who)
      ((word-u128	hash))
    (let ((lo	(bitwise-and hash #xFFFFFFFFFFFFFFFF))
	  (hi	(bitwise-and (bitwise-arithmetic-shift-right hash 64) #xFFFFFFFFFFFFFFFF)))
      (capi.cityhash-128-to-64 lo hi))))


;;;; done

)

;;; end of file
