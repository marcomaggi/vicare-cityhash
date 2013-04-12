;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/CityHash
;;;Contents: low-level bindings
;;;Date: Fri Apr 12, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare crypto cityhash unsafe-capi)
  (export

    ;; version functions
    vicare-cityhash-version-interface-current
    vicare-cityhash-version-interface-revision
    vicare-cityhash-version-interface-age
    vicare-cityhash-version

    ;; hash functions
    cityhash64
    cityhash64-with-seed
    cityhash64-with-seeds

    cityhash128
    cityhash128-with-seed
    cityhash-128-to-64)
  (import (vicare))


;;;; helpers

(define-syntax define-inline
  (syntax-rules ()
    ((_ (?name ?arg ... . ?rest) ?form0 ?form ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ ?arg ... . ?rest)
	  (begin ?form0 ?form ...)))))))


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

(define-inline (cityhash64 buf len)
  (foreign-call "ikrt_cityhash_cityhash64" buf len))

(define-inline (cityhash64-with-seed buf len seed)
  (foreign-call "ikrt_cityhash_cityhash64_with_seed" buf len seed))

(define-inline (cityhash64-with-seeds buf len seed0 seed1)
  (foreign-call "ikrt_cityhash_cityhash64_with_seeds" buf len seed0 seed1))

;;; --------------------------------------------------------------------

(define-inline (cityhash128 buf len)
  (foreign-call "ikrt_cityhash_cityhash128" buf len))

(define-inline (cityhash128-with-seed buf len seed-low seed-high)
  (foreign-call "ikrt_cityhash_cityhash128_with_seed" buf len seed-low seed-high))

(define-inline (cityhash-128-to-64 hash-low hash-high)
  (foreign-call "ikrt_cityhash_hash128to64" hash-low hash-high))


;;;; done

)

;;; end of file
