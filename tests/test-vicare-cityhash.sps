;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/CityHash
;;;Contents: tests for CityHash bindings
;;;Date: Mon Feb 13, 2012
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
(import (vicare)
  (vicare crypto cityhash)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare CityHash bindings\n")


;;;; helpers



(parametrise ((check-test-name	'version))

  (check
      (fixnum? (vicare-cityhash-version-interface-current))
    => #t)

  (check
      (fixnum? (vicare-cityhash-version-interface-revision))
    => #t)

  (check
      (fixnum? (vicare-cityhash-version-interface-age))
    => #t)

  (check
      (string? (vicare-cityhash-version))
    => #t)

  #t)


(parametrise ((check-test-name	'hash64))

  (check
      (CityHash64 #ve(ascii "ciao mamma"))
    => #x129FE6F62BE2F237)

  (check
      (CityHash64 #ve(ascii "ciao mamma") #f)
    => #x129FE6F62BE2F237)

  (check
      (let ((S (string->guarded-cstring "ciao mamma")))
	(CityHash64 S (strlen S)))
    => #x129FE6F62BE2F237)

;;; --------------------------------------------------------------------
;;; single seed

  (check
      (CityHash64 #ve(ascii "ciao mamma") #f 123)
    => #xCF727D17C9E06012)

  (check
      (let ((S (string->guarded-cstring "ciao mamma")))
	(CityHash64 S (strlen S) 123))
    => #xCF727D17C9E06012)

;;; --------------------------------------------------------------------
;;; double seed

  (check
      (CityHash64 #ve(ascii "ciao mamma") #f 123 456)
    => #xE6702B1C592DA870)

  (check
      (let ((S (string->guarded-cstring "ciao mamma")))
	(CityHash64 S (strlen S) 123 456))
    => #xE6702B1C592DA870)

  #t)


(parametrise ((check-test-name	'hash128))

  (check
      (CityHash128 #ve(ascii "ciao mamma"))
    => #x327A323FD9BD7856047BBC9B617D7F8F)

  (check
      (CityHash128 #ve(ascii "ciao mamma") #f)
    => #x327A323FD9BD7856047BBC9B617D7F8F)

  (check
      (let ((S (string->guarded-cstring "ciao mamma")))
	(CityHash128 S (strlen S)))
    => #x327A323FD9BD7856047BBC9B617D7F8F)

;;; --------------------------------------------------------------------
;;; single seed

  (check
      (CityHash128 #ve(ascii "ciao mamma") #f 123)
    => #xC1AF3615F606F6FCF940A0957F5AE34D)

  (check
      (let ((S (string->guarded-cstring "ciao mamma")))
	(CityHash128 S (strlen S) 123))
    => #xC1AF3615F606F6FCF940A0957F5AE34D)

;;; --------------------------------------------------------------------

  (check
      (Hash128to64 #x4224D3B6232040727B00DDCFB50D81C4)
    => #x120ACCCE9824D272)

  #t)


;;;; done

(check-report)

;;; end of file
