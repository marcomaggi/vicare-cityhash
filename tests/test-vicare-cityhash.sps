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
(import (vicare)
  (vicare cityhash)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing Vicare CityHash bindings\n")


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
    => #xB988A50F6B01559D)

  (check
      (CityHash64 #ve(ascii "ciao mamma") #f)
    => #xB988A50F6B01559D)

  (check
      (let ((S (string->guarded-cstring "ciao mamma")))
	(CityHash64 S (strlen S)))
    => #xB988A50F6B01559D)

;;; --------------------------------------------------------------------
;;; single seed

  (check
      (CityHash64 #ve(ascii "ciao mamma") #f 123)
    => #x24E1170C2D039861)

  (check
      (let ((S (string->guarded-cstring "ciao mamma")))
	(CityHash64 S (strlen S) 123))
    => #x24E1170C2D039861)

;;; --------------------------------------------------------------------
;;; double seed

  (check
      (CityHash64 #ve(ascii "ciao mamma") #f 123 456)
    => #xB9296FE69C35D124)

  (check
      (let ((S (string->guarded-cstring "ciao mamma")))
	(CityHash64 S (strlen S) 123 456))
    => #xB9296FE69C35D124)

  #t)


(parametrise ((check-test-name	'hash128))

  (check
      (CityHash128 #ve(ascii "ciao mamma"))
    => #x62BAF4C272013DD0FF9D3C18530BEC78)

  (check
      (CityHash128 #ve(ascii "ciao mamma") #f)
    => #x62BAF4C272013DD0FF9D3C18530BEC78)

  (check
      (let ((S (string->guarded-cstring "ciao mamma")))
	(CityHash128 S (strlen S)))
    => #x62BAF4C272013DD0FF9D3C18530BEC78)

;;; --------------------------------------------------------------------
;;; single seed

  (check
      (CityHash128 #ve(ascii "ciao mamma") #f 123)
    => #x4224D3B6232040727B00DDCFB50D81C4)

  (check
      (let ((S (string->guarded-cstring "ciao mamma")))
	(CityHash128 S (strlen S) 123))
    => #x4224D3B6232040727B00DDCFB50D81C4)

;;; --------------------------------------------------------------------

  (check
      (Hash128to64 #x4224D3B6232040727B00DDCFB50D81C4)
    => #x120ACCCE9824D272)

  #t)


;;;; done

(check-report)

;;; end of file
