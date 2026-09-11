;;; test-aqui.el --- Casual Suite Tests      -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'aqui-test-utils)
(require 'aqui)

(ert-deftest test-aqui-group ()
  "Test `aqui' group."
  (should (get 'aqui 'custom-group)))

(ert-deftest test-aqui-source ()
  "Test variable `aqui-source'."
  (should (eq aqui-source :shortcuts)))

(ert-deftest test-aqui-map-provider ()
  "Test variable `aqui-map-provider'."
  (should (eq aqui-map-provider :apple)))

(ert-deftest test-aqui-map-url-from-location ()
  "Test for `aqui-map-url-from-location'."

  (let* ((lat 3.3)
         (lon 199.2)
         (aqui-map-provider :apple)
         (control "https://maps.apple.com/place?coordinate=3.300000,199.200000&map=transit")
         (result (aqui-map-url-from-location lat lon)))

    (should (string-equal control result)))

  (let* ((lat 3.3)
         (lon 199.2)
         (aqui-map-provider :google)
         (control "https://www.google.com/maps/search/?api=1&query=3.300000,199.200000")
         (result (aqui-map-url-from-location lat lon)))

    (should (string-equal control result))))


(provide 'test-aqui)
;;; test-aqui.el ends here
