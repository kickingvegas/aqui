;;; aqui.el --- Location update (optimized for macOS) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

;; Author: Charles Y. Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/aqui
;; Keywords: tools
;; Package-Version: 0.1.2-rc.1
;; Package-Requires: ((emacs "30.1") (restlib "0.1.0"))

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

;; Aquí (`aqui.el') is an Elisp library for updating the location for GNU
;; Emacs with optimization for high accuracy on macOS. On macOS, Aquí will
;; use the Shortcuts app to obtain a location update from the native OS
;; location service. This approach avoids needing a specialized third party
;; executable to accomplish the location lookup. If Shortcuts is not
;; available, Aquí will use a third party internet service to obtain
;; location information.

;; USAGE:

;; Run M-x aqui RET to update location.

;; Refer to the Aquí User Guide (URL `https://kickingvegas.github.io/aqui/') for
;; more information.


;;; Code:
(require 'solar)
(require 'restlib)


;;; Variables

(defgroup aqui nil
  "Group settings for Aquí."
  :group 'convenience)

(defcustom aqui-source :shortcuts
  "Aquí location source."
  :type '(choice (const :tag "Shortcuts" :shortcuts)
                 (const :tag "ip-api.com" :ip-api))
  :group 'aqui)

(defcustom aqui-glyph "📍"
  "Aquí glyph to use when messaging location update.

For SF Symbols support, run the command `aqui-setup-sf-symbols' to
initialize support for rendering SF Symbols."
  :type '(choice
          (const :tag "Pushpin 📍" "📍")
          (const :tag "SF Symbols 􀋒" "􀋒")
          (const :tag "Arrow ➚" "➚")
          (const :tag "Dot ⨀" "⨀")
          (const :tag "Plain *" "*")
          (string :tag "Other"))
  :group 'aqui)

(defcustom aqui-inactive-glyph "⊘"
  "Aquí glyph to use for unexpected result in requesting a location update.

For SF Symbols support, run the command `aqui-setup-sf-symbols' to
initialize support for rendering SF Symbols."
  :type '(choice
          (const :tag "Inactive ⊘" "⊘")
          (const :tag "SF Symbols 􀋑" "􀋑")
          (string :tag "Other"))
  :group 'aqui)

(defcustom aqui-map-provider :apple
  "Aquí map provider."
  :type '(choice (const :tag "Apple Maps" :apple)
                 (const :tag "Google Maps" :google))
  :group 'aqui)

(defun aqui-customize-group ()
  "Customize ‘aqui’ group."
  (interactive)
  (customize-group "aqui"))

(defvar aqui--last-result nil
  "Last search result.")


;;; Utils

(defun aqui-map-url-from-location (latitude longitude)
  "Generate map URL using LATITUDE and LONGITUDE."
  (cond
     ((eq aqui-map-provider :apple)
      (restlib-url-add-query-items
       "https://maps.apple.com/place"
       (list
        (list "coordinate" (format "%f,%f" latitude longitude))
        (list "map" "transit"))))

     ((eq aqui-map-provider :google)
      (restlib-url-add-query-items
       "https://www.google.com/maps/search/"
       (list
        (list "api" 1)
        (list "query" (format "%f,%f" latitude longitude)))))
     (t
      (restlib-url-add-query-items
       "https://www.google.com/maps/search/"
       (list
        (list "api" 1)
        (list "query" (format "%f,%f" latitude longitude)))))))


;;; macOS Shortcuts

(defun aqui--process-filter (_process output)
  "Process filter PROCESS and OUTPUT."
  (if (and output (stringp output))
      (cond
       ((string-match-p "^Error: Running was cancelled" output)
        (setq aqui--last-result (format "%s %s" aqui-inactive-glyph "Running was cancelled")))

       (t
        (let* ((response (json-parse-string output
                                            :null-object nil)))
          (mapc (lambda (key)
                  (restlib-json-empty-string-to-nil response key))
                '("street"
                  "city"
                  "state"
                  "zipcode"
                  "region"
                  "phone"
                  "label"
                  "url"
                  "name"))

          (map-put! response "created"
                    (format-time-string "%Y-%m-%d %a %H:%M %Z"))

          (setq aqui--last-result response))))))

(defun aqui--process-sentinel (process signal)
  "Process sentinel for PROCESS and SIGNAL."
  (when (string-match-p "finished\\|exited" signal)
    (let ((exit-code (process-exit-status process)))
      (if (= exit-code 0)
          (cond
           ((stringp aqui--last-result)
            (message aqui--last-result))

           ((hash-table-p aqui--last-result)
            (let ((msg (aqui-process-location-shortcuts aqui--last-result)))
              (kill-new msg)
              (message msg)))
           (t
            (error "%s Undefined aqui--last-result" aqui-inactive-glyph)))
        (error "%s exit error" aqui-inactive-glyph)))))

(defun aqui--shortcuts ()
  "Get current location via Shortcuts."
  (let ((proc (start-process "aqui"
                             nil
                             "sh" "-c"
                             "shortcuts run 'Current Location JSON' | cat")))
    (set-process-filter proc #'aqui--process-filter)
    (set-process-sentinel proc #'aqui--process-sentinel)))

(defun aqui-process-location-shortcuts (location)
  "Process LOCATION."

  (let* ((location (if (not location)
                           aqui--last-result
                         location))
         (latitude (gethash "latitude" location))
         (longitude (gethash "longitude" location))
         (city (gethash "city" location))
         (street (gethash "street" location))
         (location-name (if (and street city)
                            (format "%s, %s" street city)
                          city))
         (msg (format "%s %s (%.5f, %.5f)" aqui-glyph location-name latitude longitude)))

    (setopt calendar-latitude latitude)
    (setopt calendar-longitude longitude)
    (setopt calendar-location-name location-name)
    msg))

(defun aqui--insert-location-via-shortcuts (location)
  "Insert last LOCATION as an Org table."
  (let* ((latitude (gethash "latitude" location))
         (longitude (gethash "longitude" location))
         (maps-url (aqui-map-url-from-location latitude longitude))
         (buflist '("|---|---|" "| Property | Value |")))

    (mapc (lambda (key)
            (let* ((value (gethash key location))
                   (label (capitalize key))
                   (fvalue
                    (cond
                     ((stringp value)
                      (format "| %s | %s |" label value))

                     ((numberp value)
                      (format "| %s | %f |" label value))

                     (t
                      (format "| %s |  |" label)))))

              (push fvalue buflist)))

          '("latitude"
            "longitude"
            "altitude"
            "created"
            "street"
            "city"
            "state"
            "zipcode"
            "region"
            "phone"
            "label"
            "url"
            "name"))

    (push (format "| Maps URL | [[%s][%f, %f]] | "
                  maps-url
                  latitude
                  longitude)
          buflist)
    (save-excursion
      (insert (string-join (reverse buflist) "\n")))))



;;; ip-api.com

(defun aqui--insert-location-via-ip-api (location)
  "Insert last LOCATION as an Org table."
  (let* ((latitude (gethash "lat" location))
         (longitude (gethash "lon" location))
         (maps-url (aqui-map-url-from-location latitude longitude))
         (buflist '("|---|---|" "| Property | Value |")))

    (mapc (lambda (key)
            (let* ((value (gethash key location))
                   (label (capitalize key))
                   (fvalue
                    (cond
                     ((stringp value)
                      (format "| %s | %s |" label value))

                     ((numberp value)
                      (format "| %s | %f |" label value))

                     (t
                      (format "| %s |  |" label)))))

              (push fvalue buflist)))

          '("lat"
            "lon"
            "created"
            "city"
            "regionName"
            "country"))

    (push (format "| Maps URL | [[%s][%f, %f]] | "
                  maps-url
                  latitude
                  longitude)
          buflist)
    (save-excursion
      (insert (string-join (reverse buflist) "\n")))))

(defun aqui--ip-api ()
  "Get current location via URL `http://ip-api.com'."

  (let* ((url "http://ip-api.com/json/?fields=lat,lon,city,regionName,country")
         (location (restlib-fetch-json url))
         (latitude (gethash "lat" location))
         (longitude (gethash "lon" location))
         (city (gethash "city" location))
         (msg (format "%s %s (%.5f, %.5f)" aqui-glyph city latitude longitude)))

    (map-put! location "created" (format-time-string "%Y-%m-%d %a %H:%M %Z"))
    (setq aqui--last-result location)
    (setopt calendar-latitude latitude)
    (setopt calendar-longitude longitude)
    (setopt calendar-location-name city)
    (kill-new msg)
    (message "%s" msg)))



;;; Commands

;;;###autoload (autoload 'aqui "aqui" nil t)
(defun aqui ()
  "Update current geographic location from `aqui-source'.

The following variables are run-time updated given a location update.

- variable `calendar-latitude'
- variable `calendar-longitude'
- variable `calendar-location-name'

Use the command `aqui-customize-save-location-data' to persist these
variables to a file."
  (interactive)
  (cond
   ((eq aqui-source :shortcuts)
    (aqui--shortcuts))

   ((eq aqui-source :ip-api)
    (aqui--ip-api))))

(defun aqui-insert-location (&optional location)
  "Insert last obtained LOCATION as an Org table.

- LOCATION: Location object

This command requires that the command `aqui' be run beforehand."
  (interactive)
  (let ((location (if (and (not location) aqui--last-result)
                      aqui--last-result
                    location)))
    (if aqui--last-result
        (cond
         ((eq aqui-source :shortcuts)
          (aqui--insert-location-via-shortcuts location))

         ((eq aqui-source :ip-api)
          (aqui--insert-location-via-ip-api location)))
      (error "Error: No location data to insert. Run ‘aqui’ and try again"))))

(defun aqui-customize-save-location-data ()
  "Persist location data in calendar location variables.

The following variables are persisted given a location update.

- variable `calendar-latitude'
- variable `calendar-longitude'
- variable `calendar-location-name'"
  (interactive)
  (customize-save-variable 'calendar-latitude calendar-latitude)
  (customize-save-variable 'calendar-longitude calendar-longitude)
  (customize-save-variable 'calendar-location-name calendar-location-name))

(defun aqui-setup-sf-symbols ()
  "Setup usage of SF Symbols for glyphs.

On macOS, the variables `aqui-glyph' and `aqui-inactive-glyph' can be
configured to use SF Symbols. Run this command to correctly display
these symbols in a GUI frame."
  (interactive)
  (if (not (eq system-type 'darwin))
        (error "Only supported on macOS")
      (if (and (display-graphic-p) (fboundp 'set-fontset-font))
          (set-fontset-font t '(?􀀀 . ?􏿽) "SF Pro Display"))))

(provide 'aqui)
;;; aqui.el ends here
