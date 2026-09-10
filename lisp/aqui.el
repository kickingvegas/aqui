;;; aqui.el --- Location update (optimized for macOS) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Charles Y. Choi

;; Author: Charles Y. Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/aqui
;; Keywords: tools
;; Package-Version: 0.0.2-rc.1
;; Package-Requires: ((emacs "30.1"))

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

;; TBD

;;; Code:

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
        (setq aqui--last-result (format "􀋑 %s" "Running was cancelled")))

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
            (error "􀋑 Undefined aqui--last-result")))
        (error "􀋑 exit error")))))

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
         (msg (format "􀋒 %s (%.5f, %.5f)" location-name latitude longitude)))

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
         (msg (format "􀋒 %s (%.5f, %.5f)" city latitude longitude)))

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
  "Insert last LOCATION as an Org table."
  (interactive)
  (let* ((location (if (not location)
                       aqui--last-result
                     location)))
    (cond
     ((eq aqui-source :shortcuts)
      (aqui--insert-location-via-shortcuts location))

     ((eq aqui-source :ip-api)
      (aqui--insert-location-via-ip-api location)))))

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

(provide 'aqui)
;;; aqui.el ends here
