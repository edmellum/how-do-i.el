;;; how-do-i.el --- Instant coding answers in Emacs

;; Copyright (C) 2013  David Eduardo Mellum <david@edmellum.com>

;; Author: David Eduardo Mellum <david@edmellum.com>
;; Keywords: stackoverflow, answers
;; Version: 0.5.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; You should always be able to find the latest version here:

;;           <URL:http://github.com/edmellum/how-do-i.el/>

(require 'url)
(require 'json)

(defun get-json (buf)
  "Extract json from buffer."
  (with-current-buffer buf
    (goto-char url-http-end-of-headers)
    (prog1 (json-read)
      (kill-buffer buf))))

(defsubst google-result-field (key json)
  "Fetch KEY's value from JSON, a parsed JSON structure."
  (cdr (assoc key json)))

(defun google-result-urls (results)
  "Extract a list of search result URLs from RESULTS."
  (let* ((responseData (google-result-field 'responseData results))
         (records (google-result-field 'results responseData)))
    (mapcar (lambda (record)
              (google-result-field 'url record))
            records)))

(defun get-google-urls (query)
  "Get result URLs for a Google search."
  (let ((url-request-extra-headers
   `(("Accept" . "application/json"))))
    (google-result-urls
     (get-json
      (url-retrieve-synchronously
       (concat
	"http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q="
	(url-hexify-string
	 (concat "site:stackoverflow.com " query))))))))

(defun get-answer (urls)
  "Get the most voted StackOverflow answer from a list of URLs to
questions."
  (let ((url-request-extra-headers
	 `(("Accept" . "text/html"))))
    (let ((buf (url-retrieve-synchronously (car urls))))
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(re-search-forward "class=\"answer" nil 'move)
	(re-search-forward "\<code\>" nil t)
	(delete-region (point-min) (point))
	(re-search-forward "\<\/code\>" nil t)
	(end-of-line)
	(delete-region (point) (point-max))
	buf))))

;;;###autoload
(defun howdoi (question)
  "Get an answer to given programming question.

Googles given question limited to StackOverflow then gets the top
voted answer to the question."
  (interactive "sQuestion: ")
  (set-buffer
   (get-answer
    (get-google-urls question)))
  (message
   (buffer-string)))
