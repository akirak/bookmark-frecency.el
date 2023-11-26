;;; bookmark-frecency.el --- Sort bookmarks by frecency -*- lexical-binding: t -*-

;; Copyright (C) 2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience
;; URL: https://github.com/akirak/bookmark-frecency.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library exposes `bookmark-frecency-mode', which is a global mode that
;; tracks every visit to a bookmark and sorts bookmarks by frecency, using the
;; algorithm described in
;; <https://slack.engineering/a-faster-smarter-quick-switcher/.

;; To retrieve a sorted list of bookmarks, you have to use `bookmark-all-names'.
;; `bookmark-alist' variable itself is not sorted by this library.

;; On every visit to a bookmark, the visited bookmark will be updated with the
;; information needed to calculate the frecency score later, which should be
;; persisted to `bookmark-file'. To control when to save the bookmarks,
;; customize `bookmark-save-flag'.

;;; Code:

(eval-when-compile
  (require 'seq)
  (require 'map)
  (require 'subr-x))

(defvar bookmark-alist)
(defvar bookmark-sort-flag)
(declare-function bookmark-store "bookmark")
(declare-function bookmark-get-bookmark-record "bookmark")
(declare-function cl-etypecase "cl-macs")
(declare-function map-insert "map")
(declare-function map-elt "map")

(defgroup bookmark-frecency
  nil
  "Sort bookmarks by frecency."
  :group 'bookmark)

;;;###autoload
(define-minor-mode bookmark-frecency-mode
  "Sort bookmarks by frecency."
  :global t
  (if bookmark-frecency-mode
      (progn
        (advice-add 'bookmark-jump :after #'bookmark-frecency--ad-jump)
        (advice-add 'bookmark-maybe-sort-alist :around #'bookmark-frecency--ad-maybe-sort-alist)
        (advice-add 'bookmark-all-names :filter-return #'bookmark-frecency--ad-all-names))
    (advice-remove 'bookmark-jump #'bookmark-frecency--ad-jump)
    (advice-remove 'bookmark-maybe-sort-alist #'bookmark-frecency--ad-maybe-sort-alist)
    (advice-remove 'bookmark-all-names #'bookmark-frecency--ad-all-names)))

(defun bookmark-frecency--ad-jump (record &rest _args)
  "An advice function activated in `bookmark-frecency-mode'.

RECORD is a bookmark record, as in the first argument of `bookmark-jump'."
  (require 'map)
  (require 'cl-lib)
  (let* ((name (cl-etypecase record
                 (string record)
                 (list (car record))))
         (alist (copy-alist (bookmark-get-bookmark-record name))))
    (setf (map-elt alist 'x-frecency-access-count)
          (1+ (or (map-elt alist 'x-frecency-access-count)
                  0))
          (map-elt alist 'x-frecency-last-access-time)
          (current-time))
    (bookmark-store name alist nil)))

(defun bookmark-frecency--ad-maybe-sort-alist (orig-fn)
  "An advice function activated in `bookmark-frecency-mode'.

ORIG-FN is supposed to be the original implementation of
`bookmark-maybe-sort-alist'."
  ;; I thought about adding a non-official option for `bookmark-sort-flag', but
  ;; it was not possible. To display entries in `bookmark-bmenu-mode', the value
  ;; of the variable must be set strictly to one of the values defined using
  ;; `defcustom'.
  (if (eq bookmark-sort-flag 'last-modified)
      (thread-last
        (copy-alist bookmark-alist)
        (mapcar (lambda (cell)
                  (cons (bookmark-frecency--record-score (cdr cell))
                        cell)))
        (seq-sort-by #'car #'>)
        (mapcar #'cdr))
    (funcall orig-fn)))

(defun bookmark-frecency--ad-all-names (names)
  "An advice function activated in `bookmark-frecency-mode'.

NAMES should be a list of all current bookmark names, as returned
by `bookmark-all-names'."
  (require 'map)
  (require 'seq)
  (thread-last
    names
    (mapcar (lambda (name)
              (cons name
                    (bookmark-frecency--record-score (bookmark-get-bookmark-record name)))))
    (seq-sort-by #'cdr #'>)
    (mapcar #'car)))

(defun bookmark-frecency--record-score (alist)
  "Calculate the frecency score from the ALIST of a bookmark record."
  ;; For performance, this function accesses the alist directly rather than to
  ;; use the bookmark API such as `bookmark-get-last-modified',
  ;; `bookmark-prop-get', etc. This may cause breakage in future.
  (let* ((count (or (map-elt alist 'x-frecency-access-count)
                    1))
         (access-time (map-elt alist 'x-frecency-last-access-time))
         (mod-time (map-elt alist 'last-modified))
         (unix-time (max (if access-time
                             (float-time access-time)
                           0)
                         (if mod-time
                             (float-time mod-time)
                           0))))
    (/ (* count
          (bookmark-frecency--score unix-time))
       (min count 10))))

(defun bookmark-frecency--score (unix-time)
  "Calculate the qualifier from a UNIX-TIME for calculating frecency."
  (let ((secs (- (float-time) unix-time)))
    (cond
     ((<= secs 14400)
      100)
     ((<= secs 86400)
      80)
     ((<= secs 259200)
      60)
     ((<= secs 604800)
      40)
     ((<= secs 2419200)
      20)
     ((<= secs 7776000)
      10)
     (t
      0))))

(provide 'bookmark-frecency)
;;; bookmark-frecency.el ends here
