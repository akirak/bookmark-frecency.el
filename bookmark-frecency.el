;;; bookmark-frecency.el --- Sort bookmarks by frecency -*- lexical-binding: t -*-

;; Copyright (C) 2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
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
        (advice-add 'bookmark-all-names :filter-return #'bookmark-frecency--ad-all-names))
    (advice-remove 'bookmark-jump #'bookmark-frecency--ad-jump)
    (advice-remove 'bookmark-all-names #'bookmark-frecency--ad-all-names)))

(defun bookmark-frecency--ad-jump (record &rest _args)
  (require 'map)
  (let* ((name (cl-etypecase record
                 (string record)
                 (list (car record))))
         (alist (bookmark-get-bookmark-record name)))
    (setf (map-elt alist 'x-frecency-access-count)
          (1+ (or (map-elt alist 'x-frecency-access-count)
                  0)))
    (setf (map-elt alist 'x-frecency-last-access-time)
          (current-time))
    (bookmark-store name alist nil)))

(defun bookmark-frecency--ad-all-names (names)
  (require 'map)
  (thread-last
    names
    (mapcar (lambda (name)
              (cons name (bookmark-frecency--record-score name))))
    (seq-sort-by #'cdr #'>)
    (mapcar #'car)))

(defun bookmark-frecency--record-score (name)
  ;; For performance, this function accesses the alist directly rather than to
  ;; use the bookmark API such as `bookmark-get-last-modified',
  ;; `bookmark-prop-get', etc. This may cause breakage in future.
  (let* ((alist (bookmark-get-bookmark-record name))
         (count (or (map-elt alist 'x-frecency-access-count)
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
