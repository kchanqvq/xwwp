;;; xwwp-history.el --- history management for `xwwp' browser  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Q. Hong <qhong@alum.mit.edu>

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
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

;; Implement a history manager for `xwidget-webkit' sessions.

;;; Code:

(require 'xwwp)
(require 'eieio)
(require 'cl-lib)
(require 'ctable)

(defgroup xwwp-history nil
  "`xwidget-webkit' history customizations."
  :group 'xwwp)

(defcustom xwwp-history-filename "~/.xwwp-history"
  "File to store history of `xwidget-webkit' sessions."
  :type 'file
  :group 'xwwp-history)

(cl-defstruct xwwp-history-item url title last-time (visit-count 1))

(defvar xwwp-history-table nil "A hashtable that maps URL to `xwwp-history-item'")
(defun xwwp-history-item-completion-text (item)
  "Format string used for completion for ITEM."
  (let* ((title (xwwp-history-item-title item))
         (url (xwwp-history-item-url item))
         (text (concat title " (" url ")")))
    (put-text-property (+ 2 (length title)) (1- (length text)) 'face 'link text)
    text))
(defun xwwp-history-add-item (item)
  "Add ITEM to `xwwp-history-table'.

ITEM should be a `xwwp-history-item'."
  (let* ((url (xwwp-history-item-url item))
         (existed (gethash url xwwp-history-table)))
    (when existed
      (cl-incf (xwwp-history-item-visit-count item)
            (xwwp-history-item-visit-count existed)))
    (puthash url item xwwp-history-table)
    (when (and xwwp-history-buffer (buffer-live-p xwwp-history-buffer))
      (xwwp-history-refresh))))
(defun xwwp-history-item-serialize (item)
  (list (xwwp-history-item-url item)
        (xwwp-history-item-title item)
        (xwwp-history-item-last-time item)
        (xwwp-history-item-visit-count item)))
(defun xwwp-history-item-deserialize (list)
  (make-xwwp-history-item
   :url (car list)
   :title (cadr list)
   :last-time (caddr list)
   :visit-count (cadddr list)))
(defun xwwp-history-load ()
  (with-current-buffer (find-file-noselect xwwp-history-filename)
    (beginning-of-buffer)
    (condition-case nil
        (while t
          (xwwp-history-add-item (xwwp-history-item-deserialize (read (current-buffer)))))
      (end-of-file nil))
    (kill-buffer)))

(defun xwwp-history-initialize ()
  "Setup required data structure and load history from XWWP-HISTORY-FILENAME."
  (setq xwwp-history-table (make-hash-table :test 'equal))
  (xwwp-history-load))
(defun xwwp-history-commit ()
  "Compact history log in XWWP-HISTORY-FILENAME."
  (interactive)
  (with-temp-file xwwp-history-filename
    (maphash (lambda (key item)
               (insert (format "%S\n" (xwwp-history-item-serialize item))))
             xwwp-history-table)))
(add-hook 'kill-emacs-hook #'xwwp-history-commit)
(defun xwwp-history-xwidget-event-callback (xwidget xwidget-event-type)
  (let ((url (xwidget-webkit-uri xwidget))
        (title (xwidget-webkit-title xwidget)))
    ;; Some webpage doesn't trigger load-changed event after loading,
    ;; so test if status is changed (by comparing to cached value
    ;; in xwidget's plist property 'last-cached-url)
    ;; after any xwidget events.
    (when (or (eq xwidget-event-type 'load-changed)
              (not (equal (xwidget-get xwidget 'last-cached-url) url)))
      (xwidget-put xwidget 'last-cached-url url)
      (let ((new-item
             (make-xwwp-history-item
              :title title :url url
              :last-time (current-time))))
        (xwwp-history-add-item new-item)
        (let ((save-silently t))
          (append-to-file
           (format "%S\n" (xwwp-history-item-serialize new-item))
           nil xwwp-history-filename))))
    ;; Sometimes the above hack failed to retrieve the correct (updated) title,
    ;; so try to fix title at every xwidget event
    (let ((existed (gethash url xwwp-history-table)))
      (unless (equal (xwwp-history-item-title existed) title)
        (setf (xwwp-history-item-title existed) title)
        (when xwwp-history-ctable-component
          (ctbl:cp-update xwwp-history-ctable-component))))))
(advice-add 'xwidget-webkit-callback :after #'xwwp-history-xwidget-event-callback)
(defun xwwp-history-unload-function ()
  (advice-remove 'xwidget-webkit-callback #'xwwp-history-xwidget-event-callback))

(defgroup xwwp-history nil
  "`xwidget-webkit' history customizations."
  :group 'xwwp)
(defvar xwwp-history-buffer nil)
(define-derived-mode xwwp-history-mode tabulated-list-mode
  "XWWP History"
  (setq tabulated-list-format [("Title" 40 t)
                               ("URL" 40 t)
                               ("Access Date" 21 t)
                               ("Visit Count" 7 t)]))
(defvar xwwp-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'xwwp-history-goto)
    (define-key map [mouse-1] #'xwwp-history-goto)
    map))
(defun xwwp-history-goto ()
  "Goto XWWP history item at point."
  (interactive)
  (xwwp (tabulated-list-get-id)))
(defun xwwp-history-refresh ()
  (with-current-buffer xwwp-history-buffer
    (xwwp-history-mode)
    (setq tabulated-list-entries nil)
    (maphash (lambda (k item)
               (push (list k (vector (xwwp-history-item-title item)
                                     (xwwp-history-item-url item)
                                     (format-time-string "%Y-%m-%d %a %H:%M" (xwwp-history-item-last-time item))
                                     (number-to-string (xwwp-history-item-visit-count item))))
                     tabulated-list-entries))
             xwwp-history-table)
    (tabulated-list-init-header)
    (tabulated-list-print)))
(defun xwwp-history-show ()
  "Show XWWP history buffer."
  (interactive)
  (unless (and xwwp-history-buffer (buffer-live-p xwwp-history-buffer))
    (setq xwwp-history-buffer (get-buffer-create "*XWWP history*")))
  (xwwp-history-refresh)
  (pop-to-buffer xwwp-history-buffer))

(xwwp-history-initialize)

(provide 'xwwp-history)
;;; xwwp-history.el ends here
