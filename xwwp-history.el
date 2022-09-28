;;; xwwp-history.el --- history management for `xwwp' browser  -*- lexical-binding: t; -*-

;; Author: K. Scarlet
;; URL: https://github.com/canatella/xwwp
;; Created: 2020-08-20
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (xwwp "0.1") (ctable "0.1.2"))

;; Copyright (C) 2020 K. Scarlet <qhong@mit.edu>

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

(cl-defstruct xwwp-history-item url title last-time (visit-count 1)
              display-cache completion-item)

(defvar xwwp-history-table nil)
(defvar xwwp-history-completion-list nil)
(defvar xwwp-history-visualization-list nil)
(defvar xwwp-history-ctable-component nil)
(defun xwwp-history-item-completion-text (item)
  (let* ((title (xwwp-history-item-title item))
         (url (xwwp-history-item-url item))
         (text (concat title " (" url ")")))
    (put-text-property (+ 2 (length title)) (1- (length text)) 'face 'link text)
    text))
(defun xwwp-history-add-item (item)
  ""
  (let* ((url (xwwp-history-item-url item))
         (existed (gethash url xwwp-history-table)))
    (when existed
      (setq item existed)
      (incf (xwwp-history-item-visit-count existed)))
    (puthash url item xwwp-history-table)
    (when existed
      (setq xwwp-history-visualization-list
            (delq (xwwp-history-item-display-cache existed)
                  xwwp-history-visualization-list)))
    (setf (xwwp-history-item-display-cache item)
          (list (xwwp-history-item-title item)
                (format-time-string "%c" (xwwp-history-item-last-time item))
                (xwwp-history-item-url item)
                (xwwp-history-item-visit-count item)))
    (push (xwwp-history-item-display-cache item) xwwp-history-visualization-list)
    (when xwwp-history-ctable-component
      (setf (ctbl:model-data (ctbl:component-model xwwp-history-ctable-component))
            xwwp-history-visualization-list)
      (ctbl:cp-update xwwp-history-ctable-component))
    (unless existed
      (let ((text (xwwp-history-item-completion-text item)))
        (let ((completion-item (cons text url)))
          (setf (xwwp-history-item-completion-item item) completion-item)
          (push completion-item xwwp-history-completion-list))))))
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
  (setq xwwp-history-completion-list nil)
  (setq xwwp-history-visualization-list nil)
  (setq xwwp-history-ctable-component nil)
  (xwwp-history-load)
  nil)
(xwwp-history-initialize)
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
        (setf (car (xwwp-history-item-display-cache existed)) title)
        (setf (car (xwwp-history-item-completion-item existed)) (xwwp-history-item-completion-text existed))
        (when xwwp-history-ctable-component
          (ctbl:cp-update xwwp-history-ctable-component))))))
(advice-add 'xwidget-webkit-callback :after #'xwwp-history-xwidget-event-callback)
(defun xwwp-history-unload-function ()
  (advice-remove 'xwidget-webkit-callback #'xwwp-history-xwidget-event-callback))

(defgroup xwwp-history nil
  "`xwidget-webkit' history customizations."
  :group 'xwwp)
(defvar xwwp-history-key-map (make-sparse-keymap))
(define-key xwwp-history-key-map (kbd "RET")
  (lambda ()
    (interactive)
    (ctbl:navi-on-click)
    (xwwp
     (caddr
      (ctbl:cp-get-selected-data-row xwwp-history-ctable-component)))))
(defun xwwp-history-show ()
  ""
  (interactive)
  (unless xwwp-history-ctable-component
    (let* ((column-model
            (list (make-ctbl:cmodel :title "Title" :align 'left)
                  (make-ctbl:cmodel :title "Access Date" :align 'left)
                  (make-ctbl:cmodel :title "URL" :align 'left)
                  (make-ctbl:cmodel :title "Visit Count" :align 'right)))
           (model (make-ctbl:model
                   :column-model column-model
                   :data xwwp-history-visualization-list))
           (component (ctbl:create-table-component-buffer
                       :model model
                       :buffer (get-buffer-create "*XWWP history*")
                       :custom-map xwwp-history-key-map)))
      (setq xwwp-history-ctable-component component)))
  (pop-to-buffer (ctbl:cp-get-buffer xwwp-history-ctable-component))
  (rename-buffer "*XWWP history*"))
(provide 'xwwp-history)
;;; xwwp-history.el ends here
