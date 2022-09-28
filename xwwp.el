;;; xwwp.el --- Enhance xwidget webkit browser -*- lexical-binding: t; -*-

;; Author: Damien Merenne, Q. Hong
;; URL: https://github.com/canatella/xwwp
;; Created: 2020-03-11
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;; Copyright (C) 2020 Damien Merenne <dam@cosinux.org>, Q. Hong <qhong@mit.edu>

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

;; This package provides the common functionnality for other xwidget webkit plus
;; packages.  It provides the customize group and a framework to inject css and
;; javascript functions into an `xwidget-webkit' session.

;;; Code:

(defgroup xwwp nil
  "`xwidget-webkit' browser enhancement suite."
  :group 'convenience)



(require 'json)
(require 'subr-x)
(require 'xwidget)

(defconst xwwp-source-directory (file-name-directory (or load-file-name buffer-file-name)))

(defun xwwp-css-make-class (class style)
  "Generate a css CLASS definition from the STYLE alist."
  (format ".%s { %s }\\n" class (mapconcat (lambda (v) (format "%s: %s;" (car v) (cdr v))) style " ")))

(defmacro xwwp--js (js _ &rest replacements)
  "Apply `format' on JS with REPLACEMENTS  providing MMM mode delimiters.

This file has basic support for javascript using MMM mode and
local variables (see at the end of the file)."
  (declare (indent 2))
  `(format ,js ,@replacements))

(defun xwwp-js-string-escape (string)
  "Escape STRING for injection."
  (replace-regexp-in-string "\n" "\\\\n" (replace-regexp-in-string "'" "\\\\'" string)))

(defun xwwp-html-inject-head-element (xwidget tag id type content)
  "Insert TAG element under XWIDGET head with ID TYPE and CONTENT."
  (let* ((id (xwwp-js-string-escape id))
         (tag (xwwp-js-string-escape tag))
         (type (xwwp-js-string-escape type))
         (content (xwwp-js-string-escape content))
         (script (xwwp--js "
__xwidget_id = '%s';
if (!document.getElementById(__xwidget_id)) {
    var e = document.createElement('%s');
    e.type = '%s';
    e.id = __xwidget_id;
    e.innerHTML = '%s';
    document.getElementsByTagName('head')[0].appendChild(e);
};
null;
" js-- id tag type content)))
    (xwidget-webkit-execute-script xwidget script)))

(defun xwwp-html-inject-script (xwidget id script)
  "Inject javascript SCRIPT in XWIDGET session using a script element with ID."
  (xwwp-html-inject-head-element xwidget "script" id "text/javascript" script))

(defun xwwp-html-inject-style (xwidget id style)
  "Inject css STYLE in XWIDGET session using a style element with ID."
  (xwwp-html-inject-head-element xwidget "style" id "text/css" style))

(defun xwwp-js-lisp-to-js (identifier)
  "Convert IDENTIFIER from Lisp style to javascript style."
  (replace-regexp-in-string "-" "_" (if (symbolp identifier) (symbol-name identifier) identifier)))

(defvar xwwp-js-scripts '() "An  alist of list of javascript function.")

(defun xwwp-js-register-function (ns-name name js-script)
  "Register javascript function NAME in namespace NS-NAME with body JS-SCRIPT."
  (let* ((namespace (assoc ns-name xwwp-js-scripts))
         (fun (when namespace (assoc name (cdr namespace)))))
    (cond (fun
           (delete fun namespace)
           (xwwp-js-register-function ns-name name js-script))
          ((not namespace)
           (push (cons ns-name '()) xwwp-js-scripts)
           (xwwp-js-register-function ns-name name js-script))
          (t
           (push (cons name js-script) (cdr namespace))))
    (cons ns-name name)))

(defun xwwp-js-funcall (xwidget namespace name &rest arguments)
  "Invoke javascript function NAME in XWIDGET instance passing ARGUMENTS witch CALLBACK in NAMESPACE."
  ;;; Try to be smart
  (let* ((callback (car (last arguments)))
         (arguments (if (functionp callback) (reverse (cdr (reverse arguments))) arguments))
         (json-args (seq-map #'json-encode arguments))
         (arg-string (string-join json-args ", "))
         (namespace (xwwp-js-lisp-to-js namespace))
         (name (xwwp-js-lisp-to-js name))
         (script (format "__xwidget_plus_%s_%s(%s)" namespace name arg-string)))
    (xwidget-webkit-execute-script xwidget script (and (functionp callback) callback))))

(defmacro xwwp-js-def (namespace name arguments docstring js-body)
  "Create a function NAME with ARGUMENTS, DOCSTRING and JS-BODY.

This will define a javascript function in the namespace NAMESPACE
and a Lisp function to call it."
  (declare (indent 3) (doc-string 4))
  (let* ((js-arguments (seq-map #'xwwp-js-lisp-to-js arguments))
         (js-name (xwwp-js-lisp-to-js name))
         (js-namespace (xwwp-js-lisp-to-js namespace))
         (lisp-arguments (append '(xwidget) arguments '(&optional callback)))
         (script (xwwp--js "function __xwidget_plus_%s_%s(%s) {%s};" js--
                   js-namespace js-name (string-join js-arguments ", ") (eval js-body)))
         (lisp-def  `(defun ,(intern (format "xwwp-%s-%s" namespace name)) ,lisp-arguments
                       ,docstring
                       (xwwp-js-funcall xwidget (quote ,namespace) (quote ,name) ,@arguments callback)))
         (lisp-store `(xwwp-js-register-function (quote ,namespace) (quote ,name) ,script)))
    `(progn ,lisp-def ,lisp-store)))

(defun xwwp-js-inject (xwidget ns-name)
  "Inject the functions defined in NS-NAME into XWIDGET session."
  (let* ((namespace (assoc ns-name xwwp-js-scripts))
         (script (mapconcat #'cdr (cdr namespace) "\n")))
    (xwwp-html-inject-script xwidget (format "--xwwp-%s" (symbol-name ns-name)) script)))

;;;###autoload
(defun xwwp-browse-url-other-window (url &optional new-session)
  "Ask xwidget-webkit to browse URL.
NEW-SESSION specifies whether to create a new xwidget-webkit session.
Interactively, URL defaults to the string looking like a url around point."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: "
                                             ;;(xwidget-webkit-current-url)
                                             )))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))
  (when (stringp url)
    (if new-session
        (xwidget-webkit-new-session url)
      (progn (xwidget-webkit-goto-url url)
             (switch-to-buffer-other-window (xwidget-buffer (xwidget-webkit-current-session)))))))

;;; Adapted from EWW code to provide a DWIM style XWWP command
(require 'eww)
(require 'puny)

(defcustom xwwp-search-prefix "https://google.com/search?q="
  "Prefix URL to search engine."
  :group 'xwwp
  :type 'string)

(defun xwwp (url &optional arg)
  "Fetch URL and render the page.
If the input doesn't look like an URL or a domain name, the
word(s) will be searched for via `xwwp-search-prefix'.

If called with a prefix ARG, create a new Webkit buffer instead of reusing
the default Webkit buffer."
  (interactive
   (let ((prompt "Enter URL or keywords: "))
     (list (if (require 'xwwp-history nil t)
               (let ((cand (completing-read prompt xwwp-history-completion-list)))
                 (cdr (assoc cand xwwp-history-completion-list)))
             (read-string prompt nil 'eww-prompt-history ""))
           (prefix-numeric-value current-prefix-arg))))
  (when url
    (setq url
          (let ((eww-search-prefix xwwp-search-prefix))
            (eww--dwim-expand-url url)))
    ;; Check whether the domain only uses "Highly Restricted" Unicode
    ;; IDNA characters.  If not, transform to punycode to indicate that
    ;; there may be funny business going on.
    (let ((parsed (url-generic-parse-url url)))
      (when (url-host parsed)
        (unless (puny-highly-restrictive-domain-p (url-host parsed))
          (setf (url-host parsed) (puny-encode-domain (url-host parsed)))))
      ;; When the URL is on the form "http://a/../../../g", chop off all
      ;; the leading "/.."s.
      (when (url-filename parsed)
        (while (string-match "\\`/[.][.]/" (url-filename parsed))
          (setf (url-filename parsed) (substring (url-filename parsed) 3))))
      (setq url (url-recreate-url parsed))))
  (xwwp-browse-url-other-window url (eq arg 4)))

(provide 'xwwp)
;;; xwwp.el ends here
