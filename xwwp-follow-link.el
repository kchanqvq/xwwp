;;; xwwp-follow-link.el --- Link navigation in `xwidget-webkit' sessions -*- lexical-binding: t; -*-

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

;; Add support for navigating web pages in `xwidget-webkit' sessions using the
;; minibuffer completion.

;;; Code:

(require 'xwidget)
(require 'xwwp)
(require 'cl-lib)

(defgroup xwwp-follow-link nil
  "`xwidget-webkit' follow link customizations."
  :group 'xwwp)

(defcustom xwwp-follow-link-candidate-style '(("border" . "1px dashed blue")
                                              ("background" . "#0000ff20"))
  "Style to apply to candidate links."
  :type '(list (cons string string))
  :group 'xwwp-follow-link)

(defcustom xwwp-follow-link-selected-style '(("border" . "1px dashed red")
                                             ("background" . "#ff000020"))
  "Style to apply to currently selected link."
  :type '(list (cons string string))
  :group 'xwwp-follow-link)

(defun xwwp-follow-link-style-definition ()
  "Return the css definitions for the follow link feature."
  (concat (xwwp-css-make-class "xwwp-follow-link-candidate" xwwp-follow-link-candidate-style)
          (xwwp-css-make-class "xwwp-follow-link-selected" xwwp-follow-link-selected-style)))

(xwwp-js-def follow-link cleanup ()
  "Remove all custom class from links.""
window.__xwidget_plus_follow_link_candidates.forEach(a => {
    a.classList.remove('xwwp-follow-link-candidate', 'xwwp-follow-link-selected');
});
window.__xwidget_plus_follow_link_candidates = null;
")

(xwwp-js-def follow-link highlight (ids selected)
  "Highlight IDS as candidate and SELECTED as selected.""
window.__xwidget_plus_follow_link_candidates.forEach((a, id) => {
    a.classList.remove('xwwp-follow-link-candidate', 'xwwp-follow-link-selected');
    if (selected == id) {
        a.classList.add('xwwp-follow-link-selected');
        a.scrollIntoView({behavior: 'smooth', block: 'center'});
    } else if (ids && ids.includes(id)) {
        a.classList.add('xwwp-follow-link-candidate');
    }
});
")

(xwwp-js-def follow-link action (link-id)
  "Click on the link identified by LINK-ID""
let selected = window.__xwidget_plus_follow_link_candidates[link_id];
__xwidget_plus_follow_link_cleanup();
selected.click();
")

(xwwp-js-def follow-link fetch-links ()
  "Fetch all visible, non empty links from the current page.""
var r = {};
window.__xwidget_plus_follow_link_candidates = Array.from(document.querySelectorAll('a'));
window.__xwidget_plus_follow_link_candidates.forEach((a, i) => {
    if (a.offsetWidth || a.offsetHeight || a.getClientRects().length) {
        if (a.innerText.match(/\\\\S/))
            r[i] = [a.innerText, a.href];
    }
});
return r;
")


(defun xwwp-follow-link-format-link (str)
  "Format link title STR."
  (setq str (replace-regexp-in-string "^[[:space:][:cntrl:]]+" "" str))
  (setq str (replace-regexp-in-string "[[:space:][:cntrl:]]+$" "" str))
  (setq str (replace-regexp-in-string "[[:cntrl:]]+" "/" str))
  (replace-regexp-in-string "[[:space:]]+" " " str))

(defun xwwp-follow-link-prepare-links (links)
  "Prepare the alist of LINKS."
  (seq-sort-by (lambda (v) (cadr v)) #'<
               (seq-map (lambda (v) (list (xwwp-follow-link-format-link (aref (cdr v) 0))
                                          (string-to-number (car v))
                                          (aref (cdr v) 1)))
                        links)))

(defun xwwp-follow-link-callback (links)
  "Ask for a link belonging to the alist LINKS.
LINKS maps a numerical ID to an array of form [link-text, link-uri]"
  (let* ((xwidget (xwidget-webkit-current-session))
         (links (xwwp-follow-link-prepare-links links)))
    (message "%S" links)
    (condition-case nil
        (xwwp-follow-link-action
         xwidget
         (cadr
          (assoc
           (completing-read "Link: "
                            (lambda (string pred action)
                              (pcase action
                                ('metadata '(metadata))
                                ('t
                                 (let ((candidates (complete-with-action t links string pred)))
                                   (xwwp-follow-link-highlight
                                    xwidget
                                    (mapcar (lambda (cand) (cadr (assoc cand links))) candidates)
                                    nil)
                                   candidates))
                                (_ (complete-with-action action links string pred)))))
           links)))
      (t (xwwp-follow-link-cleanup xwidget)))))

;;;###autoload
(defun xwwp-follow-link (&optional xwidget)
  "Ask for a link in the XWIDGET session or the current one and follow it."
  (interactive)
  (let ((xwidget (or xwidget (xwidget-webkit-current-session))))
    (xwwp-html-inject-style xwidget "__xwidget_plus_follow_link_style" (xwwp-follow-link-style-definition))
    (xwwp-js-inject xwidget 'follow-link)
    (xwwp-follow-link-fetch-links xwidget #'xwwp-follow-link-callback)))

(provide 'xwwp-follow-link)
;;; xwwp-follow-link.el ends here
