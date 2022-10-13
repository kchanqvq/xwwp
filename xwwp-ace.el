;;; xwwp-ace.el --- Ace-jump style navigation in `xwidget-webkit' sessions -*- lexical-binding: t; -*-

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

;; Add support for navigating web pages in `xwidget-webkit' sessions in ace-jump style.

;;; Code:

(require 'xwwp)

(defgroup xwwp-ace nil
  "`xwidget-webkit' xwwp-ace customizations."
  :group 'xwwp)

(defcustom xwwp-ace-label-style '(("z-index" . "2147483647")
                                  ("color" . "red")
                                  ("opacity" . "0.5")
                                  ("background-color" . "yellow")
                                  ("font-family" . "monospace")
                                  ("font-size" . "2em"))
  "CSS style to apply to xwwp-ace labels."
  :type '(list (cons string string))
  :group 'xwwp-ace)

(defcustom xwwp-ace-candidate-selector
  "button, input, [href], select, textarea, [tabindex]:not([tabindex=\"-1\"])"
  "CSS selector to select destination of xwwp-ace."
  :type 'string
  :group 'xwwp-ace)
(defcustom xwwp-ace-too-crowded-threshold 5
  "Don't create labels that are within this proximity."
  :type 'integer
  :group 'xwwp-ace)
(defun xwwp-ace-style-definition ()
  "Return the css definitions for the follow link feature."
  (xwwp-css-make-class "xwwp-ace-label" xwwp-ace-label-style))
(xwwp-js-def ace highlight ()
  "Label visible focusable elements."
  (format "
window.__xwidget_plus_ace_candidates = [];
document.querySelectorAll('%s').forEach(
  function (elem) {
    let bounding = elem.getBoundingClientRect();
    if(bounding.top >= 0 &&
      bounding.left >= 0 &&
      bounding.bottom <= (window.innerHeight || document.documentElement.clientHeight) &&
      bounding.right <= (window.innerWidth || document.documentElement.clientWidth) &&
      (elem.offsetWidth > 0 || elem.offsetHeight > 0 || elem.getClientRects().length > 0) &&
      window.__xwidget_plus_ace_candidates.every(
          function (other_candidate){
            let other_overlay = other_candidate[0];
            other_bounding = other_overlay.getBoundingClientRect();
            return !(Math.abs(other_bounding.top - bounding.top) < %s
              && Math.abs(other_bounding.left - bounding.left) < %s)
          })){
        let overlay = document.createElement('div');
        overlay.className = 'xwwp-ace-label';
        overlay.style.position = 'fixed';
        overlay.style.left = bounding.left + 'px';
        overlay.style.top = bounding.top + 'px';
        document.body.appendChild(overlay);
        window.__xwidget_plus_ace_candidates.push([overlay, elem]);
     };
  });
let label_length = Math.ceil(Math.log(window.__xwidget_plus_ace_candidates.length)/Math.log(26));
window.__xwidget_plus_ace_candidates.forEach(
  function (candidate, id) {
    let elem = candidate[0];
    elem.appendChild(document.createTextNode(
      id.toString(26).split('').map(
      function (char) {
        let code = char.charCodeAt(0);
        if (code < 97) {
          return String.fromCharCode(code + 49);
        }
        else {
          return String.fromCharCode(code + 10);
        }
      }).join('').padStart(label_length,'a')));
  });
return window.__xwidget_plus_ace_candidates.length;
"
          xwwp-ace-candidate-selector
          xwwp-ace-too-crowded-threshold
          xwwp-ace-too-crowded-threshold))
(xwwp-js-def ace cleanup ()
  "Remove xwwp-ace labels.""
window.__xwidget_plus_ace_candidates.forEach(candidate => candidate[0].remove());
window.__xwidget_plus_ace_candidates=[];
")
(xwwp-js-def ace read-key (keycode)
  "Accept one keystroke.
Return 1 if located an element, 0 if need more input or -1 if failed.""
let key = String.fromCharCode(keycode);
let updated_candidates = window.__xwidget_plus_ace_candidates.filter(
  candidate => candidate[0].innerText.startsWith(key));
if(updated_candidates.length > 1){
    window.__xwidget_plus_ace_candidates.forEach(
    function (candidate) {
      let elem = candidate[0];
      if (!elem.innerText.startsWith(key))
      elem.remove();
    });
  updated_candidates.forEach(function (candidate)
    {let elem = candidate[0];
     elem.innerText = elem.innerText.substring(1)});
  window.__xwidget_plus_ace_candidates = updated_candidates;
  return 0;
}
else if (updated_candidates.length == 1){
  let selected = updated_candidates[0][1];
  __xwidget_plus_ace_cleanup();
  selected.focus();
  selected.click();
  return 1;
}
else {
  return -1;
}")
(defun xwwp-webkit-insert-string ()
  "Insert string into the active field in the current webkit widget.

This improves `xwidget-webkit-insert-string': it doesn't
generates JS error when there's no active input element, and it
generates a submit event when the `last-command-event' is
`RET'. If you don't wish to submit, use `C-j'."
  ;; Read out the string in the field first and provide for edit.
  (interactive nil xwidget-webkit-mode)
  ;; As the prompt differs on JavaScript execution results,
  ;; the function must handle the prompt itself.
  (let ((xww (xwidget-webkit-current-session)))
    (xwidget-webkit-execute-script
     xww
     (concat xwidget-webkit-activeelement-js "
(function () {
  var res = findactiveelement(document);
  if (res)
    return [res.value, res.type];
})();")
     (lambda (field)
       "Prompt a string for the FIELD and insert in the active input."
       (let ((str (pcase field
                    (`[,val "text"]
                     (read-string "Text: " val))
                    (`[,val "password"]
                     (read-passwd "Password: " nil val))
                    (`[,val "textarea"]
                     (xwidget-webkit-begin-edit-textarea xww val)))))
         (when str
           (xwidget-webkit-execute-script
            xww
            (concat "findactiveelement(document).value='" str "';"
                    (if (eq last-command-event 13)
                        "findactiveelement(document).form.submit();"
                      "")))))))))

(defun xwwp-ace-read-key-command (action)
  "Read a key to narrow down selection.
ACTION is passed from JavaScript side, to indicate
the effect of last key stroke.
0 means it successfully narrows down some selection,
-1 means it does not match any candidates,
and 1 means one unique candidate has been selected and clicked."
  (let ((xwidget (xwidget-webkit-current-session)))
    (pcase action
      (0 (let ((next-key (read-key "Enter prefix of the candidate labels to narrow down selection")))
           (if (= next-key 7)
               (xwwp-ace-cleanup xwidget)
             (xwwp-ace-read-key xwidget next-key #'xwwp-ace-read-key-command))))
      (-1 (message "No candidate matching the prefix. Type C-g to quit.")
          (xwwp-ace-read-key-command 0))
      (1 (xwidget-webkit-execute-script
          xwidget "findactiveelement(document) && findactiveelement(document).type"
          (lambda (type)
            (pcase type
              ((or "text" "password" "textarea")
               (xwwp-webkit-insert-string)))))))))
(defun xwwp-ace-toggle-callback (length)
  "Callback for JavaScript function xwwp-ace-highlight.
LENGTH is the number of highlighted candidates."
  (print length)
  (if (> length 0)
      (xwwp-ace-read-key-command 0)
    (message "No candidate for xwwp-ace.")))

(defun xwwp-ace-toggle ()
  "Start an xwwp-ace jump."
  (interactive)
  (let ((xwidget (xwidget-webkit-current-session)))
    (xwwp-ace-cleanup xwidget)
    (xwwp-html-inject-style xwidget "__xwidget_plus_ace_style" (xwwp-ace-style-definition))
    (xwwp-js-inject xwidget 'ace)
    (xwwp-ace-highlight xwidget #'xwwp-ace-toggle-callback)))

(provide 'xwwp-ace)
;;; xwwp-ace.el ends here
