;;; xwwp-full.el --- Enhance xwidget webkit browser -*- lexical-binding: t; -*-

;; Author: Q. Hong
;; URL: https://github.com/canatella/xwwp
;; Created: 2020-08-24
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;; Copyright (C) 2020  Q. Hong <qhong@mit.edu>

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

;; This package loads all available xwwp packages

;;; Code:

(require 'xwwp)
(require 'xwwp-follow-link)
(require 'xwwp-history)
(require 'xwwp-ace)
(require 'xwwp-section)
(require 'xwwp-reader)

(provide 'xwwp-full)
;;; xwwp-full.el ends here
