;;; sideline-emoji.el --- Show emoji information with sideline  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-sideline/sideline-emoji
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (sideline "0.1.0") (emojify "1.2.1"))
;; Keywords: convenience sideline emoji

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Show emoji information with sideline.
;;
;; 1) Add sideline-emoji to sideline backends list,
;;
;;   (setq sideline-backends-right '(sideline-emoji))
;;
;; 2) Then enable sideline-mode in the target buffer,
;;
;;   M-x sideline-mode
;;

;;; Code:

(require 'emojify)
(require 'sideline)

(defgroup sideline-emoji nil
  "Show emoji information with sideline."
  :prefix "sideline-emoji-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-sideline/sideline-emoji"))

;;;###autoload
(defun sideline-emoji (command)
  "Backend for sideline.

Argument COMMAND is required in sideline backend."
  (cl-case command
    (`candidates (cons :async #'sideline-emoji--show))))

(defun sideline-emoji--show (callback &rest _)
  "Execute CALLBACK to display with sideline."
  (when-let* ((text (sideline-color--thing-at-point 'symbol t))
              (color (color-values text))
              (display (concat (propertize sideline-color-text 'font-lock-face `(:foreground ,text)) " " text)))
    (funcall callback (list display))))

(provide 'sideline-emoji)
;;; sideline-emoji.el ends here
