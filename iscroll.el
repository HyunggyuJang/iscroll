;;; iscroll.el --- Smooth scrolling over images      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>
;; Maintainer: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/casouri/iscroll
;; Version: 1.0.0
;; Keywords: convenience, image
;; Package-Requires: ((emacs "26.0"))

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Gone are the days when images jumps in and out of the window when
;; scrolling! This package makes scrolling over images as if the image
;; is made of many lines, instead of a single line. (Indeed, sliced
;; image with default scrolling has the similar behavior as what this
;; package provides.)
;;
;; To use this package:
;;
;;     M-x iscroll-mode RET
;;
;; This mode remaps mouse scrolling functions and `next/previous-line'.
;; If you use other commands, you need to adapt them accordingly. See
;; `iscroll-mode-map' and `iscroll-mode' for some inspiration.
;;
;; You probably don't want to enable this in programming modes because
;; it is slower than normal scrolling commands.
;;
;; If a line is taller than double the default line height, smooth
;; scrolling is triggered and Emacs will reveal one line’s height each
;; time.
;;
;; Commands provided:
;;
;; - iscroll-up
;; - iscroll-down
;; - iscroll-next-line
;; - iscroll-previous-line
;;

;;; Code:
;;

(require 'cl-lib)

(defun iscroll-up (&optional arg)
  "Scroll up ARG lines.
Normally just calls `scroll-up'. But if the top of the window is
an image, scroll inside the image. Return the number of logical
lines scrolled."
  (interactive "p")
  (if (pos-visible-in-window-p (window-start))
      (scroll-up arg)
    (let ((arg (or arg 1))
          (original-point (point))
          (scroll-amount nil)
          (need-to-recalculate-img-height t)
          img-height
          hit-end-of-buffer)
      ;; 1) We first do a dry-run: not actually scrolling, just moving
      ;; point and modifying SCROLL-AMOUNT.
      (goto-char (window-start))
      (while (> arg 0)
        ;; Initialize SCROLL-AMOUNT when we arrived at a new line or
        ;; first entered the command.
        (when (null scroll-amount)
          (setq scroll-amount (window-vscroll nil t)))
        ;; `line-pixel-height' is expensive so we try to call it as less
        ;; as possible.
        (when need-to-recalculate-img-height
          (setq img-height (line-pixel-height)
                need-to-recalculate-img-height nil))
        ;; Scroll.
        (if (< scroll-amount img-height)
            ;; If we are in the middle of scrolling an image, scroll
            ;; that image.
            (setq scroll-amount
                  (min (+ scroll-amount (default-line-height))
                       img-height))
          ;; If we are not on an image or the image is scrolled over,
          ;; scroll display line.
          (setq need-to-recalculate-img-height t)
          ;; We hit the end of buffer, stop.
          (when (not (eq (vertical-motion 1) 1))
            (setq hit-end-of-buffer t)
            (setq arg 0))
          (setq scroll-amount nil))
        (cl-decf arg))
      ;; 2) Finally, we’ve finished the dry-run, apply the result.
      ;;
      ;; The third argument `t' tells redisplay that (point) doesn't
      ;; have to be the window start and completely visible. That
      ;; allows our vscroll value to survive.
      (set-window-start nil (point) t)
      (if scroll-amount
          (set-window-vscroll nil scroll-amount t)
        (set-window-vscroll nil 0 t)
        ;; 3) Misc stuff.
        ;;
        ;; If the original point is after window-start, it is in the
        ;; visible portion of the window, and is safe to go back to.
        (let ((start (window-start)))
          (when (> original-point start)
            (goto-char original-point))))
      ;; Show “error message”.
      (when hit-end-of-buffer
        (message "%s" (error-message-string '(end-of-buffer)))))))

(defun iscroll-down (&optional arg)
  "Scroll down ARG lines.
Normally just calls `scroll-down'. But if the top of the window is
an image, scroll inside the image. Return the number of logical
lines scrolled. If PRESERVE-SCREEN-POS non-nil, try to preserve
screen position."
  (interactive "p")
  (let ((arg (or arg 1))
        (original-point (point))
        ;; Nil means this needs to re-measured.
        (scroll-amount nil)
        hit-beginning-of-buffer)
    ;; 1) Dry-run.
    (goto-char (window-start))
    (while (> arg 0)
      (when (null scroll-amount)
        (setq scroll-amount (window-vscroll nil t)))
      (let ((img-height (line-pixel-height)))
        (if (and (>= img-height (* 2 (default-line-height)))
                 (> scroll-amount 0))
            ;; Scroll image.
            (setq scroll-amount
                  (- scroll-amount (default-line-height)))
          ;; Scroll display line.
          (when (not (eq (vertical-motion -1) -1))
            ;; If we hit the beginning of buffer, stop.
            (setq hit-beginning-of-buffer t
                  arg 0))
          ;; If the line we stopped at is an image, we don't want to
          ;; show it completely, instead, modify vscroll and only
          ;; show a bottom strip of it. If we are at the beginning
          ;; of the buffer and `vertical-motion' returns 0, we don't
          ;; want to do this.
          (let ((img-height (line-pixel-height)))
            (if (>= img-height (* 10 (default-line-height)))
                (setq scroll-amount (- img-height (default-line-height)))
              (setq scroll-amount nil)))))
      (cl-decf arg))
    ;; 2) Apply result.
    (set-window-start nil (point) t)
    (if scroll-amount
        (set-window-vscroll nil scroll-amount t)
      (set-window-vscroll nil 0 t))
    ;; 3) Misc
    ;;
    ;; HACK: There is no fast and reliable way to get the last visible
    ;; point, hence this hack: move point up until it is visible.
    (goto-char original-point)
    ;; Checking point > window-start is important, otherwise we could
    ;; fall into infinite loop. E.g., when point = window-start and
    ;; under the point is an image that is not completely visible.
    (while (and (> (point) (window-start))
                (not (pos-visible-in-window-p (point))))
      (vertical-motion -2))
    (when hit-beginning-of-buffer
      (message "%s" (error-message-string '(beginning-of-buffer))))))

;;;###autoload
(define-minor-mode iscroll-mode
  "Smooth scrolling over images."
  :lighter " IS"
  :group 'scrolling
  (if iscroll-mode
      (setq-local mwheel-scroll-up-function #'iscroll-up
                  mwheel-scroll-down-function #'iscroll-down)
    (kill-local-variable 'mwheel-scroll-up-function)
    (kill-local-variable 'mwheel-scroll-down-function)))

(provide 'iscroll)

;;; iscroll.el ends here
