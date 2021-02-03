;;;;;; config-org.el --- Org, agenda, appointments... -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Nathan
;;
;; Author: Nathan Lovato <nathan@gdquest.com>
;; Maintainer: Nathan Lovato <nathan@gdquest.com>
;; Created: March 08, 2020
;; Modified: March 08, 2020
;; Version: 0.0.1
;; Keywords:
;; Package-Requires: ((emacs 27.0.60) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Configuration for org-mode, agenda and desktop notifications.
;;
;;; Code:

(require 'notifications)
(require 'appt)

(setq org-support-shift-select t)
(setq org-directory "~/Documents/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))
;; (setq org-agenda-files '("/home/gdquest/Documents/org/calendar.org" "/home/gdquest/Documents/org/tasks.org"))

(setq org-projectile-capture-template "* %?\n  %U\n  %i\n  %a")

(setq! org-refile-targets '((nil :maxlevel . 1) (org-agenda-files :maxlevel . 1)))
(defvar-local time-interval 10)

(defun appt-agenda-notify (minutes-to-appt time-current message)
  "Display a notification before scheduled events registered in org-agenda."
  (notifications-notify :title "Appointment"
                        :body (format "[%s] In %s minutes: %s" time-current minutes-to-appt message)
                        :app-name "Emacs: Appointments"
                        :urgency "normal"
                        :sound-name "alarm-clock-elapsed"))

(setq appt-message-warning-time time-interval
      appt-display-interval time-interval
      appt-display-mode-line t
      appt-display-format 'window
      appt-disp-window-function
      (function appt-agenda-notify))

;; Update appointments automatically
(org-agenda-to-appt)
(run-at-time "12:00am"
             (* 24 3600)
             'org-agenda-to-appt)
(add-hook 'after-save-hook
          '(lambda ()
             (if (seq-contains (org-agenda-files)
                               (buffer-file-name))
                 (org-agenda-to-appt))))
(appt-activate 1)
(display-time)

(defun org-sum-points-in-subtree ()
  "Add up all the TALLY properties of headings underneath the current one
The total is written to the TALLY_SUM property of this heading"
  (interactive)
  (org-entry-put (point) "POINTS_TOTAL"
                 (number-to-string
                  (let ((total 0))
                    (save-excursion
                      (org-map-tree
                       (lambda ()
                         (let ((n (org-entry-get (point) "POINTS")))
                           (when (stringp n)
                             (setq total (+ total (string-to-number n))))))))
                    total))))

(after! org
  (setq! org-agenda-span 'day
         org-agenda-include-diary t
         diary-show-holidays-flag nil))
(defun diary-last-day-of-month (date)
  "Return `t` if DATE is the last day of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day-of-month
          (calendar-last-day-of-month month year)))
    (= day last-day-of-month)))

(provide 'config-org)
;;; config-org.el ends here
