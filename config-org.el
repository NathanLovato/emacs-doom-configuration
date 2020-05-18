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

(defvar-local time-interval 15)

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

(provide 'config-org)
;;; config-org.el ends here
