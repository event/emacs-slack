;;; slack-mpim.el ---slack multi-party direct message interface    -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Leo Movshovich

;; Author: Leo Movshovich <event.riga@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-room)
(require 'slack-buffer)
(require 'slack-user)

(defvar slack-buffer-function)

(defconst slack-mpim-history-url "https://slack.com/api/mpim.history")
(defconst slack-mpim-buffer-name "*Slack - MP Direct Messages*")
(defconst slack-user-list-url "https://slack.com/api/users.list")
(defconst slack-mpim-list-url "https://slack.com/api/mpim.list")
(defconst slack-mpim-close-url "https://slack.com/api/mpim.close")
(defconst slack-mpim-open-url "https://slack.com/api/mpim.open")
(defconst slack-mpim-update-mark-url "https://slack.com/api/mpim.mark")

(defclass slack-mpim (slack-room)
  ((members :initarg :members :type list)))

(defmethod slack-room-open-p ((room slack-mpim))
  t)

(defmethod slack-room-name ((room slack-mpim))
  (with-slots (members team-id) room
    (let ((team (slack-team-find team-id)))
      (mapconcat #'(lambda (user) (slack-user-name user team)) members "-"))))

(defmethod slack-room-buffer-name ((room slack-mpim))
  (concat slack-mpim-buffer-name
          " : "
          (slack-room-name-with-team-name room)))

(defun slack-mpim-select ()
  (interactive)
  (slack-room-select (oref (slack-team-select) mpims)))

(defun slack-mpim-list-update ()
  (interactive)
  (let ((team (slack-team-select)))
    (cl-labels ((on-list-update
                 (&key data &allow-other-keys)
                 (slack-request-handle-error
                  (data "slack-mpim-list-update")
                  (with-slots (mpims) team
                    (setq mpims
                          (mapcar #'(lambda (g)
                                      (slack-room-create g team 'slack-mpim))
                                  (plist-get data :groups))))
                  (message "Slack MPIM List Updated"))))
      (slack-room-list-update slack-mpim-list-url
                              #'on-list-update
                              team
                              :sync nil))))

(defmethod slack-room-update-mark-url ((_room slack-mpim))
  slack-mpim-update-mark-url)

(defmethod slack-room-history-url ((_room slack-mpim))
  slack-mpim-history-url)

(defun slack-mpim-members-s (mpim)
  (with-slots (members team-id) mpim
    (let ((team (slack-team-find team-id)))
      (mapconcat #'(lambda (user) (slack-user-name user team)) members ", "))))

(defun slack-mpim-names (team &optional filter)
  (with-slots (mpims) team
    (slack-room-names mpims filter)))

(defun slack-mpim-close ()
  (interactive)
  (let* ((team (slack-team-select)))
    (slack-select-from-list
     ((slack-mpim-names team) "Select MPIM: ")
     (cl-labels
         ((on-success
           (&key data &allow-other-keys)
           (slack-request-handle-error
            (data "slack-mpim-close")
            (if (plist-get data :already_closed)
               (message "Direct Message Channel with %s Already Closed" 
                        (slack-mpim-members-s (slack-room-find (oref selected id) team)))))))
       (slack-request
        slack-mpim-close-url
        team
        :type "POST"
        :params (list (cons "channel" (oref selected id)))
        :success #'on-success
        :sync nil)))))

       
(defun slack-mpim-open ()
  (interactive)
  (let* ((team (slack-team-select))
         (users (slack-user-names team)))
    (cl-labels
        ((select-users (users acc)
                       (let ((selected (completing-read "Select User: "
                                                        users nil t)))
                         (if (< 0 (length selected))
                             (select-users users
                                           (push (cdr (cl-assoc selected users :test #'string=)) acc))
                           acc)))
         (on-success
          (&key data &allow-other-keys)
          (slack-request-handle-error
           (data "slack-mpim-open")
           (if (plist-get data :already_open)
               (let ((mpim (slack-room-find (oref selected id) team)))
                 (message "Direct Message Channel with %s Already Open"
                          (slack-mpim-members-s mpim)))))))
      (slack-request
       slack-mpim-open-url
       team
       :type "POST"
       :params (list (cons "users" (mapconcat (lambda (u) (plist-get u :id)) (select-users users '()) ",")))
       :success #'on-success
       :sync nil))))

(provide 'slack-mpim)
;;; slack-mpim.el ends here
