;;;; Dropbox REST API for LispWorks
;;;;
;;;; Copyright (c) 2013 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :dropbox
  (:use :cl :http :json)
  (:export
   ;; OAuth functions
   #:dropbox-request-token
   #:dropbox-request-access

   ;; REST API end-points
   #:dropbox-account-info
   #:dropbox-files
   #:dropbox-metadata
   #:dropbox-revisions
   
   ;; access token readers
   #:access-token
   #:access-token-uid
   #:access-token-type
   ))

(in-package :dropbox)

(defclass access-token ()
  ((|access_token| :reader access-token)
   (|uid|          :reader access-token-uid)
   (|token_type|   :reader access-token-type))
  (:documentation "Returned by REQUEST-ACCESS function."))

(defclass quota-info ()
  ((|datastores| :reader quota-datastores)
   (|shared|     :reader quota-shared)
   (|quota|      :reader quota)
   (|normal|     :reader quota-normal))
  (:documentation "The quota from account-info."))

(defclass account-info ()
  ((|referral_link| :reader account-info-referral-link)
   (|display_name|  :reader account-info-display-name)
   (|uid|           :reader account-info-uid)
   (|country|       :reader account-info-country)
   (|quota_info|    :reader account-info-quota :type quota-info)
   (|email|         :reader account-info-email))
  (:documentation "Account information."))

(defclass metadata ()
  ((|size|         :reader metadata-size)
   (|bytes|        :reader metadata-bytes)
   (|path|         :reader metadata-path)
   (|is_dir|       :reader metadata-dir-p)
   (|is_deleted|   :reader metadata-deleted-p)
   (|rev|          :reader metadata-rev)
   (|hash|         :reader metadata-hash)
   (|thumb_exists| :reader metadata-thumb-p)
   (|icon|         :reader metadata-icon)
   (|modified|     :reader metadata-modified)
   (|client_mtime| :reader metadata-client-mtime)
   (|root|         :reader metadata-root)
   (|contents|     :reader metadata-contents :type metadata))
  (:documentation "Metadata for any content file or directory."))

(defclass revision (metadata)
  ()
  (:documentation "Revision metadata."))

(defmethod auth-header ((token access-token))
  "Create an authorization header for all REST API end-point requests."
  (list "Authorization" (format nil "~@(~a~) ~a" (access-token-type token) (access-token token))))

(defconstant +auth-url+ "https://www.dropbox.com/1/oauth2/authorize"
  "End-point for requesting an authorization token.")
(defconstant +token-url+ "https://api.dropbox.com/1/oauth2/token"
  "End-point for requesting an access token.")
(defconstant +api-url+ "https://api.dropbox.com"
  "End-point domain for all API calls.")
(defconstant +content-url+ "https://api-content.dropbox.com"
  "End-point domain for all API content calls.")

(defun parse-json-response (resp type)
  "Perform an HTTP request, and if successful, parse the JSON body."
  (when (= (response-code resp) 200)
    (let ((json (json-decode (response-body resp) (request-url (response-request resp)))))
      (when json
        (json-decode-into json type)))))

(defun dropbox-request-token (key)
  "Open the Dropbox authorization end-point to get a request token."
  (with-url (url +auth-url+ :query `(("response_type" "code") ("client_id" ,key)))
    (sys:open-url (format-url url))))

(defun dropbox-request-access (key secret token)
  "Perform the POST request to obtain the access token."
  (with-url (url +token-url+ :auth (list key secret) :query `(("grant_type" "authorization_code")
                                                              ("code" ,token)))
    (parse-json-response (http-post url) 'access-token)))

(defun dropbox-account-info (token)
  "Perform a GET to /account/info."
  (with-url (url +api-url+ :path "/1/account/info")
    (parse-json-response (http-get url :headers (list (auth-header token))) 'account-info)))

(defun dropbox-files (token &key (root "dropbox") (path "/"))
  "Perform a GET to /1/files/<root>/<path>."
  (with-url (url +content-url+ :path (format nil "/1/files/~a~a" root path))
    (http-get url :headers (list (auth-header token)))))

(defun dropbox-metadata (token &key (root "dropbox") (path "/"))
  "Fetch the metadata for a give file or folder."
  (with-url (url +api-url+ :path (format nil "/1/metadata/~a~a" root path))
    (parse-json-response (http-get url :headers (list (auth-header token))) 'metadata)))

(defun dropbox-revisions (token &key (root "dropbox") (path "/"))
  "Obtains metadata for the previous revisions of a file."
  (with-url (url +api-url+ :path (format nil "/1/revisions/~a~a" root path))
    (parse-json-response (http-get url :headers (list (auth-header token))) 'revision)))