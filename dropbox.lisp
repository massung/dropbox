;;;; Dropbox API for LispWorks
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
   #:*app-key*
   #:*app-secret*
   #:*app-root*

   ;; Conditions
   #:dropbox-error

   ;; OAuth functions
   #:dropbox-request-token
   #:dropbox-request-access

   ;; REST API end-points
   #:dropbox-account-info
   #:dropbox-files-get
   #:dropbox-files-put
   #:dropbox-media
   #:dropbox-metadata
   #:dropbox-revisions
   #:dropbox-shares
   #:dropbox-thumbnails
   
   ;; access token readers
   #:access-token
   #:access-token-uid
   #:access-token-type

   ;; account info readers
   #:account-info-referral-link
   #:account-info-display-name
   #:account-info-uid
   #:account-info-country
   #:account-info-quota
   #:account-info-email

   ;; media readers
   #:media-url
   #:media-expires

   ;; metadata readers
   #:metadata-size
   #:metadata-bytes
   #:metadata-path
   #:metadata-dir-p
   #:metadata-deleted-p
   #:metadata-rev
   #:metadata-hash
   #:metadata-thumb-p
   #:metadata-icon
   #:metadata-modified
   #:metadata-client-mtime
   #:metadata-root
   #:metadata-mime-type
   #:metadata-contents

   ;; quota info readers
   #:quota-datastores
   #:quota-shared
   #:quota
   #:quota-normal))

(in-package :dropbox)

(define-condition dropbox-error (error)
  ((|error| :reader dropbox-error))
  (:report (lambda (c stream)
             (format stream "Dropbox error: ~s" (dropbox-error c)))))

(defvar *app-key* nil
  "The App Key for your application. Provided by Dropbox.")
(defvar *app-secret* nil
  "The App Secret for your application. Provided by Dropbox.")
(defvar *app-root* :sandbox
  "Either :dropbox or :sandbox, based on the application's scope.")

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

(defclass media ()
  ((|url|     :reader media-url)
   (|expires| :reader media-expires))
  (:documentation "Media information."))

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
   (|mime_type|    :reader metadata-mime-type)
   (|contents|     :reader metadata-contents :type metadata))
  (:documentation "Metadata for any content file or directory."))

(defclass revision (metadata)
  ()
  (:documentation "Revision metadata."))

(defclass shares (media)
  ()
  (:documentation "Shares result."))

(defconstant +auth-url+ "https://www.dropbox.com/1/oauth2/authorize"
  "End-point for requesting an authorization token.")
(defconstant +token-url+ "https://api.dropbox.com/1/oauth2/token"
  "End-point for requesting an access token.")
(defconstant +api-url+ "https://api.dropbox.com"
  "End-point domain for all API calls.")
(defconstant +content-url+ "https://api-content.dropbox.com"
  "End-point domain for all API content calls.")

(defun auth-header (token)
  "Create an authorization header for all REST API end-point requests."
  (list "Authorization" (format nil "~@(~a~) ~a" (access-token-type token) (access-token token))))

(defun end-point (api &optional path)
  "Create the path end-point for any given API call."
  (format nil "/1/~a~:[~;/~(~a~)~a~]" api path *app-root* path))

(defun parse-json-response (type resp)
  "Decode a JSON response into a type."
  (let ((body (response-body resp))
        (req (response-request resp)))
    (if (= (response-code resp) 200)
        (json-decode-into type body (request-url req))
      (error (json-decode-into 'dropbox-error body (request-url req))))))

(defun dropbox-request (type method url token &key data)
  "Perform an HTTP request, and if successful, parse the JSON body."
  (let ((req (make-instance 'request :data data :url url :method method :headers (list (auth-header token)))))
    (parse-json-response type (http-perform req))))

(defun dropbox-request-token ()
  "Open the Dropbox authorization end-point to get a request token."
  (with-url (url +auth-url+ :query `(("response_type" "code") ("client_id" ,*app-key*)))
    (sys:open-url (format-url url))))

(defun dropbox-request-access (token)
  "Perform the POST request to obtain the access token."
  (let ((qs `(("grant_type" "authorization_code") ("code" ,token))))
    (with-url (url +token-url+ :auth (list *app-key* *app-secret*) :query qs)
      (parse-json-response 'access-token (http-post url)))))

(defun dropbox-account-info (token)
  "Perform a GET to /account/info."
  (with-url (url +api-url+ :path (end-point "account/info"))
    (dropbox-request 'account-info "GET" url token)))

(defun dropbox-files-get (token path)
  "Perform a GET on a file."
  (with-url (url +content-url+ :path (end-point "files" path))
    (let ((resp (http-get url :headers (list (auth-header token)))))
      (when (= (response-code resp) 200)
        (response-body resp)))))

(defun dropbox-files-put (token path data)
  "Uploads a file."
  (with-url (url +content-url+ :path (end-point "files" path))
    (dropbox-request 'metadata "PUT" url token :data data)))

(defun dropbox-media (token path)
  "Returns a direct link to a file."
  (with-url (url +api-url+ :path (end-point "media" path))
    (dropbox-request 'media "POST" url token)))

(defun dropbox-metadata (token path)
  "Fetch the metadata for a give file or folder."
  (with-url (url +api-url+ :path (end-point "metadata" path))
    (dropbox-request 'metadata "GET" url token)))

(defun dropbox-revisions (token path)
  "Obtains metadata for the previous revisions of a file."
  (with-url (url +api-url+ :path (end-point "revisions" path))
    (dropbox-request 'revision "GET" url token)))

(defun dropbox-shares (token path)
  "Creates and returns a Dropbox link to files or folders."
  (with-url (url +api-url+ :path (end-point "shares" path))
    (dropbox-request 'shares "POST" url token)))

(defun dropbox-thumbnails (token path &key (format "jpeg") (size "s"))
  "Gets a thumbnail image."
  (with-url (url +content-url+ :path (end-point "thumbnails" path) :query `(("format" ,format) ("size" ,size)))
    (http-get url :headers (list (auth-header token)))))