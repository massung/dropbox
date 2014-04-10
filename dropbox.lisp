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
   #:dropbox-chunked-upload
   #:dropbox-commit-chunked-upload
   #:dropbox-copy
   #:dropbox-copy-ref
   #:dropbox-create-folder
   #:dropbox-delete
   #:dropbox-files-get
   #:dropbox-files-put
   #:dropbox-media
   #:dropbox-metadata
   #:dropbox-move
   #:dropbox-restore
   #:dropbox-revisions
   #:dropbox-search
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

   ;; copy ref readers
   #:copy-ref
   #:copy-ref-expires

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
             (write-string (dropbox-error c) stream))))

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
  (:documentation "Returned by dropbox-request-access."))

(defclass chunked-upload ()
  ((|upload_id| :reader chunked-upload-id)
   (|offset|    :reader chunked-upload-offset)
   (|expires|   :reader chunked-upload-expires))
  (:documentation "Returned by dropbox-chunked-upload."))

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

(defclass copy-ref ()
  ((|copy_ref| :reader copy-ref)
   (|expires|  :reader copy-ref-expires))
  (:documentation "Copy reference."))

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
  (let ((rel-path (string-trim "/" path)))
    (format nil "/1/~a~:[~;/~(~a~)~:[~;/~a~]~]" api path *app-root* path rel-path)))

(defun query-params (params)
  "Create a list of query parameters from keywords and value."
  (loop :for key := (pop params)
        :for value := (pop params)
        :until (null key)
        :collect (list (string-downcase key) value)))

(defun parse-json-response (type resp)
  "Decode a JSON response into a type."
  (let ((body (response-body resp))
        (req (response-request resp)))
    (if (= (response-code resp) 200)
        (json-decode-into type body (request-url req))
      (error (json-decode-into 'dropbox-error body (request-url req))))))

(defun dropbox-request (type method url token &key data)
  "Perform an HTTP request, and if successful, parse the JSON body."
  (let ((req (make-instance 'request
                            :data data
                            :url url
                            :method method
                            :headers (list (auth-header token)))))
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

(defun dropbox-chunked-upload (token &optional upload)
  "Uploads large files to Dropbox in multiple chunks."
  (let ((query (when upload
                 `(("upload_id" ,(chunked-upload-id upload))
                   ("offset" ,(chunked-upload-offset upload))))))
    (with-url (url +content-url+ :path (end-point "chunked_upload") :query query)
      (dropbox-request 'chunked-upload "PUT" url token))))

(defun dropbox-commit-chunked-upload (token upload &rest params &key parent_rev overwrite locale)
  "Completes an upload initiated by dropbox-chunked-upload."
  (declare (ignore parent_rev overwrite locale))
  (let ((query `("upload_id" ,(chunked-upload-id upload) ,@params)))
    (with-url (url +content-url+ :path (end-point "commit_chunked_upload") :query (query-params query))
      (dropbox-request 'metadata "POST" url token))))

(defun dropbox-copy (token from to-path &rest params &key locale)
  "Copies a file or folder to a new location."
  (declare (ignore locale))
  (let* ((from (if (stringp from)
                   (list "from_path" from)
                 (list "from_copy_ref" (copy-ref from))))
         (query `("root" ,(string-downcase *app-root*) "to_path" ,to-path ,@from ,@params)))
    (with-url (url +api-url+ :path (end-point "fileops/copy") :query (query-params query))
      (dropbox-request 'metadata "POST" url token))))

(defun dropbox-copy-ref (token path)
  "Returns a direct link to a file."
  (with-url (url +api-url+ :path (end-point "copy_ref" path))
    (dropbox-request 'copy-ref "GET" url token)))

(defun dropbox-create-folder (token path)
  "Creates a folder."
  (let ((query `(("root" ,(string-downcase *app-root*)) ("path" ,path))))
    (with-url (url +api-url+ :path (end-point "fileops/create_folder") :query query)
      (dropbox-request 'metadata "POST" url token))))

(defun dropbox-delete (token path)
  "Deletes a file or folder."
  (let ((query `(("root" ,(string-downcase *app-root*)) ("path" ,path))))
    (with-url (url +api-url+ :path (end-point "/fileops/delete") :query query)
      (dropbox-request 'metadata "POST" url token))))

(defun dropbox-files-get (token path &rest params &key rev)
  "Perform a GET on a file."
  (declare (ignore rev))
  (with-url (url +content-url+ :path (end-point "files" path) :query (query-params params))
    (let ((resp (http-get url :headers (list (auth-header token)))))
      (when (= (response-code resp) 200)
        (with-headers ((metadata "x-dropbox-metadata"))
            resp
          (values (response-body resp)
                  (when metadata
                    (json-decode-into 'metadata metadata))))))))

(defun dropbox-files-put (token path data &rest params &key overwrite parent_rev locale)
  "Uploads a file."
  (declare (ignore overwrite parent_rev locale))
  (with-url (url +content-url+ :path (end-point "files_put" path) :query (query-params params))
    (dropbox-request 'metadata "PUT" url token :data data)))

(defun dropbox-media (token path &rest params &key locale)
  "Returns a direct link to a file."
  (declare (ignore locale))
  (with-url (url +api-url+ :path (end-point "media" path) :query (query-params params))
    (dropbox-request 'media "POST" url token)))

(defun dropbox-metadata (token path &rest params &key file_limit hash list include_deleted rev locale)
  "Fetch the metadata for a give file or folder."
  (declare (ignore file_limit hash list include_deleted rev locale))
  (with-url (url +api-url+ :path (end-point "metadata" path) :query (query-params params))
    (dropbox-request 'metadata "GET" url token)))

(defun dropbox-move (token from-path to-path)
  "Deletes a file or folder."
  (let ((query `(("root" ,(string-downcase *app-root*)) ("from_path" ,from-path) ("to_path" ,to-path))))
    (with-url (url +api-url+ :path (end-point "/fileops/move") :query query)
      (dropbox-request 'metadata "POST" url token))))

(defun dropbox-restore (token path rev &rest params &key locale)
  "Restore a file path to a previous revision."
  (declare (ignore locale))
    (with-url (url +api-url+ :path (end-point "restore" path) :query (query-params `("rev" ,rev ,@params)))
      (dropbox-request 'metadata "POST" url token)))

(defun dropbox-revisions (token path &rest params &key rev_limit locale)
  "Obtains metadata for the previous revisions of a file."
  (declare (ignore rev_limit locale))
  (with-url (url +api-url+ :path (end-point "revisions" path) :query (query-params params))
    (dropbox-request 'metadata "GET" url token)))

(defun dropbox-search (token path query &rest params &key file_limit include_deleted locale)
  "Returns metadata for all files and folders whose filename contains the given search string."
  (declare (ignore file_limit include_deleted locale))
  (with-url (url +api-url+ :path (end-point "search" path) :query (query-params `("query" ,query ,@params)))
    (dropbox-request 'metadata "GET" url token)))

(defun dropbox-shares (token path &rest params &key locale short_url)
  "Creates and returns a Dropbox link to files or folders."
  (declare (ignore locale short_url))
  (with-url (url +api-url+ :path (end-point "shares" path) :query (query-params params))
    (dropbox-request 'media "POST" url token)))

(defun dropbox-thumbnails (token path &rest params &key format size)
  "Gets a thumbnail image."
  (declare (ignorable format size))
  (with-url (url +content-url+ :path (end-point "thumbnails" path) :query (query-params params))
    (let ((resp (http-get url :headers (list (auth-header token)))))
      (if (= (response-code resp) 200)
          (with-headers ((metadata "x-dropbox-metadata"))
              resp
            (let ((bytes (map '(vector (unsigned-byte 8)) #'char-code (response-body resp))))
              (values bytes (json-decode-into 'metadata metadata))))
        (error (json-decode-into 'dropbox-error (response-body resp)))))))