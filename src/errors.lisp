(defpackage #:visualcrossing/errors
  (:use #:cl)
  (:export #:visual-crossing-error
           #:error-message
           #:error-code
           #:api-error
           #:error-status-code
           #:error-response-body
           #:authentication-error
           #:rate-limit-error
           #:retry-after
           #:invalid-location-error
           #:error-location
           #:network-error
           #:original-error
           #:validation-error
           #:error-parameter
           #:error-value))
(in-package #:visualcrossing/errors)

(define-condition visual-crossing-error (error)
  ((message :initarg :message :reader error-message)
   (code :initarg :code :reader error-code :initform nil))
  (:report (lambda (condition stream)
             (format stream "Visual Crossing API Error: ~A"
                     (error-message condition)))))

(define-condition api-error (visual-crossing-error)
  ((status-code :initarg :status-code :reader error-status-code)
   (response-body :initarg :response-body :reader error-response-body)))

(define-condition authentication-error (api-error)
  ())

(define-condition rate-limit-error (api-error)
  ((retry-after :initarg :retry-after :reader retry-after :initform nil)))

(define-condition invalid-location-error (api-error)
  ((location :initarg :location :reader error-location)))

(define-condition network-error (visual-crossing-error)
  ((original-error :initarg :original-error :reader original-error)))

(define-condition validation-error (visual-crossing-error)
  ((parameter :initarg :parameter :reader error-parameter)
   (value :initarg :value :reader error-value)))
