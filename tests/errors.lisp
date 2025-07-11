(defpackage #:visualcrossing/tests/errors
  (:use #:cl
        #:rove)
  (:import-from #:visualcrossing/errors
                #:visual-crossing-error
                #:api-error
                #:authentication-error
                #:rate-limit-error
                #:invalid-location-error
                #:validation-error
                #:error-message
                #:error-status-code
                #:error-response-body
                #:retry-after
                #:error-location
                #:error-parameter
                #:error-value))
(in-package #:visualcrossing/tests/errors)

(deftest error-condition-hierarchy
  (testing "Error condition hierarchy"
    (ok (subtypep 'api-error 'visual-crossing-error))
    (ok (subtypep 'authentication-error 'api-error))
    (ok (subtypep 'rate-limit-error 'api-error))
    (ok (subtypep 'invalid-location-error 'api-error))
    (ok (subtypep 'validation-error 'visual-crossing-error))))

(deftest visual-crossing-error-basic
  (testing "Visual crossing error basic functionality"
    (let ((error (make-condition 'visual-crossing-error
                                 :message "Test error")))
      (ok (string= (error-message error) "Test error")))))

(deftest api-error-creation
  (testing "API error creation and accessors"
    (let ((error (make-condition 'api-error
                                 :message "API failed"
                                 :status-code 500
                                 :response-body "Internal server error")))
      (ok (string= (error-message error) "API failed"))
      (ok (= (error-status-code error) 500))
      (ok (string= (error-response-body error) "Internal server error")))))

(deftest authentication-error-creation
  (testing "Authentication error creation"
    (let ((error (make-condition 'authentication-error
                                 :message "Invalid API key"
                                 :status-code 401
                                 :response-body "Unauthorized")))
      (ok (string= (error-message error) "Invalid API key"))
      (ok (= (error-status-code error) 401))
      (ok (string= (error-response-body error) "Unauthorized")))))

(deftest rate-limit-error-creation
  (testing "Rate limit error creation"
    (let ((error (make-condition 'rate-limit-error
                                 :message "Rate limit exceeded"
                                 :status-code 429
                                 :response-body "Too many requests"
                                 :retry-after 60)))
      (ok (string= (error-message error) "Rate limit exceeded"))
      (ok (= (error-status-code error) 429))
      (ok (= (retry-after error) 60)))))

(deftest invalid-location-error-creation
  (testing "Invalid location error creation"
    (let ((error (make-condition 'invalid-location-error
                                 :message "Location not found"
                                 :status-code 400
                                 :response-body "Bad request"
                                 :location "Invalid City")))
      (ok (string= (error-message error) "Location not found"))
      (ok (= (error-status-code error) 400))
      (ok (string= (error-location error) "Invalid City")))))

(deftest validation-error-creation
  (testing "Validation error creation"
    (let ((error (make-condition 'validation-error
                                 :message "Invalid parameter"
                                 :parameter :unit-group
                                 :value :invalid)))
      (ok (string= (error-message error) "Invalid parameter"))
      (ok (eq (error-parameter error) :unit-group))
      (ok (eq (error-value error) :invalid)))))
