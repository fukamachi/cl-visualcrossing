(defpackage #:visualcrossing/tests/client
  (:use #:cl
        #:rove)
  (:import-from #:visualcrossing/client
                #:*api-key*
                #:*base-url*
                #:*timeout*
                #:*retry-attempts*
                #:*retry-delay*
                #:get-api-key
                #:build-url))
(in-package #:visualcrossing/tests/client)

(deftest api-key-configuration
  (testing "API key configuration"
    (testing "Dynamic variable"
      (let ((original-key *api-key*))
        (unwind-protect
             (progn
               (setf *api-key* "test-key")
               (ok (string= (get-api-key) "test-key")))
          (setf *api-key* original-key))))

    (testing "Provided key overrides"
      (let ((original-key *api-key*))
        (unwind-protect
             (progn
               (setf *api-key* "dynamic-key")
               (ok (string= (get-api-key "provided-key") "provided-key")))
          (setf *api-key* original-key))))

    (testing "No API key available"
      (let ((original-key *api-key*)
            (original-env (uiop:getenv "VISUAL_CROSSING_WEATHER_API_KEY")))
        (unwind-protect
             (progn
               (setf *api-key* nil)
               (setf (uiop:getenv "VISUAL_CROSSING_WEATHER_API_KEY") nil)
               (ok (signals (get-api-key) 'visualcrossing/errors:authentication-error)))
          (setf *api-key* original-key)
          (when original-env
            (setf (uiop:getenv "VISUAL_CROSSING_WEATHER_API_KEY") original-env)))))))

(deftest build-url-test
  (testing "URL building"
    (testing "Location only"
      (let ((url (build-url "New York, NY")))
        (ok (string= url (format nil "~A/New York, NY" *base-url*)))))

    (testing "Location with start date"
      (let ((url (build-url "New York, NY" "2024-01-01")))
        (ok (string= url (format nil "~A/New York, NY/2024-01-01" *base-url*)))))

    (testing "Location with start and end date"
      (let ((url (build-url "New York, NY" "2024-01-01" "2024-01-07")))
        (ok (string= url (format nil "~A/New York, NY/2024-01-01/2024-01-07" *base-url*)))))))

(deftest configuration-variables
  (testing "Configuration variables"
    (testing "Default values"
      (ok (stringp *base-url*))
      (ok (integerp *timeout*))
      (ok (integerp *retry-attempts*))
      (ok (integerp *retry-delay*))
      (ok (> *timeout* 0))
      (ok (> *retry-attempts* 0))
      (ok (>= *retry-delay* 0)))

    (testing "Base URL format"
      (ok (str:starts-with? "https://" *base-url*))
      (ok (str:contains? "timeline" *base-url*)))))
