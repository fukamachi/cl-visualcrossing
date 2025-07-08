(defpackage #:visualcrossing/tests/params
  (:use #:cl
        #:rove)
  (:import-from #:visualcrossing/params
                #:validate-location
                #:validate-date
                #:validate-unit-group
                #:validate-icon-set
                #:validate-lang
                #:validate-content-type
                #:build-query-params
                #:format-date
                #:format-locations))
(in-package #:visualcrossing/tests/params)

(deftest validate-location-test
  (testing "Location validation"
    (testing "Valid string locations"
      (ok (string= (validate-location "New York, NY") "New York, NY"))
      (ok (string= (validate-location "40.7128,-74.0060") "40.7128,-74.0060")))

    (testing "Valid list locations"
      (ok (equal (validate-location '("New York" "London")) '("New York" "London"))))

    (testing "Invalid locations"
      (ok (signals (validate-location 123) 'visualcrossing/errors:validation-error))
      (ok (signals (validate-location "") 'visualcrossing/errors:validation-error))
      (ok (signals (validate-location '("" "London")) 'visualcrossing/errors:validation-error))
      (ok (signals (validate-location '("New York" 123)) 'visualcrossing/errors:validation-error)))))

(deftest validate-date-test
  (testing "Date validation"
    (testing "Valid dates"
      (ok (string= (validate-date "2024-01-01") "2024-01-01"))
      (ok (string= (validate-date "today") "today"))
      (ok (string= (validate-date "yesterday") "yesterday"))
      (ok (string= (validate-date "tomorrow") "tomorrow"))
      (ok (null (validate-date nil))))

    (testing "Invalid dates"
      (ok (signals (validate-date "2024-1-1") 'visualcrossing/errors:validation-error))
      (ok (signals (validate-date "invalid") 'visualcrossing/errors:validation-error))
      (ok (signals (validate-date 123) 'visualcrossing/errors:validation-error)))))

(deftest validate-unit-group-test
  (testing "Unit group validation"
    (testing "Valid unit groups"
      (ok (eq (validate-unit-group :us) :us))
      (ok (eq (validate-unit-group :uk) :uk))
      (ok (eq (validate-unit-group :metric) :metric))
      (ok (eq (validate-unit-group :base) :base)))

    (testing "Invalid unit groups"
      (ok (signals (validate-unit-group :invalid) 'visualcrossing/errors:validation-error))
      (ok (signals (validate-unit-group "us") 'visualcrossing/errors:validation-error)))))

(deftest validate-icon-set-test
  (testing "Icon set validation"
    (testing "Valid icon sets"
      (ok (eq (validate-icon-set :icons1) :icons1))
      (ok (eq (validate-icon-set :icons2) :icons2)))

    (testing "Invalid icon sets"
      (ok (signals (validate-icon-set :invalid) 'visualcrossing/errors:validation-error))
      (ok (signals (validate-icon-set "icons1") 'visualcrossing/errors:validation-error)))))

(deftest validate-content-type-test
  (testing "Content type validation"
    (testing "Valid content types"
      (ok (eq (validate-content-type :json) :json))
      (ok (eq (validate-content-type :csv) :csv)))

    (testing "Invalid content types"
      (ok (signals (validate-content-type :xml) 'visualcrossing/errors:validation-error))
      (ok (signals (validate-content-type "json") 'visualcrossing/errors:validation-error)))))

(deftest format-locations-test
  (testing "Format locations"
    (testing "String input"
      (ok (string= (format-locations "New York") "New York")))

    (testing "List input"
      (ok (string= (format-locations '("New York" "London" "Tokyo")) "New York|London|Tokyo")))

    (testing "Invalid input"
      (ok (signals (format-locations 123) 'visualcrossing/errors:validation-error)))))

(deftest build-query-params-test
  (testing "Build query parameters"
    (testing "Basic parameters"
      (let ((params (build-query-params :location "New York" :unit-group :metric :api-key "test-key")))
        (ok (not (null params)))
        (ok (string= (cdr (assoc "location" params :test #'string=)) "New York"))
        (ok (string= (cdr (assoc "unitGroup" params :test #'string=)) "metric"))
        (ok (string= (cdr (assoc "key" params :test #'string=)) "test-key"))))

    (testing "Date parameters"
      (let ((params (build-query-params :start-date "2024-01-01" :end-date "2024-01-07")))
        (ok (string= (cdr (assoc "startDateTime" params :test #'string=)) "2024-01-01"))
        (ok (string= (cdr (assoc "endDateTime" params :test #'string=)) "2024-01-07"))))

    (testing "Boolean parameters"
      (let ((params (build-query-params :include-hours t :include-current t :include-alerts t)))
        (ok (member "hours" (mapcar #'cdr (remove-if-not (lambda (p) (string= (car p) "include")) params)) :test #'string=))
        (ok (member "current" (mapcar #'cdr (remove-if-not (lambda (p) (string= (car p) "include")) params)) :test #'string=))
        (ok (member "alerts" (mapcar #'cdr (remove-if-not (lambda (p) (string= (car p) "include")) params)) :test #'string=))))

    (testing "Days parameter"
      (let ((params (build-query-params :days 7)))
        (ok (string= (cdr (assoc "days" params :test #'string=)) "7"))))

    (testing "Invalid days parameter"
      (ok (signals (build-query-params :days 0) 'visualcrossing/errors:validation-error))
      (ok (signals (build-query-params :days 20) 'visualcrossing/errors:validation-error))
      (ok (signals (build-query-params :days "7") 'visualcrossing/errors:validation-error)))))
