(defpackage #:visualcrossing/tests/data
  (:use #:cl
        #:rove)
  (:import-from #:visualcrossing/data
                #:weather-response
                #:make-weather-response
                #:weather-response-p
                #:weather-response-query-cost
                #:weather-response-latitude
                #:weather-response-longitude
                #:weather-response-resolved-address
                #:weather-response-address
                #:weather-response-timezone
                #:weather-response-days
                #:weather-response-current-conditions
                #:weather-response-alerts
                #:weather-day
                #:make-weather-day
                #:weather-day-p
                #:weather-day-datetime
                #:weather-day-temp
                #:weather-day-tempmax
                #:weather-day-tempmin
                #:weather-day-humidity
                #:weather-day-precip
                #:weather-day-conditions
                #:weather-day-description
                #:weather-day-hours
                #:weather-hour
                #:make-weather-hour
                #:weather-hour-p
                #:weather-hour-datetime
                #:weather-hour-temp
                #:weather-hour-humidity
                #:weather-hour-precip
                #:weather-hour-conditions
                #:weather-hour-description
                #:current-conditions
                #:make-current-conditions
                #:current-conditions-p
                #:current-conditions-datetime
                #:current-conditions-temp
                #:current-conditions-humidity
                #:current-conditions-precip
                #:current-conditions-conditions
                #:current-conditions-description
                #:weather-alert
                #:make-weather-alert
                #:weather-alert-p
                #:weather-alert-event
                #:weather-alert-description
                #:weather-alert-severity
                #:parse-weather-response
                #:parse-weather-day
                #:parse-weather-hour
                #:parse-current-conditions
                #:parse-weather-alert))
(in-package #:visualcrossing/tests/data)

(defparameter *sample-json-response*
  "{\"queryCost\":1,\"latitude\":40.7146,\"longitude\":-74.0071,\"resolvedAddress\":\"New York, NY, United States\",\"address\":\"New York, NY\",\"timezone\":\"America/New_York\",\"tzoffset\":-4.0,\"description\":\"Similar temperatures continuing with no rain expected.\",\"days\":[{\"datetime\":\"2025-07-08\",\"datetimeEpoch\":1751947200,\"tempmax\":94.4,\"tempmin\":77.2,\"temp\":84.5,\"feelslikemax\":99.8,\"feelslikemin\":77.2,\"feelslike\":87.7,\"dew\":70.8,\"humidity\":65.9,\"precip\":0.028,\"precipprob\":59.0,\"precipcover\":8.33,\"preciptype\":[\"rain\"],\"snow\":0.0,\"snowdepth\":0.0,\"windgust\":15.7,\"windspeed\":8.7,\"winddir\":248.0,\"pressure\":30.02,\"cloudcover\":65.4,\"visibility\":9.9,\"uvindex\":7.0,\"severerisk\":10.0,\"conditions\":\"Partly cloudy\",\"description\":\"Partly cloudy throughout the day.\",\"icon\":\"partly-cloudy-day\",\"stations\":null,\"source\":\"fcst\",\"sunrise\":\"05:30:59\",\"sunset\":\"20:14:27\",\"moonphase\":0.38,\"currentConditions\":{\"datetime\":\"15:21:00\",\"datetimeEpoch\":1751982060,\"temp\":89.8,\"feelslike\":94.6,\"humidity\":59.0,\"dew\":72.3,\"precip\":0.0,\"precipprob\":0.0,\"snow\":0.0,\"snowdepth\":0.0,\"preciptype\":null,\"windgust\":10.1,\"windspeed\":6.9,\"winddir\":270.0,\"pressure\":29.98,\"visibility\":10.0,\"cloudcover\":75.0,\"uvindex\":6.0,\"conditions\":\"Partly cloudy\",\"icon\":\"partly-cloudy-day\",\"stations\":[\"KJFK\",\"KLGA\",\"KNYC\",\"KTEB\"],\"source\":\"obs\"},\"hours\":[{\"datetime\":\"00:00:00\",\"datetimeEpoch\":1751892000,\"temp\":79.5,\"feelslike\":79.5,\"humidity\":68.0,\"dew\":68.8,\"precip\":0.0,\"precipprob\":0.0,\"snow\":0.0,\"snowdepth\":0.0,\"preciptype\":null,\"windgust\":12.3,\"windspeed\":6.5,\"winddir\":255.0,\"pressure\":30.08,\"visibility\":10.0,\"cloudcover\":45.0,\"uvindex\":0.0,\"conditions\":\"Partly cloudy\",\"icon\":\"partly-cloudy-night\",\"stations\":[\"KJFK\",\"KLGA\",\"KNYC\",\"KTEB\"],\"source\":\"obs\"}]}]}")

(defparameter *sample-weather-alert*
  "{\"event\":\"Heat Advisory\",\"headline\":\"Heat Advisory in effect\",\"description\":\"Dangerous heat conditions\",\"severity\":\"Moderate\",\"areas\":[\"New York\"],\"onset\":\"2025-07-08T12:00:00\",\"expires\":\"2025-07-08T20:00:00\",\"effective\":\"2025-07-08T12:00:00\"}")

(deftest weather-response-structure
  (testing "Weather response structure creation"
    (let ((response (make-weather-response
                     :query-cost 1
                     :latitude 40.7146
                     :longitude -74.0071
                     :resolved-address "New York, NY, United States"
                     :address "New York, NY"
                     :timezone "America/New_York")))
      (ok (= (weather-response-query-cost response) 1))
      (ok (= (weather-response-latitude response) 40.7146))
      (ok (= (weather-response-longitude response) -74.0071))
      (ok (string= (weather-response-resolved-address response) "New York, NY, United States"))
      (ok (string= (weather-response-address response) "New York, NY"))
      (ok (string= (weather-response-timezone response) "America/New_York")))))

(deftest weather-day-structure
  (testing "Weather day structure creation"
    (let ((day (make-weather-day
                :datetime "2025-07-08"
                :temp 84.5
                :tempmax 94.4
                :tempmin 77.2
                :humidity 65.9
                :precip 0.028
                :conditions "Partly cloudy"
                :description "Partly cloudy throughout the day.")))
      (ok (string= (weather-day-datetime day) "2025-07-08"))
      (ok (= (weather-day-temp day) 84.5))
      (ok (= (weather-day-tempmax day) 94.4))
      (ok (= (weather-day-tempmin day) 77.2))
      (ok (= (weather-day-humidity day) 65.9))
      (ok (= (weather-day-precip day) 0.028))
      (ok (string= (weather-day-conditions day) "Partly cloudy"))
      (ok (string= (weather-day-description day) "Partly cloudy throughout the day.")))))

(deftest weather-hour-structure
  (testing "Weather hour structure creation"
    (let ((hour (make-weather-hour
                 :datetime "00:00:00"
                 :temp 79.5
                 :humidity 68.0
                 :precip 0.0
                 :conditions "Partly cloudy"
                 :description "Clear sky")))
      (ok (string= (weather-hour-datetime hour) "00:00:00"))
      (ok (= (weather-hour-temp hour) 79.5))
      (ok (= (weather-hour-humidity hour) 68.0))
      (ok (= (weather-hour-precip hour) 0.0))
      (ok (string= (weather-hour-conditions hour) "Partly cloudy"))
      (ok (string= (weather-hour-description hour) "Clear sky")))))

(deftest current-conditions-structure
  (testing "Current conditions structure creation"
    (let ((current (make-current-conditions
                    :datetime "15:21:00"
                    :temp 89.8
                    :humidity 59.0
                    :precip 0.0
                    :conditions "Partly cloudy"
                    :description "Current conditions")))
      (ok (string= (current-conditions-datetime current) "15:21:00"))
      (ok (= (current-conditions-temp current) 89.8))
      (ok (= (current-conditions-humidity current) 59.0))
      (ok (= (current-conditions-precip current) 0.0))
      (ok (string= (current-conditions-conditions current) "Partly cloudy"))
      (ok (string= (current-conditions-description current) "Current conditions")))))

(deftest weather-alert-structure
  (testing "Weather alert structure creation"
    (let ((alert (make-weather-alert
                  :event "Heat Advisory"
                  :description "Dangerous heat conditions"
                  :severity "Moderate")))
      (ok (string= (weather-alert-event alert) "Heat Advisory"))
      (ok (string= (weather-alert-description alert) "Dangerous heat conditions"))
      (ok (string= (weather-alert-severity alert) "Moderate")))))

(deftest parse-weather-response-integration
  (testing "Parse complete weather response from JSON"
    (let ((response (parse-weather-response *sample-json-response*)))
      (ok (weather-response-p response))
      (ok (= (weather-response-query-cost response) 1))
      (ok (= (weather-response-latitude response) 40.7146))
      (ok (= (weather-response-longitude response) -74.0071))
      (ok (string= (weather-response-resolved-address response) "New York, NY, United States"))
      (ok (string= (weather-response-address response) "New York, NY"))
      (ok (string= (weather-response-timezone response) "America/New_York"))

      ;; Test days data
      (ok (= (length (weather-response-days response)) 1))
      (let ((day (first (weather-response-days response))))
        (ok (weather-day-p day))
        (ok (string= (weather-day-datetime day) "2025-07-08"))
        (ok (= (weather-day-temp day) 84.5))
        (ok (= (weather-day-tempmax day) 94.4))
        (ok (= (weather-day-tempmin day) 77.2))
        (ok (= (weather-day-humidity day) 65.9))
        (ok (string= (weather-day-conditions day) "Partly cloudy"))

        ;; Test hours data
        (ok (= (length (weather-day-hours day)) 1))
        (let ((hour (first (weather-day-hours day))))
          (ok (weather-hour-p hour))
          (ok (string= (weather-hour-datetime hour) "00:00:00"))
          (ok (= (weather-hour-temp hour) 79.5))
          (ok (= (weather-hour-humidity hour) 68.0))))

      ;; Test current conditions
      (ok (current-conditions-p (weather-response-current-conditions response)))
      (let ((current (weather-response-current-conditions response)))
        (ok (string= (current-conditions-datetime current) "15:21:00"))
        (ok (= (current-conditions-temp current) 89.8))
        (ok (= (current-conditions-humidity current) 59.0))
        (ok (string= (current-conditions-conditions current) "Partly cloudy"))))))

(deftest parse-weather-alert-test
  (testing "Parse weather alert from JSON"
    (let* ((alert-json (yason:parse *sample-weather-alert*))
           (alert (parse-weather-alert alert-json)))
      (ok (weather-alert-p alert))
      (ok (string= (weather-alert-event alert) "Heat Advisory"))
      (ok (string= (weather-alert-description alert) "Dangerous heat conditions"))
      (ok (string= (weather-alert-severity alert) "Moderate")))))

(deftest parse-edge-cases
  (testing "Parse edge cases and null values"
    (let ((minimal-json "{\"queryCost\":1,\"latitude\":40.0,\"longitude\":-74.0,\"resolvedAddress\":\"Test\",\"address\":\"Test\",\"timezone\":\"UTC\",\"tzoffset\":0,\"days\":[]}"))
      (let ((response (parse-weather-response minimal-json)))
        (ok (weather-response-p response))
        (ok (= (weather-response-query-cost response) 1))
        (ok (null (weather-response-days response)))
        (ok (null (weather-response-current-conditions response)))
        (ok (null (weather-response-alerts response)))))))

(deftest camel-case-conversion
  (testing "CamelCase to Lisp symbol conversion"
    (let ((test-json "{\"queryCost\":1,\"resolvedAddress\":\"test\",\"currentConditions\":{\"temp\":80},\"datetimeEpoch\":123}"))
      (let ((json-data (yason:parse test-json)))
        ;; Test that the conversion works by checking if we can access the data
        (ok (not (null (visualcrossing/data::get-json-value "queryCost" json-data))))
        (ok (not (null (visualcrossing/data::get-json-value "resolvedAddress" json-data))))
        (ok (not (null (visualcrossing/data::get-json-value "currentConditions" json-data))))
        (ok (not (null (visualcrossing/data::get-json-value "datetimeEpoch" json-data))))))))
