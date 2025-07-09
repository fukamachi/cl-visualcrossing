(defpackage #:visualcrossing/tests/main
  (:use #:cl
        #:rove
        #:visualcrossing/tests/utils)
  (:import-from #:visualcrossing
                #:*api-key*
                #:timeline-weather
                #:timeline-weather-multi
                #:current-conditions
                #:forecast-weather
                #:historical-weather))
(in-package #:visualcrossing/tests/main)

(defparameter *sample-json-response*
  "{\"queryCost\":1,\"latitude\":40.7146,\"longitude\":-74.0071,\"resolvedAddress\":\"New York, NY, United States\",\"address\":\"New York, NY\",\"timezone\":\"America/New_York\",\"tzoffset\":-4.0,\"description\":\"Similar temperatures continuing with no rain expected.\",\"days\":[{\"datetime\":\"2025-07-08\",\"datetimeEpoch\":1751947200,\"tempmax\":94.4,\"tempmin\":77.2,\"temp\":84.5,\"feelslikemax\":99.8,\"feelslikemin\":77.2,\"feelslike\":87.7,\"dew\":70.8,\"humidity\":65.9,\"precip\":0.028,\"precipprob\":59.0,\"precipcover\":8.33,\"preciptype\":[\"rain\"],\"snow\":0.0,\"snowdepth\":0.0,\"windgust\":15.7,\"windspeed\":8.7,\"winddir\":248.0,\"pressure\":30.02,\"cloudcover\":65.4,\"visibility\":9.9,\"uvindex\":7.0,\"severerisk\":10.0,\"conditions\":\"Partly cloudy\",\"description\":\"Partly cloudy throughout the day.\",\"icon\":\"partly-cloudy-day\",\"stations\":null,\"source\":\"fcst\",\"sunrise\":\"05:30:59\",\"sunset\":\"20:14:27\",\"moonphase\":0.38,\"currentConditions\":{\"datetime\":\"15:21:00\",\"datetimeEpoch\":1751982060,\"temp\":89.8,\"feelslike\":94.6,\"humidity\":59.0,\"dew\":72.3,\"precip\":0.0,\"precipprob\":0.0,\"snow\":0.0,\"snowdepth\":0.0,\"preciptype\":null,\"windgust\":10.1,\"windspeed\":6.9,\"winddir\":270.0,\"pressure\":29.98,\"visibility\":10.0,\"cloudcover\":75.0,\"uvindex\":6.0,\"conditions\":\"Partly cloudy\",\"icon\":\"partly-cloudy-day\",\"stations\":[\"KJFK\",\"KLGA\",\"KNYC\",\"KTEB\"],\"source\":\"obs\"},\"hours\":[{\"datetime\":\"00:00:00\",\"datetimeEpoch\":1751892000,\"temp\":79.5,\"feelslike\":79.5,\"humidity\":68.0,\"dew\":68.8,\"precip\":0.0,\"precipprob\":0.0,\"snow\":0.0,\"snowdepth\":0.0,\"preciptype\":null,\"windgust\":12.3,\"windspeed\":6.5,\"winddir\":255.0,\"pressure\":30.08,\"visibility\":10.0,\"cloudcover\":45.0,\"uvindex\":0.0,\"conditions\":\"Partly cloudy\",\"icon\":\"partly-cloudy-night\",\"stations\":[\"KJFK\",\"KLGA\",\"KNYC\",\"KTEB\"],\"source\":\"obs\"}]}]}")

(deftest test-api-key-exists
  (ok (uiop:getenvp "TEST_VISUAL_CROSSING_WEATHER_API_KEY")))

(deftest function-signatures
  (testing "Function signatures"
    (testing "timeline-weather function exists"
      (ok (fboundp 'timeline-weather)))

    (testing "timeline-weather-multi function exists"
      (ok (fboundp 'timeline-weather-multi)))

    (testing "current-conditions function exists"
      (ok (fboundp 'current-conditions)))

    (testing "forecast-weather function exists"
      (ok (fboundp 'forecast-weather)))

    (testing "historical-weather function exists"
      (ok (fboundp 'historical-weather)))))

(deftest parameter-validation
  (testing "Parameter validation"
    (testing "Invalid API key results in error"
      (with-test-api-key ("")
        (ok (signals (current-conditions "New York, NY")
                'visualcrossing/errors:authentication-error))))

    (testing "Invalid location validation"
      (with-test-api-key ()
        (ok (signals (current-conditions "")))))

    (testing "Invalid unit group validation"
      (with-test-api-key ()
        (ok (signals (current-conditions "New York, NY" :unit-group :invalid)
                'visualcrossing/errors:validation-error))))))

(deftest mock-response-parsing
  (testing "Mock response parsing"
    (testing "Parse sample JSON response"
      (let ((response (visualcrossing/data:parse-weather-response *sample-json-response*)))
        (ok (visualcrossing/data:weather-response-p response))
        (ok (= (visualcrossing/data:weather-response-query-cost response) 1))
        (ok (string= (visualcrossing/data:weather-response-resolved-address response)
                     "New York, NY, United States"))
        (ok (= (length (visualcrossing/data:weather-response-days response)) 1))
        (ok (not (null (visualcrossing/data:weather-response-current-conditions response))))

        (let ((day (first (visualcrossing/data:weather-response-days response))))
          (ok (= (visualcrossing/data:weather-temp day) 84.5))
          (ok (string= (visualcrossing/data:weather-conditions day) "Partly cloudy")))

        (let ((current (visualcrossing/data:weather-response-current-conditions response)))
          (ok (= (visualcrossing/data:weather-temp current) 89.8d0))
          (ok (string= (visualcrossing/data:weather-conditions current) "Partly cloudy")))))))

(deftest keyword-argument-handling
  (testing "Keyword argument handling"
    (testing "timeline-weather accepts all keyword arguments"
      (with-test-api-key ("")
        (ok (timeline-weather "New York, NY"
                              :start-date "2024-01-01"
                              :end-date "2024-01-07"
                              :unit-group :metric
                              :include-hours t
                              :include-current t
                              :include-alerts t
                              :elements nil
                              :timezone nil
                              :icon-set :icons1
                              :lang :en
                              :content-type :json
                              :api-key (uiop:getenv "TEST_VISUAL_CROSSING_WEATHER_API_KEY")))))

    (testing "current-conditions accepts keyword arguments"
      (with-test-api-key ("")
        (ok (current-conditions "New York, NY"
                                :unit-group :metric
                                :elements nil
                                :timezone nil
                                :icon-set :icons1
                                :lang :en
                                :api-key (uiop:getenv "TEST_VISUAL_CROSSING_WEATHER_API_KEY")))))))

(deftest integration-smoke-tests
  (testing "Integration smoke tests"
    (testing "Functions can be called without throwing unexpected errors"
      ;; These tests may fail due to network issues or invalid API keys,
      ;; but they shouldn't fail due to argument errors or other code issues
      (with-test-api-key ()
        ;; timeline-weather
        (ok (timeline-weather "New York, NY"))

        ;; current-conditions
        (ok (current-conditions "New York, NY"))

        ;; forecast-weather
        (ok (forecast-weather "New York, NY"))

        ;; historical-weather
        (ok (historical-weather "New York, NY" "2024-01-01"))

        ;; timeline-weather-multi
        (ok (timeline-weather-multi '("New York, NY" "London, UK")))))))
