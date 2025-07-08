(uiop:define-package #:visualcrossing
  (:use #:cl)
  (:nicknames #:vc)
  (:import-from #:visualcrossing/client
                #:*api-key*)
  (:use-reexport #:visualcrossing/data
                 #:visualcrossing/errors)
  (:export #:*api-key*
           #:timeline-weather
           #:timeline-weather-multi
           #:current-conditions
           #:forecast-weather
           #:historical-weather))
(in-package #:visualcrossing)

(defun timeline-weather (location &key
                         (start-date nil)
                         (end-date nil)
                         (unit-group :us)
                         (include-hours t)
                         (include-current t)
                         (include-alerts t)
                         (elements nil)
                         (timezone nil)
                         (icon-set :icons1)
                         (lang :en)
                         (content-type :json)
                         (api-key nil))
  "Retrieve weather data for a location using the Timeline API.

  Parameters:
  - location: Address, coordinates, or location name
  - start-date: Start date (YYYY-MM-DD format or 'today', 'yesterday', 'tomorrow')
  - end-date: End date (YYYY-MM-DD format)
  - unit-group: Unit system (:us, :uk, :metric, :base)
  - include-hours: Include hourly data
  - include-current: Include current conditions
  - include-alerts: Include weather alerts
  - elements: Specific data elements to include
  - timezone: Timezone for date/time values
  - icon-set: Icon set to use (:icons1, :icons2)
  - lang: Language for text descriptions
  - content-type: Response format (:json, :csv)
  - api-key: API key (overrides global setting)

  Returns: weather-response structure with parsed data"

  (let ((response (visualcrossing/client:make-timeline-request
                   location
                   :start-date start-date
                   :end-date end-date
                   :api-key api-key
                   :unit-group unit-group
                   :include-hours include-hours
                   :include-current include-current
                   :include-alerts include-alerts
                   :elements elements
                   :timezone timezone
                   :icon-set icon-set
                   :lang lang
                   :content-type content-type)))

    (if (eq content-type :json)
        (visualcrossing/data:parse-weather-response response)
        response)))

(defun timeline-weather-multi (locations &key
                               (start-date nil)
                               (end-date nil)
                               (unit-group :us)
                               (include-hours t)
                               (elements nil)
                               (timezone nil)
                               (icon-set :icons1)
                               (lang :en)
                               (content-type :json)
                               (api-key nil))
  "Retrieve weather data for multiple locations in a single request.

  Parameters:
  - locations: List of locations (addresses, coordinates, or location names)
  - start-date: Start date (YYYY-MM-DD format or 'today', 'yesterday', 'tomorrow')
  - end-date: End date (YYYY-MM-DD format)
  - unit-group: Unit system (:us, :uk, :metric, :base)
  - include-hours: Include hourly data
  - elements: Specific data elements to include
  - timezone: Timezone for date/time values
  - icon-set: Icon set to use (:icons1, :icons2)
  - lang: Language for text descriptions
  - content-type: Response format (:json, :csv)
  - api-key: API key (overrides global setting)

  Returns: List of weather-response structures with parsed data"

  (let ((response (visualcrossing/client:make-multi-timeline-request
                   locations
                   :start-date start-date
                   :end-date end-date
                   :api-key api-key
                   :unit-group unit-group
                   :include-hours include-hours
                   :elements elements
                   :timezone timezone
                   :icon-set icon-set
                   :lang lang
                   :content-type content-type)))

    (if (eq content-type :json)
        (visualcrossing/data:parse-weather-response response)
        response)))

(defun current-conditions (location &key
                           (unit-group :us)
                           (elements nil)
                           (timezone nil)
                           (icon-set :icons1)
                           (lang :en)
                           (api-key nil))
  "Get current weather conditions for a location.

  Parameters:
  - location: Address, coordinates, or location name
  - unit-group: Unit system (:us, :uk, :metric, :base)
  - elements: Specific data elements to include
  - timezone: Timezone for date/time values
  - icon-set: Icon set to use (:icons1, :icons2)
  - lang: Language for text descriptions
  - api-key: API key (overrides global setting)

  Returns: weather-response structure with current conditions"
  (assert (and (stringp location)
               (/= 0 (length location))))

  (timeline-weather location
                   :start-date "today"
                   :end-date "today"
                   :unit-group unit-group
                   :include-hours nil
                   :include-current t
                   :include-alerts nil
                   :elements elements
                   :timezone timezone
                   :icon-set icon-set
                   :lang lang
                   :api-key api-key))

(defun forecast-weather (location &key
                         (days 15)
                         (unit-group :us)
                         (include-hours t)
                         (include-alerts t)
                         (elements nil)
                         (timezone nil)
                         (icon-set :icons1)
                         (lang :en)
                         (api-key nil))
  "Get weather forecast for a location.

  Parameters:
  - location: Address, coordinates, or location name
  - days: Number of forecast days (1-15)
  - unit-group: Unit system (:us, :uk, :metric, :base)
  - include-hours: Include hourly data
  - include-alerts: Include weather alerts
  - elements: Specific data elements to include
  - timezone: Timezone for date/time values
  - icon-set: Icon set to use (:icons1, :icons2)
  - lang: Language for text descriptions
  - api-key: API key (overrides global setting)

  Returns: weather-response structure with forecast data"

  (let ((end-date (multiple-value-bind (sec min hour date month year)
                      (decode-universal-time (+ (get-universal-time)
                                                (* days 24 60 60)))
                    (declare (ignore sec min hour))
                    (format nil "~4,'0D-~2,'0D-~2,'0D" year month date))))
    (timeline-weather location
                     :start-date "today"
                     :end-date end-date
                     :unit-group unit-group
                     :include-hours include-hours
                     :include-current t
                     :include-alerts include-alerts
                     :elements elements
                     :timezone timezone
                     :icon-set icon-set
                     :lang lang
                     :api-key api-key)))

(defun historical-weather (location start-date &key
                           (end-date nil)
                           (unit-group :us)
                           (include-hours t)
                           (elements nil)
                           (timezone nil)
                           (icon-set :icons1)
                           (lang :en)
                           (api-key nil))
  "Get historical weather data for a location.

  Parameters:
  - location: Address, coordinates, or location name
  - start-date: Start date (YYYY-MM-DD format)
  - end-date: End date (YYYY-MM-DD format, defaults to start-date)
  - unit-group: Unit system (:us, :uk, :metric, :base)
  - include-hours: Include hourly data
  - elements: Specific data elements to include
  - timezone: Timezone for date/time values
  - icon-set: Icon set to use (:icons1, :icons2)
  - lang: Language for text descriptions
  - api-key: API key (overrides global setting)

  Returns: weather-response structure with historical data"

  (timeline-weather location
                   :start-date start-date
                   :end-date end-date
                   :unit-group unit-group
                   :include-hours include-hours
                   :include-current nil
                   :include-alerts nil
                   :elements elements
                   :timezone timezone
                   :icon-set icon-set
                   :lang lang
                   :api-key api-key))
