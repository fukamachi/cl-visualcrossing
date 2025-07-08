(defpackage #:visualcrossing/data
  (:use #:cl)
  (:export #:weather-response
           #:make-weather-response
           #:weather-response-p
           #:weather-response-query-cost
           #:weather-response-latitude
           #:weather-response-longitude
           #:weather-response-resolved-address
           #:weather-response-address
           #:weather-response-timezone
           #:weather-response-tzoffset
           #:weather-response-elevation
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
(in-package #:visualcrossing/data)

(defstruct weather-response
  query-cost
  latitude
  longitude
  resolved-address
  address
  timezone
  tzoffset
  elevation
  days
  current-conditions
  alerts)

(defstruct weather-day
  datetime
  datetimeepoch
  temp
  tempmax
  tempmin
  feelslike
  feelslikemax
  feelslikemin
  humidity
  precip
  precipprob
  precipcover
  preciptype
  snow
  snowdepth
  windgust
  windspeed
  winddir
  pressure
  cloudcover
  visibility
  uvindex
  conditions
  description
  icon
  sunrise
  sunset
  moonphase
  hours)

(defstruct weather-hour
  datetime
  datetimeepoch
  temp
  feelslike
  humidity
  precip
  precipprob
  preciptype
  snow
  snowdepth
  windgust
  windspeed
  winddir
  pressure
  cloudcover
  visibility
  uvindex
  conditions
  description
  icon)

(defstruct current-conditions
  datetime
  datetimeepoch
  temp
  feelslike
  humidity
  precip
  precipprob
  preciptype
  snow
  snowdepth
  windgust
  windspeed
  winddir
  pressure
  cloudcover
  visibility
  uvindex
  conditions
  description
  icon)

(defstruct weather-alert
  event
  headline
  description
  severity
  areas
  onset
  expires
  effective)

(defun get-json-value (key data)
  "Get value from JSON data using CL-JSON."
  (gethash key data))

(defun parse-weather-response (json-data)
  "Parse JSON response into weather-response structure."
  (check-type json-data string)
  (let ((data (yason:parse json-data)))
    (make-weather-response
     :query-cost (get-json-value "queryCost" data)
     :latitude (get-json-value "latitude" data)
     :longitude (get-json-value "longitude" data)
     :resolved-address (get-json-value "resolvedAddress" data)
     :address (get-json-value "address" data)
     :timezone (get-json-value "timezone" data)
     :tzoffset (get-json-value "tzoffset" data)
     :elevation (get-json-value "elevation" data)
     :days (let ((days (get-json-value "days" data)))
             (when days (mapcar #'parse-weather-day days)))
     :current-conditions (let ((days (get-json-value "days" data)))
                          (when (and days (> (length days) 0))
                            (let ((cc (get-json-value "currentConditions" (first days))))
                              (when cc (parse-current-conditions cc)))))
     :alerts (let ((alerts (get-json-value "alerts" data)))
               (when alerts (mapcar #'parse-weather-alert alerts))))))

(defun parse-weather-day (day-data)
  "Parse JSON day data into weather-day structure."
  (make-weather-day
   :datetime (get-json-value "datetime" day-data)
   :datetimeepoch (get-json-value "datetimeEpoch" day-data)
   :temp (get-json-value "temp" day-data)
   :tempmax (get-json-value "tempmax" day-data)
   :tempmin (get-json-value "tempmin" day-data)
   :feelslike (get-json-value "feelslike" day-data)
   :feelslikemax (get-json-value "feelslikemax" day-data)
   :feelslikemin (get-json-value "feelslikemin" day-data)
   :humidity (get-json-value "humidity" day-data)
   :precip (get-json-value "precip" day-data)
   :precipprob (get-json-value "precipprob" day-data)
   :precipcover (get-json-value "precipcover" day-data)
   :preciptype (get-json-value "preciptype" day-data)
   :snow (get-json-value "snow" day-data)
   :snowdepth (get-json-value "snowdepth" day-data)
   :windgust (get-json-value "windgust" day-data)
   :windspeed (get-json-value "windspeed" day-data)
   :winddir (get-json-value "winddir" day-data)
   :pressure (get-json-value "pressure" day-data)
   :cloudcover (get-json-value "cloudcover" day-data)
   :visibility (get-json-value "visibility" day-data)
   :uvindex (get-json-value "uvindex" day-data)
   :conditions (get-json-value "conditions" day-data)
   :description (get-json-value "description" day-data)
   :icon (get-json-value "icon" day-data)
   :sunrise (get-json-value "sunrise" day-data)
   :sunset (get-json-value "sunset" day-data)
   :moonphase (get-json-value "moonphase" day-data)
   :hours (let ((hours (get-json-value "hours" day-data)))
            (when hours (mapcar #'parse-weather-hour hours)))))

(defun parse-weather-hour (hour-data)
  "Parse JSON hour data into weather-hour structure."
  (make-weather-hour
   :datetime (get-json-value "datetime" hour-data)
   :datetimeepoch (get-json-value "datetimeEpoch" hour-data)
   :temp (get-json-value "temp" hour-data)
   :feelslike (get-json-value "feelslike" hour-data)
   :humidity (get-json-value "humidity" hour-data)
   :precip (get-json-value "precip" hour-data)
   :precipprob (get-json-value "precipprob" hour-data)
   :preciptype (get-json-value "preciptype" hour-data)
   :snow (get-json-value "snow" hour-data)
   :snowdepth (get-json-value "snowdepth" hour-data)
   :windgust (get-json-value "windgust" hour-data)
   :windspeed (get-json-value "windspeed" hour-data)
   :winddir (get-json-value "winddir" hour-data)
   :pressure (get-json-value "pressure" hour-data)
   :cloudcover (get-json-value "cloudcover" hour-data)
   :visibility (get-json-value "visibility" hour-data)
   :uvindex (get-json-value "uvindex" hour-data)
   :conditions (get-json-value "conditions" hour-data)
   :description (get-json-value "description" hour-data)
   :icon (get-json-value "icon" hour-data)))

(defun parse-current-conditions (cc-data)
  "Parse JSON current conditions data into current-conditions structure."
  (make-current-conditions
   :datetime (get-json-value "datetime" cc-data)
   :datetimeepoch (get-json-value "datetimeEpoch" cc-data)
   :temp (get-json-value "temp" cc-data)
   :feelslike (get-json-value "feelslike" cc-data)
   :humidity (get-json-value "humidity" cc-data)
   :precip (get-json-value "precip" cc-data)
   :precipprob (get-json-value "precipprob" cc-data)
   :preciptype (get-json-value "preciptype" cc-data)
   :snow (get-json-value "snow" cc-data)
   :snowdepth (get-json-value "snowdepth" cc-data)
   :windgust (get-json-value "windgust" cc-data)
   :windspeed (get-json-value "windspeed" cc-data)
   :winddir (get-json-value "winddir" cc-data)
   :pressure (get-json-value "pressure" cc-data)
   :cloudcover (get-json-value "cloudcover" cc-data)
   :visibility (get-json-value "visibility" cc-data)
   :uvindex (get-json-value "uvindex" cc-data)
   :conditions (get-json-value "conditions" cc-data)
   :description (get-json-value "description" cc-data)
   :icon (get-json-value "icon" cc-data)))

(defun parse-weather-alert (alert-data)
  "Parse JSON alert data into weather-alert structure."
  (make-weather-alert
   :event (get-json-value "event" alert-data)
   :headline (get-json-value "headline" alert-data)
   :description (get-json-value "description" alert-data)
   :severity (get-json-value "severity" alert-data)
   :areas (get-json-value "areas" alert-data)
   :onset (get-json-value "onset" alert-data)
   :expires (get-json-value "expires" alert-data)
   :effective (get-json-value "effective" alert-data)))
