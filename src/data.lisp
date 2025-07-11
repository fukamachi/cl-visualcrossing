(defpackage #:visualcrossing/data
  (:use #:cl)
  (:export #:weather-multi-response
           #:weather-multi-response-query-cost
           #:weather-multi-response-locations
           #:parse-weather-multi-response
           #:weather-response
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
           #:weather-datetime
           #:weather-datetime-epoch
           #:weather-temp
           #:weather-feelslike
           #:weather-dew
           #:weather-humidity
           #:weather-precip
           #:weather-precipprob
           #:weather-preciptype
           #:weather-snow
           #:weather-snowdepth
           #:weather-windgust
           #:weather-windspeed
           #:weather-winddir
           #:weather-pressure
           #:weather-cloudcover
           #:weather-visibility
           #:weather-solarradiation
           #:weather-solarenergy
           #:weather-uvindex
           #:weather-conditions
           #:weather-description
           #:weather-icon
           #:weather-stations
           #:weather-source
           #:weather-day
           #:make-weather-day
           #:weather-day-p
           #:weather-day-tempmax
           #:weather-day-tempmin
           #:weather-day-feelslikemax
           #:weather-day-feelslikemin
           #:weather-day-windspeedmax
           #:weather-day-windspeedmean
           #:weather-day-windspeedmin
           #:weather-day-precipcover
           #:weather-day-sunrise
           #:weather-day-sunrise-epoch
           #:weather-day-sunset
           #:weather-day-sunset-epoch
           #:weather-day-moonphase
           #:weather-day-hours
           #:weather-hour
           #:make-weather-hour
           #:weather-hour-p
           #:current-conditions
           #:make-current-conditions
           #:current-conditions-p
           #:weather-alert
           #:make-weather-alert
           #:weather-alert-p
           #:weather-alert-event
           #:weather-alert-headline
           #:weather-alert-description
           #:weather-alert-severity
           #:weather-alert-areas
           #:weather-alert-onset
           #:weather-alert-expires
           #:weather-alert-effective
           #:parse-weather-response
           #:parse-weather-day
           #:parse-weather-hour
           #:parse-current-conditions
           #:parse-weather-alert))
(in-package #:visualcrossing/data)

(defstruct weather-multi-response
  (query-cost nil :type (integer 0))
  (locations nil :type list))

(defstruct weather-response
  (query-cost nil :type (integer 0))
  (latitude nil :type (or double-float null))
  (longitude nil :type (or double-float null))
  resolved-address
  address
  (timezone nil :type string)
  ;; tzoffset is an integer in most cases,
  ;; but there're exceptional ones (ex. "Kathmandu, Nepal")
  (tzoffset 0 :type (or integer single-float))
  elevation
  (days nil :type list)
  current-conditions
  (alerts nil :type list))

(defstruct (weather-base (:constructor nil)
                         (:predicate nil)
                         (:copier nil)
                         (:conc-name weather-))
  (datetime nil :type string)
  (datetime-epoch nil :type (integer 0))
  (temp nil :type (or double-float null))
  (feelslike nil :type (or double-float null))
  (dew nil :type (or double-float null))
  (humidity nil :type (or double-float null))
  (precip nil :type (or double-float null))
  (precipprob nil :type (or double-float null))
  (preciptype nil :type list)
  (snow nil :type (or double-float null))
  (snowdepth nil :type (or double-float null))
  (windgust nil :type (or double-float null))
  (windspeed nil :type (or double-float null))
  (winddir nil :type (or double-float null))
  (pressure nil :type (or double-float null))
  (cloudcover nil :type (or double-float null))
  (visibility nil :type (or double-float null))
  (solarradiation nil :type (or double-float null))
  (solarenergy nil :type (or double-float null))
  (uvindex nil :type (or double-float null))
  (conditions nil :type (or string null))
  (description nil :type (or string null))
  (icon nil :type (or string null))
  (stations nil :type t)
  (source nil :type (or string null)))

(defstruct (weather-day (:include weather-base))
  (tempmax nil :type (or double-float null))
  (tempmin nil :type (or double-float null))
  (feelslikemax nil :type (or double-float null))
  (feelslikemin nil :type (or double-float null))
  (windspeedmax nil :type (or double-float null))
  (windspeedmean nil :type (or double-float null))
  (windspeedmin nil :type (or double-float null))
  precipcover
  (sunrise nil :type (or string null))
  (sunrise-epoch nil :type (or (integer 0) null))
  (sunset nil :type (or string null))
  (sunset-epoch nil :type (or (integer 0) null))
  (moonphase nil :type (or single-float null))
  (hours nil :type list))

(defstruct (weather-hour (:include weather-base)))

(defstruct (current-conditions (:include weather-base)))

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

(defun to-float (value)
  (etypecase value
    (null nil)
    (integer (coerce value 'double-float))
    (double-float value)))

(defun prefer-integer (value)
  (etypecase value
    (null nil)
    (integer value)
    (double-float
     (multiple-value-bind (quotient remainder)
         (floor value)
       (if (zerop remainder)
           quotient
           (coerce value 'single-float))))))

(defun make-weather-response-from-hash (data)
  (make-weather-response
   :query-cost (get-json-value "queryCost" data)
   :latitude (to-float (get-json-value "latitude" data))
   :longitude (to-float (get-json-value "longitude" data))
   :resolved-address (get-json-value "resolvedAddress" data)
   :address (get-json-value "address" data)
   :timezone (get-json-value "timezone" data)
   :tzoffset (prefer-integer (or (get-json-value "tzoffset" data) 0))
   :elevation (get-json-value "elevation" data)
   :days (let ((days (get-json-value "days" data)))
           (when days (mapcar #'parse-weather-day days)))
   :current-conditions (let ((days (get-json-value "days" data)))
                         (when (and days (> (length days) 0))
                           (let ((cc (get-json-value "currentConditions" (first days))))
                             (when cc (parse-current-conditions cc)))))
   :alerts (let ((alerts (get-json-value "alerts" data)))
             (when alerts (mapcar #'parse-weather-alert alerts)))))

(defun parse-weather-response (json-data)
  "Parse JSON response into weather-response structure."
  (check-type json-data string)
  (let ((data (yason:parse json-data)))
    (make-weather-response-from-hash data)))

(defun parse-weather-multi-response (json-data)
  (check-type json-data string)
  (let ((data (yason:parse json-data)))
    (make-weather-multi-response
     :query-cost (get-json-value "queryCost" data)
     :locations (mapcar #'make-weather-response-from-hash
                        (gethash "locations" data)))))

(defun parse-weather-day (day-data)
  "Parse JSON day data into weather-day structure."
  (make-weather-day
   :datetime (get-json-value "datetime" day-data)
   :datetime-epoch (get-json-value "datetimeEpoch" day-data)
   :temp (to-float (get-json-value "temp" day-data))
   :tempmax (to-float (get-json-value "tempmax" day-data))
   :tempmin (to-float (get-json-value "tempmin" day-data))
   :feelslike (to-float (get-json-value "feelslike" day-data))
   :feelslikemax (to-float (get-json-value "feelslikemax" day-data))
   :feelslikemin (to-float (get-json-value "feelslikemin" day-data))
   :dew (to-float (get-json-value "dew" day-data))
   :humidity (to-float (get-json-value "humidity" day-data))
   :precip (to-float (get-json-value "precip" day-data))
   :precipprob (to-float (get-json-value "precipprob" day-data))
   :precipcover (to-float (get-json-value "precipcover" day-data))
   :preciptype (get-json-value "preciptype" day-data)
   :snow (to-float (get-json-value "snow" day-data))
   :snowdepth (to-float (get-json-value "snowdepth" day-data))
   :windgust (to-float (get-json-value "windgust" day-data))
   :windspeed (to-float (get-json-value "windspeed" day-data))
   :windspeedmax (to-float (get-json-value "windspeedmax" day-data))
   :windspeedmean (to-float (get-json-value "windspeedmean" day-data))
   :windspeedmin (to-float (get-json-value "windspeedmin" day-data))
   :winddir (to-float (get-json-value "winddir" day-data))
   :pressure (to-float (get-json-value "pressure" day-data))
   :cloudcover (to-float (get-json-value "cloudcover" day-data))
   :visibility (to-float (get-json-value "visibility" day-data))
   :solarradiation (to-float (get-json-value "solarradiation" day-data))
   :solarenergy (to-float (get-json-value "solarenergy" day-data))
   :uvindex (to-float (get-json-value "uvindex" day-data))
   :conditions (get-json-value "conditions" day-data)
   :description (get-json-value "description" day-data)
   :icon (get-json-value "icon" day-data)
   :sunrise (get-json-value "sunrise" day-data)
   :sunrise-epoch (get-json-value "sunriseEpoch" day-data)
   :sunset (get-json-value "sunset" day-data)
   :sunset-epoch (get-json-value "sunsetEpoch" day-data)
   :moonphase (coerce (to-float (get-json-value "moonphase" day-data)) 'single-float)
   :hours (let ((hours (get-json-value "hours" day-data)))
            (when hours (mapcar #'parse-weather-hour hours)))
   :stations (get-json-value "stations" day-data)
   :source (get-json-value "source" day-data)))

(defun parse-weather-hour (hour-data)
  "Parse JSON hour data into weather-hour structure."
  (make-weather-hour
   :datetime (get-json-value "datetime" hour-data)
   :datetime-epoch (get-json-value "datetimeEpoch" hour-data)
   :temp (to-float (get-json-value "temp" hour-data))
   :feelslike (to-float (get-json-value "feelslike" hour-data))
   :dew (to-float (get-json-value "dew" hour-data))
   :humidity (to-float (get-json-value "humidity" hour-data))
   :precip (to-float (get-json-value "precip" hour-data))
   :precipprob (to-float (get-json-value "precipprob" hour-data))
   :preciptype (get-json-value "preciptype" hour-data)
   :snow (to-float (get-json-value "snow" hour-data))
   :snowdepth (to-float (get-json-value "snowdepth" hour-data))
   :windgust (to-float (get-json-value "windgust" hour-data))
   :windspeed (to-float (get-json-value "windspeed" hour-data))
   :winddir (to-float (get-json-value "winddir" hour-data))
   :pressure (to-float (get-json-value "pressure" hour-data))
   :cloudcover (to-float (get-json-value "cloudcover" hour-data))
   :visibility (to-float (get-json-value "visibility" hour-data))
   :solarradiation (to-float (get-json-value "solarradiation" hour-data))
   :solarenergy (to-float (get-json-value "solarenergy" hour-data))
   :uvindex (to-float (get-json-value "uvindex" hour-data))
   :conditions (get-json-value "conditions" hour-data)
   :description (get-json-value "description" hour-data)
   :icon (get-json-value "icon" hour-data)
   :stations (get-json-value "stations" hour-data)
   :source (get-json-value "source" hour-data)))

(defun parse-current-conditions (cc-data)
  "Parse JSON current conditions data into current-conditions structure."
  (make-current-conditions
   :datetime (get-json-value "datetime" cc-data)
   :datetime-epoch (get-json-value "datetimeEpoch" cc-data)
   :temp (to-float (get-json-value "temp" cc-data))
   :feelslike (to-float (get-json-value "feelslike" cc-data))
   :dew (to-float (get-json-value "dew" cc-data))
   :humidity (to-float (get-json-value "humidity" cc-data))
   :precip (to-float (get-json-value "precip" cc-data))
   :precipprob (to-float (get-json-value "precipprob" cc-data))
   :preciptype (get-json-value "preciptype" cc-data)
   :snow (to-float (get-json-value "snow" cc-data))
   :snowdepth (to-float (get-json-value "snowdepth" cc-data))
   :windgust (to-float (get-json-value "windgust" cc-data))
   :windspeed (to-float (get-json-value "windspeed" cc-data))
   :winddir (to-float (get-json-value "winddir" cc-data))
   :pressure (to-float (get-json-value "pressure" cc-data))
   :cloudcover (to-float (get-json-value "cloudcover" cc-data))
   :visibility (to-float (get-json-value "visibility" cc-data))
   :solarradiation (to-float (get-json-value "solarradiation" cc-data))
   :solarenergy (to-float (get-json-value "solarenergy" cc-data))
   :uvindex (to-float (get-json-value "uvindex" cc-data))
   :conditions (get-json-value "conditions" cc-data)
   :description (get-json-value "description" cc-data)
   :icon (get-json-value "icon" cc-data)
   :stations (get-json-value "stations" cc-data)
   :source (get-json-value "source" cc-data)))

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
