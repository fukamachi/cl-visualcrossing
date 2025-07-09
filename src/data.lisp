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
           #:weather-cloudover
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

(defstruct (weather-base (:constructor nil)
                         (:predicate nil)
                         (:copier nil)
                         (:conc-name weather-))
  datetime
  datetime-epoch
  temp
  feelslike
  dew
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
  solarradiation
  solarenergy
  uvindex
  conditions
  description
  icon
  stations
  source)

(defstruct (weather-day (:include weather-base))
  tempmax
  tempmin
  feelslikemax
  feelslikemin
  windspeedmax
  windspeedmean
  windspeedmin
  precipcover
  sunrise
  sunrise-epoch
  sunset
  sunset-epoch
  moonphase
  hours)

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
   :datetime-epoch (get-json-value "datetimeEpoch" day-data)
   :temp (get-json-value "temp" day-data)
   :tempmax (get-json-value "tempmax" day-data)
   :tempmin (get-json-value "tempmin" day-data)
   :feelslike (get-json-value "feelslike" day-data)
   :feelslikemax (get-json-value "feelslikemax" day-data)
   :feelslikemin (get-json-value "feelslikemin" day-data)
   :dew (get-json-value "dew" day-data)
   :humidity (get-json-value "humidity" day-data)
   :precip (get-json-value "precip" day-data)
   :precipprob (get-json-value "precipprob" day-data)
   :precipcover (get-json-value "precipcover" day-data)
   :preciptype (get-json-value "preciptype" day-data)
   :snow (get-json-value "snow" day-data)
   :snowdepth (get-json-value "snowdepth" day-data)
   :windgust (get-json-value "windgust" day-data)
   :windspeed (get-json-value "windspeed" day-data)
   :windspeedmax (get-json-value "windspeedmax" day-data)
   :windspeedmean (get-json-value "windspeedmean" day-data)
   :windspeedmin (get-json-value "windspeedmin" day-data)
   :winddir (get-json-value "winddir" day-data)
   :pressure (get-json-value "pressure" day-data)
   :cloudcover (get-json-value "cloudcover" day-data)
   :visibility (get-json-value "visibility" day-data)
   :solarradiation (get-json-value "solarradiation" day-data)
   :solarenergy (get-json-value "solarenergy" day-data)
   :uvindex (get-json-value "uvindex" day-data)
   :conditions (get-json-value "conditions" day-data)
   :description (get-json-value "description" day-data)
   :icon (get-json-value "icon" day-data)
   :sunrise (get-json-value "sunrise" day-data)
   :sunrise-epoch (get-json-value "sunriseEpoch" day-data)
   :sunset (get-json-value "sunset" day-data)
   :sunset-epoch (get-json-value "sunsetEpoch" day-data)
   :moonphase (get-json-value "moonphase" day-data)
   :hours (let ((hours (get-json-value "hours" day-data)))
            (when hours (mapcar #'parse-weather-hour hours)))
   :stations (get-json-value "stations" day-data)
   :source (get-json-value "source" day-data)))

(defun parse-weather-hour (hour-data)
  "Parse JSON hour data into weather-hour structure."
  (make-weather-hour
   :datetime (get-json-value "datetime" hour-data)
   :datetime-epoch (get-json-value "datetimeEpoch" hour-data)
   :temp (get-json-value "temp" hour-data)
   :feelslike (get-json-value "feelslike" hour-data)
   :dew (get-json-value "dew" hour-data)
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
   :solarradiation (get-json-value "solarradiation" hour-data)
   :solarenergy (get-json-value "solarenergy" hour-data)
   :uvindex (get-json-value "uvindex" hour-data)
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
   :temp (get-json-value "temp" cc-data)
   :feelslike (get-json-value "feelslike" cc-data)
   :dew (get-json-value "dew" cc-data)
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
   :solarradiation (get-json-value "solarradiation" cc-data)
   :solarenergy (get-json-value "solarenergy" cc-data)
   :uvindex (get-json-value "uvindex" cc-data)
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
