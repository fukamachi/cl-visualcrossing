(defpackage #:visualcrossing/client
  (:use #:cl)
  (:local-nicknames
   (#:errors #:visualcrossing/errors))
  (:export #:*api-key*
           #:*base-url*
           #:*timeout*
           #:*retry-attempts*
           #:*retry-delay*
           #:get-api-key
           #:build-url
           #:make-request
           #:make-timeline-request
           #:make-multi-timeline-request))
(in-package #:visualcrossing/client)

(defparameter *api-key* nil
  "Visual Crossing Weather API key")

(defparameter *base-url* "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline"
  "Base URL for Visual Crossing Timeline API")

(defparameter *timeout* 5
  "Request timeout in seconds")

(defparameter *retry-attempts* 3
  "Number of retry attempts for failed requests")

(defparameter *retry-delay* 1
  "Initial delay between retries in seconds")

(defun get-api-key (&optional provided-key)
  "Get API key from parameter, dynamic variable, or environment variable."
  (or provided-key
      *api-key*
      (uiop:getenvp "VISUAL_CROSSING_WEATHER_API_KEY")
      (error 'errors:authentication-error
             :message "No API key provided. Set VISUAL_CROSSING_WEATHER_API_KEY environment variable or *api-key* dynamic variable")))

(defun build-url (location &optional start-date end-date)
  "Build API URL for timeline request."
  (let ((url (format nil "~A/~A" *base-url* location)))
    (when start-date
      (setf url (format nil "~A/~A" url start-date)))
    (when end-date
      (setf url (format nil "~A/~A" url end-date)))
    url))

(defun params-to-query-string (params)
  "Convert parameter alist to query string."
  (str:join "&"
    (mapcar (lambda (param)
              (format nil "~A=~A"
                     (quri:url-encode (car param))
                     (quri:url-encode (cdr param))))
            params)))

(defun make-request-with-retry (url &key (method :get) headers content (retry-count 0))
  "Make HTTP request with retry logic."
  (block retry
    (handler-bind (((or dex:http-request-internal-server-error
                        dex:http-request-bad-gateway
                        dex:http-request-service-unavailable
                        dex:http-request-gateway-timeout)
                     (lambda (e)
                       (declare (ignore e))
                       (when (< retry-count *retry-attempts*)
                         (sleep (* *retry-delay* (expt 2 retry-count))) ; Exponential backoff
                         (return-from retry
                           (make-request-with-retry url
                                                    :method method
                                                    :headers headers
                                                    :content content
                                                    :retry-count (1+ retry-count))))))
                   (dex:http-request-failed #'dex:ignore-and-continue))
      (dexador:request url
                       :method method
                       :headers headers
                       :content content
                       :read-timeout *timeout*
                       :connect-timeout *timeout*
                       :force-string t))))

(defun make-request (url params &key (method :get) content)
  "Make HTTP request to Visual Crossing API."
  (let* ((query-string (when params (params-to-query-string params)))
         (full-url (if query-string
                      (format nil "~A?~A" url query-string)
                      url))
         (headers '(("User-Agent" . "visualcrossing/0.1.0"))))

    (multiple-value-bind (body status-code response-headers)
        (make-request-with-retry full-url
                               :method method
                               :headers headers
                               :content content)

      ;; Handle HTTP status codes
      (case status-code
        (200 body)
        (401 (error 'errors:authentication-error
                    :message "Invalid API key"
                    :status-code status-code
                    :response-body body))
        (429 (error 'errors:rate-limit-error
                    :message "Rate limit exceeded"
                    :status-code status-code
                    :response-body body
                    :retry-after (gethash "retry-after" response-headers)))
        (400 (error 'errors:invalid-location-error
                    :message "Invalid location or parameters"
                    :status-code status-code
                    :response-body body))
        (404 (error 'errors:invalid-location-error
                    :message "Location not found"
                    :status-code status-code
                    :response-body body))
        (otherwise (error 'errors:api-error
                          :message (format nil "API request failed with status ~D" status-code)
                          :status-code status-code
                          :response-body body))))))

(defun make-timeline-request (location &key start-date end-date api-key
                                           unit-group include-hours include-current
                                           include-alerts elements timezone
                                           icon-set lang content-type period)
  "Make timeline API request."
  (let* ((api-key (get-api-key api-key))
         (url (build-url location start-date end-date))
         (params (visualcrossing/params:build-query-params
                  :api-key api-key
                  :unit-group unit-group
                  :include-hours include-hours
                  :include-current include-current
                  :include-alerts include-alerts
                  :elements elements
                  :timezone timezone
                  :icon-set icon-set
                  :lang lang
                  :content-type content-type
                  :period period)))

    (make-request url params)))

(defun make-multi-timeline-request (locations &key start-date end-date api-key
                                                   unit-group include-hours
                                                   elements timezone icon-set
                                                   lang content-type)
  "Make multi-location timeline API request."
  (let* ((api-key (get-api-key api-key))
         (url (format nil "~A/timelinemulti"
                     (str:replace-all "/timeline" "" *base-url*)))
         (params (visualcrossing/params:build-query-params
                  :locations locations
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

    (make-request url params)))
