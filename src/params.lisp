(defpackage #:visualcrossing/params
  (:use #:cl)
  (:local-nicknames
   (#:errors #:visualcrossing/errors))
  (:export #:validate-location
           #:validate-date
           #:validate-unit-group
           #:validate-icon-set
           #:validate-lang
           #:validate-content-type
           #:build-query-params
           #:format-date
           #:format-locations))
(in-package #:visualcrossing/params)

(defun validate-location (location)
  "Validate location parameter."
  (unless (or (stringp location) (listp location))
    (error 'errors:validation-error
           :message "Location must be a string or list of strings"
           :parameter :location
           :value location))
  (when (stringp location)
    (when (str:emptyp (str:trim location))
      (error 'errors:validation-error
             :message "Location cannot be empty"
             :parameter :location
             :value location)))
  (when (listp location)
    (unless (every #'stringp location)
      (error 'errors:validation-error
             :message "All locations must be strings"
             :parameter :location
             :value location))
    (when (some (lambda (loc) (str:emptyp (str:trim loc))) location)
      (error 'errors:validation-error
             :message "Location cannot contain empty strings"
             :parameter :location
             :value location)))
  location)

(defun validate-date (date)
  "Validate date parameter (string in YYYY-MM-DD format or local-time timestamp)."
  (when date
    (cond
      ((stringp date)
       (unless (or (string= date "today")
                   (string= date "yesterday")
                   (string= date "tomorrow")
                   (ppcre:scan "\\d{4}-\\d{2}-\\d{2}" date))
         (error 'errors:validation-error
                :message "Date must be in YYYY-MM-DD format or 'today', 'yesterday', 'tomorrow'"
                :parameter :date
                :value date)))
      ((not (stringp date))
       (error 'errors:validation-error
              :message "Date must be a string"
              :parameter :date
              :value date))))
  date)

(defun validate-unit-group (unit-group)
  "Validate unit-group parameter."
  (unless (member unit-group '(:us :uk :metric :base))
    (error 'errors:validation-error
           :message "Unit group must be one of :us, :uk, :metric, :base"
           :parameter :unit-group
           :value unit-group))
  unit-group)

(defun validate-icon-set (icon-set)
  "Validate icon-set parameter."
  (unless (member icon-set '(:icons1 :icons2))
    (error 'errors:validation-error
           :message "Icon set must be one of :icons1, :icons2"
           :parameter :icon-set
           :value icon-set))
  icon-set)

(defun validate-lang (lang)
  "Validate language parameter."
  (unless (member lang '(:en :es :fr :de :it :pt :ru :zh :ja :ar :hi :ko :nl :sv :da :no :fi :pl :cs :hu :ro :tr :bg :hr :sk :sl :et :lv :lt :uk :be :ka :hy :az :kk :ky :uz :tg :mn :my :km :lo :vi :th :id :ms :tl :sw :am :om :so :ha :ig :yo :zu :xh :af :sq :eu :ca :gl :mt :is :ga :cy :br :co :eo :ia :ie :io :jbo :kw :la :lfn :li :lmo :nap :nn :oc :rm :sc :scn :vec :vo :wa))
    (error 'errors:validation-error
           :message "Invalid language code"
           :parameter :lang
           :value lang))
  lang)

(defun validate-content-type (content-type)
  "Validate content-type parameter."
  (unless (member content-type '(:json :csv))
    (error 'errors:validation-error
           :message "Content type must be one of :json, :csv"
           :parameter :content-type
           :value content-type))
  content-type)

(defun format-date (date)
  "Format date for API request."
  (cond
    ((null date) nil)
    ((stringp date) date)
    (t (error 'errors:validation-error
              :message "Date must be a string"
              :parameter :date
              :value date))))

(defun format-locations (locations)
  "Format locations for API request."
  (cond
    ((stringp locations) locations)
    ((listp locations) (str:join "|" locations))
    (t (error 'errors:validation-error
              :message "Locations must be a string or list of strings"
              :parameter :locations
              :value locations))))

(defun build-query-params (&key location locations start-date end-date unit-group
                                include-hours include-current include-alerts
                                elements timezone icon-set lang content-type
                                days period api-key)
  "Build query parameters for API request."
  (let ((params '()))

    ;; Location handling
    (when location
      (validate-location location)
      (push (cons "location" (format-locations location)) params))

    (when locations
      (validate-location locations)
      (push (cons "locations" (format-locations locations)) params))

    ;; Date parameters
    (when start-date
      (validate-date start-date)
      (push (cons "startDateTime" (format-date start-date)) params))

    (when end-date
      (validate-date end-date)
      (push (cons "endDateTime" (format-date end-date)) params))

    ;; Unit group
    (when unit-group
      (validate-unit-group unit-group)
      (push (cons "unitGroup" (string-downcase (symbol-name unit-group))) params))

    ;; Boolean parameters
    (when include-hours
      (push (cons "include" "hours") params))

    (when include-current
      (push (cons "include" "current") params))

    (when include-alerts
      (push (cons "include" "alerts") params))

    ;; Elements
    (when elements
      (push (cons "elements" (if (listp elements)
                                (str:join "," (mapcar #'string-downcase
                                                     (mapcar #'symbol-name elements)))
                                (string-downcase (symbol-name elements)))) params))

    ;; Timezone
    (when timezone
      (push (cons "timezone" (string timezone)) params))

    ;; Icon set
    (when icon-set
      (validate-icon-set icon-set)
      (push (cons "iconSet" (string-downcase (symbol-name icon-set))) params))

    ;; Language
    (when lang
      (validate-lang lang)
      (push (cons "lang" (string-downcase (symbol-name lang))) params))

    ;; Content type
    (when content-type
      (validate-content-type content-type)
      (push (cons "contentType" (string-downcase (symbol-name content-type))) params))

    ;; Days (for forecast)
    (when days
      (unless (and (integerp days) (> days 0) (<= days 15))
        (error 'errors:validation-error
               :message "Days must be an integer between 1 and 15"
               :parameter :days
               :value days))
      (push (cons "days" (write-to-string days)) params))

    ;; Period
    (when period
      (push (cons "period" (string-downcase (symbol-name period))) params))

    ;; API key
    (when api-key
      (push (cons "key" api-key) params))

    (nreverse params)))
