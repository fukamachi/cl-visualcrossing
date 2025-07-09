(defpackage #:visualcrossing/tests/utils
  (:use #:cl)
  (:import-from #:visualcrossing
                #:*api-key*)
  (:export #:with-test-api-key))
(in-package #:visualcrossing/tests/utils)

(defmacro with-test-api-key ((&optional api-key) &body body)
  (let ((original-key (gensym "ORIGINAL-KEY")))
    `(let ((*api-key* ,(or api-key '(uiop:getenv "TEST_VISUAL_CROSSING_WEATHER_API_KEY"))))
       (let ((,original-key (uiop:getenv "VISUAL_CROSSING_WEATHER_API_KEY")))
         (unwind-protect
              (progn
                (setf (uiop:getenv "VISUAL_CROSSING_WEATHER_API_KEY") (or *api-key* ""))
                ,@body)
           (when ,original-key
             (setf (uiop:getenv "VISUAL_CROSSING_WEATHER_API_KEY") ,original-key)))))))
