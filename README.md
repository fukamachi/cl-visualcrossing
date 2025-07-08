# cl-visualcrossing

`cl-visualcrossing` is a Common Lisp library for Visual Crossing's Weather API.

It provides functions for each API endpoint with comprehensive error handling and structured data types.

**Note:** This library was written with assistance from Claude (Anthropic's AI assistant).

## Features

* **Timeline Weather API** - Historical, current, and forecast weather data
* **Multiple locations** - Query multiple locations in a single request
* **Comprehensive error handling** - Custom error conditions for different API errors
* **Structured data types** - Proper Common Lisp structures for weather data
* **Flexible parameters** - All API parameters supported as keyword arguments
* **Rate limiting support** - Automatic handling of API rate limits

## Installation

```common-lisp
$ qlot exec sbcl
* (ql:quickload :visualcrossing)
```

## Usage

### Basic Timeline Weather Query

```common-lisp
(vc:timeline-weather "New York, NY"
                     :start-date "2024-01-01"
                     :end-date "2024-01-07"
                     :unit-group :metric
                     :include-hours t)
```

### Current Conditions

```common-lisp
(vc:current-conditions "London, UK"
                       :unit-group :metric)
```

### Forecast Weather

```common-lisp
(vc:forecast-weather "Tokyo, Japan"
                     :days 7
                     :unit-group :metric
                     :include-hours t)
```

### Historical Weather

```common-lisp
(vc:historical-weather "Paris, France" "2023-12-01"
                       :end-date "2023-12-31"
                       :unit-group :metric)
```

### Multiple Locations

```common-lisp
(vc:timeline-weather-multi '("New York, NY" "London, UK" "Tokyo, Japan")
                           :start-date "2024-01-01"
                           :unit-group :metric)
```

## API Functions

* `timeline-weather` - Main Timeline API function for any date range
* `timeline-weather-multi` - Multiple locations in one request
* `current-conditions` - Current weather conditions only
* `forecast-weather` - 15-day weather forecast
* `historical-weather` - Historical weather data

## Configuration

The API key can be set in two ways:

* Environment variable: `VISUAL_CROSSING_WEATHER_API_KEY`
* Dynamic variable: `*api-key*`

```common-lisp
;; Using dynamic variable (recommended)
(setf vc:*api-key* "your-api-key-here")
;; or
(setf visualcrossing:*api-key* "your-api-key-here")

;; Using environment variable
$ export VISUAL_CROSSING_WEATHER_API_KEY="your-api-key-here"
```

## Error Handling

The library provides comprehensive error handling with custom conditions:

```common-lisp
(handler-case
    (vc:timeline-weather "Invalid Location")
  (vc:invalid-location-error (e)
    (format t "Invalid location: ~A~%" (vc:error-message e)))
  (vc:authentication-error (e)
    (format t "Authentication failed: ~A~%" (vc:error-message e)))
  (vc:rate-limit-error (e)
    (format t "Rate limit exceeded: ~A~%" (vc:error-message e))))
```

## Data Structures

The library returns structured data types:

* `weather-response` - Main response structure
* `weather-day` - Daily weather data
* `weather-hour` - Hourly weather data
* `current-conditions` - Current conditions
* `weather-alert` - Weather alerts

## Dependencies

* [Dexador](https://github.com/fukamachi/dexador) - HTTP client
* [yason](https://github.com/phmarek/yason) - JSON parsing
* [cl-str](https://github.com/vindarel/cl-str) - String utilities
* [cl-ppcre](https://github.com/edicl/cl-ppcre) - Regular expressions
* [Rove](https://github.com/fukamachi/rove) - Testing framework

## Development

### Running Tests

```common-lisp
$ qlot exec sbcl
* (ql:quickload :visualcrossing/tests)
* (rove:run :visualcrossing/tests)
```

## See Also

* [Visual Crossing Weather API Documentation](https://www.visualcrossing.com/resources/documentation/weather-api/timeline-weather-api/)
* [Visual Crossing Weather Data Documentation](https://www.visualcrossing.com/resources/documentation/weather-data/weather-data-documentation/)

## LICENSE

See [LICENSE](LICENSE).
