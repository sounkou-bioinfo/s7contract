# Laws for Calendar Intervals

``` r

library(S7)
library(s7contract)
```

## Whole days from integer offsets

Calendar intervals promise to include both endpoints. Testing that
promise needs ordered bounds, queries at those bounds, and ordinary
dates between or outside them.

This recipe maps integer offsets onto a fixed `Date` origin. Size zero
draws the origin; size `s` draws uniformly within `origin ± s` days,
clipped to the bounds. Shrinking moves offsets toward zero. Its `Date`
prototype preserves the class when composed into vectors, including
empty and nested vectors.

``` r

whole_day <- function(x) {
  if (!inherits(x, "Date") || length(x) != 1L) return(FALSE)
  day <- as.double(x)
  is.finite(day) && day == trunc(day) && abs(day) <= .Machine$integer.max
}

date_generator <- function(min, max, origin = min) {
  bounds <- list(min = min, max = max, origin = origin)
  for (name in names(bounds)) {
    if (!whole_day(bounds[[name]])) {
      stop(name, " must be one finite whole Date within the integer day range")
    }
  }
  if (min > max) stop("date bounds must be ordered")
  if (origin < min || origin > max) stop("origin must lie within the bounds")
  offsets <- gen_integer(as.double(min) - as.double(origin),
                         as.double(max) - as.double(origin))
  gen_map(offsets, function(offset) .Date(as.double(origin) + offset),
          prototype = .Date(double()))
}
```

Bounds and origins must be finite whole dates. This recipe restricts
their day counts and the offsets from the origin to R’s non-missing
integer range. It generates calendar days without a time zone or a
dependency on today’s date. [R’s Date
documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Dates.html)
warns that fractional day values can be hidden by printing, and that
integer and double storage can represent the same date. The law compares
dates with `==`, `<=`, and `>=` rather than requiring identical storage.

## An inclusive interval protocol

The class validator requires ordered whole-day bounds. The interface
requires a membership operation accepting dates and returning logical
values.

``` r

interval_contains <- new_generic("interval_contains", "x",
                                 function(x, day) S7_dispatch())
CalendarInterval <- new_interface("CalendarInterval", generics = list(
  contains = interface_requirement(interval_contains,
    args = list(day = class_Date), returns = class_logical)
))
DayInterval <- new_class("DayInterval",
  properties = list(start = class_Date, end = class_Date),
  validator = function(self) {
    if (!whole_day(self@start) || !whole_day(self@end)) {
      return("bounds must be finite whole days within the integer day range")
    }
    if (self@start > self@end) "start must not follow end"
  }
)
method(interval_contains, DayInterval) <- function(x, day) {
  day >= x@start & day <= x@end
}
```

Dependent generators rebuild valid intervals as their start or end
shrinks. Query generation explicitly includes both endpoints and leap
day. Coverage records those cases and intervals crossing New Year.

``` r

first_day <- as.Date("2023-12-28")
last_day <- as.Date("2024-03-02")
new_year <- as.Date("2024-01-01")
leap_day <- as.Date("2024-02-29")

interval_law <- function(make) {
  cases <- gen_bind(date_generator(first_day, last_day, new_year), function(start) {
    gen_bind(date_generator(start, last_day), function(end) {
      gen_product(
        x = gen_constant(make(start, end)),
        start = gen_constant(start), end = gen_constant(end),
        day = gen_choice(gen_constant(start), gen_constant(end),
                         gen_constant(leap_day),
                         date_generator(first_day, last_day, new_year))
      )
    })
  })
  new_law("calendar intervals include both bounds", list(input = cases),
    function(input) with(CalendarInterval, {
      contains(input$x, input$day) ==
        (input$day >= input$start && input$day <= input$end)
    }),
    classify = function(input) c(
      if (input$day == input$start) "lower_bound",
      if (input$day == input$end) "upper_bound",
      if (input$day == leap_day) "leap_day",
      if (input$start < new_year && input$end >= new_year) "year_crossing"
    ),
    min_coverage = c(lower_bound = 0.15, upper_bound = 0.15,
                     leap_day = 0.15, year_crossing = 0.02)
  )
}
calendar_result <- check_law(interval_law(DayInterval), tests = 100L, seed = 1L)
calendar_result@status
#> [1] "passed"
calendar_result@coverage
#>           label count proportion minimum  met
#> 1   lower_bound    22       0.22    0.15 TRUE
#> 2   upper_bound    27       0.27    0.15 TRUE
#> 3      leap_day    29       0.29    0.15 TRUE
#> 4 year_crossing     9       0.09    0.02 TRUE
```

## An end date accidentally excluded

Changing `<=` to `<` keeps the required method available but violates
the law. The reduced failure is a one-day interval queried on that same
day.

``` r

ExclusiveEnd <- new_class("ExclusiveEnd", parent = DayInterval)
method(interval_contains, ExclusiveEnd) <- function(x, day) {
  day >= x@start & day < x@end
}
implements(ExclusiveEnd, CalendarInterval)
#> [1] TRUE
calendar_failure <- check_law(interval_law(ExclusiveEnd), tests = 100L, seed = 1L)
calendar_failure
#> Law 'calendar intervals include both bounds' was falsified after 2 attempts and 2 shrinks (seed 1).
#> The law returned FALSE.
#> Shrinking stopped: no child of this counterexample preserves the failure.
#> Smallest counterexample found:
#> List of 1
#>  $ input:List of 4
#>   ..$ x    : <ExclusiveEnd>
#>   .. ..@ start: Date[1:1], format: "2024-01-01"
#>   .. ..@ end  : Date[1:1], format: "2024-01-01"
#>   ..$ start: Date[1:1], format: "2024-01-01"
#>   ..$ end  : Date[1:1], format: "2024-01-01"
#>   ..$ day  : Date[1:1], format: "2024-01-01"
#> Case coverage (2 accepted cases; partial run):
#>   "year_crossing": 0/2 (0%; minimum 2% unmet)
#>   "lower_bound": 1/2 (50%; minimum 15%)
#>   "upper_bound": 1/2 (50%; minimum 15%)
#>   "leap_day": 1/2 (50%; minimum 15%)
calendar_failure@counterexample@minimal$input[c("start", "end", "day")]
#> $start
#> [1] "2024-01-01"
#> 
#> $end
#> [1] "2024-01-01"
#> 
#> $day
#> [1] "2024-01-01"
```

The same installed script supplies this article and the regression
tests. Replay retains the bounds and query along with the run
parameters:

``` r

calendar_replayed <- do.call(check_law,
  c(list(law = calendar_failure@law), calendar_failure@parameters))
identical(calendar_replayed@counterexample@minimal, calendar_failure@counterexample@minimal)
#> [1] TRUE
```
