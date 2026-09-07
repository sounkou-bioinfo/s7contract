## ---- calendar-setup
library(S7)
library(s7contract)

## ---- calendar-generator
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

## ---- calendar-interface
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

## ---- calendar-law
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
calendar_result@coverage

## ---- calendar-broken
ExclusiveEnd <- new_class("ExclusiveEnd", parent = DayInterval)
method(interval_contains, ExclusiveEnd) <- function(x, day) {
  day >= x@start & day < x@end
}
implements(ExclusiveEnd, CalendarInterval)
calendar_failure <- check_law(interval_law(ExclusiveEnd), tests = 100L, seed = 1L)
calendar_failure
calendar_failure@counterexample@minimal$input[c("start", "end", "day")]

## ---- calendar-replay
calendar_replayed <- do.call(check_law,
  c(list(law = calendar_failure@law), calendar_failure@parameters))
identical(calendar_replayed@counterexample@minimal, calendar_failure@counterexample@minimal)
