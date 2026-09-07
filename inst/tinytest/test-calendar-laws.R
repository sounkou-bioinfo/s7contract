sys.source(system.file("examples", "calendar-laws.R", package = "s7contract"),
           envir = environment())

expect_identical(calendar_result@status, "passed")
expect_true(all(calendar_result@coverage$met))
expect_true(implements(ExclusiveEnd, CalendarInterval))
expect_identical(calendar_failure@status, "falsified")
expect_identical(calendar_failure@shrink_status, "complete")
expect_identical(calendar_replayed@counterexample@minimal, calendar_failure@counterexample@minimal)
expect_identical(calendar_replayed@shrink_attempts, calendar_failure@shrink_attempts)
input <- calendar_failure@counterexample@minimal$input
expect_true(input$start == new_year && input$end == new_year && input$day == new_year)

# Compare calendar meaning across storage modes; both include leap/year boundaries.
for (days in list(c("2023-12-31", "2024-01-01"),
                  c("2024-02-28", "2024-02-29", "2024-03-01"))) {
  dates <- as.Date(days)
  interval <- DayInterval(start = dates[1L], end = dates[length(dates)])
  expect_true(all(interval_contains(interval, dates)))
  expect_false(interval_contains(interval, dates[1L] - 1L))
  expect_false(interval_contains(interval, dates[length(dates)] + 1L))
  integer_dates <- .Date(as.integer(dates))
  expect_identical(interval_contains(interval, integer_dates), interval_contains(interval, dates))
  draws <- vapply(1:50, function(seed) as.double(gen_example(
    date_generator(dates[1L], dates[length(dates)]), size = 10L, seed = seed)), double(1))
  expect_equal(sort(unique(draws)), as.double(dates))
}
expect_true(gen_example(date_generator(leap_day, leap_day)) == leap_day)
generator <- date_generator(first_day, last_day, new_year)
expect_true(gen_example(generator, size = 0L) == new_year)
for (size in 0:4) {
  dates <- vapply(1:20, function(seed) as.double(gen_example(generator, size, seed)), double(1))
  expect_true(all(abs(dates - as.double(new_year)) <= size))
}

# Every executed shrink retains ordered bounds, Date classes and whole day values.
visits <- 0L
audited <- new_law(calendar_failure@law@name, calendar_failure@law@generators, function(input) {
  stopifnot(all(vapply(input[c("start", "end", "day")], whole_day, logical(1))),
            input$start >= first_day, input$end <= last_day,
            input$start <= input$end, input$x@start == input$start,
            input$x@end == input$end)
  visits <<- visits + 1L
  (calendar_failure@law@holds)(input)
})
result <- do.call(check_law, c(list(law = audited), calendar_failure@parameters))
expect_identical(result@counterexample@minimal, calendar_failure@counterexample@minimal)
expect_identical(visits, result@attempts + result@shrink_attempts)

# Empty and nested date vectors retain their class at every visited node.
expect_identical(gen_example(gen_vector(generator, 0L, 0L)), .Date(double()))
nested <- gen_vector(gen_vector(generator, max = 4L), min = 1L, max = 3L)
law <- new_law("nested dates", list(x = gen_resize(nested, 10L)), function(x) {
  for (dates in x) {
    stopifnot(inherits(dates, "Date"), all(is.finite(dates)),
              all(as.double(dates) == trunc(as.double(dates))),
              all(dates >= first_day & dates <= last_day))
  }
  FALSE
})
result <- check_law(law, tests = 1L, seed = 4L)
expect_identical(result@status, "falsified")
expect_identical(result@counterexample@minimal$x, list(.Date(double())))
expect_identical(result@shrink_status, "complete")

for (bad in list(NULL, .Date(double()), c(first_day, last_day), 1L, "2024-01-01",
                 .Date(NA_real_), .Date(Inf), .Date(NaN), .Date(0.5),
                 .Date(as.double(.Machine$integer.max) + 1))) {
  expect_error(date_generator(bad, last_day))
  expect_error(date_generator(first_day, bad))
  expect_error(date_generator(first_day, last_day, bad))
}
expect_error(date_generator(last_day, first_day), pattern = "ordered")
expect_error(date_generator(first_day, last_day, last_day + 1L), pattern = "within")
expect_error(date_generator(first_day, last_day, first_day - 1L), pattern = "within")
expect_error(date_generator(.Date(-.Machine$integer.max), .Date(.Machine$integer.max)))
expect_error(DayInterval(start = leap_day + 0.5, end = last_day))
expect_error(DayInterval(start = last_day, end = first_day))
