#' @title calc_duration
#' @description Returns the duration of time between two provided date objects.
#' @param start Required. Date or Timestamp. The start date.
#' @param end Required. Date or Timestamp. The end date.
#' @param units Optional. Character. Units of the returned duration
#' (i.e. 'seconds', 'days', 'years'). Defaults to 'years'.
#' @export
calc_duration <- function(start = NULL, end = NULL, units = 'years') {
  if (is.null(start) | is.null(end))
    stop('Missing necessary parameter. Look at \'start\' and \'end\'.')
  durations <- as.numeric(lubridate::as.duration(lubridate::interval(start, end)), units)
  replace(durations, durations < 0, 0)
}
