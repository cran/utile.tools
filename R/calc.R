#' @title Calculate Duration
#' @description Returns the duration of time between two provided date objects.
#' Supports vectorized data (i.e. dplyr::mutate()).
#' @param start Required. Date or POSIXt object. The start date/timestamp.
#' @param end Required. Date or POSIXt object. The end date/timestamp.
#' @param units Optional. Character. Units of the returned duration
#' (i.e. 'seconds', 'days', 'years'). Defaults to 'years'.
#' @examples
#' # Timestamps
#' calc_duration(
#'    start = as.POSIXct('01/01/1999 10:00', format = '%m/%d/%Y %H:%M'),
#'    end = as.POSIXct('01/01/2001 00:00', format = '%m/%d/%Y %H:%M'),
#'    units = 'days'
#' )
#'
#' # Dates
#' calc_duration(
#'    start = as.Date('01/01/1999', format = '%m/%d/%Y'),
#'    end = as.Date('01/01/2001', format = '%m/%d/%Y'),
#'    units = 'years'
#' )
#' @export
calc_duration <- function(start = NA, end = NA, units = 'years') {

  # Hard Stop
  if (
    !(
      all(lubridate::is.Date(start), na.rm = TRUE) |
      all(lubridate::is.POSIXt(start), na.rm = TRUE) |
      all(is.na(start))
    ) &
    !(
      all(lubridate::is.Date(end), na.rm = TRUE) |
      all(lubridate::is.POSIXt(end), na.rm = TRUE) |
      all(is.na(end))
    )
  ) stop('Missing Date or POSIXt object. Check: [\'start\', \'end\']')
  if (length(start) != length(end)) stop('Provided data are not of the same length. Check [\'start\', \'end\']')

  # Ignore timestamp if one variable is a Date object
  if (all(lubridate::is.Date(end), na.rm = TRUE) & all(lubridate::is.POSIXt(start), na.rm = TRUE))
    start <- as.Date(start)
  if (all(lubridate::is.Date(start), na.rm = TRUE) & all(lubridate::is.POSIXt(end), na.rm = TRUE))
    end <- as.Date(end)

  # Calculate and return data
  as.numeric(lubridate::as.duration(lubridate::interval(start, end)), units)
}
