#' @title paste_freq
#' @description Returns a human-readable frequency from count(able) data. Handily
#' has methods for several types of data.
#' @param count Required. Tibble, Column (logical), or Numeric. The numerator.
#' Tibbles and columns are automatically tallied (nrow or sum(na.rm = TRUE)).
#' @param total Required. Tibble, Column, or Numeric. The denominator. Tibbles
#' and columns are automatically tallied (nrow or sum(na.rm = TRUE)).
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param remove.na Optional. Logical. Remove NA from denominator in frequency calculation.
#' Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @examples
#' library(tibble)
#'
#' # Numeric
#' paste_freq(20, 100)
#'
#' # Logical
#' data_logical <- tribble(
#'   ~numerator, ~denomenator,
#'   TRUE, TRUE,
#'   FALSE, TRUE,
#'   FALSE, TRUE
#' )
#' paste_freq(data_logical$numerator, data_logical$denomenator)
#'
#' # Tibble
#' data_tibble <- tibble(column = c(1:100))
#' paste_freq(data_tibble[1:20,], data_tibble)
#' @export
paste_freq <- function(count, total, percent.sign, remove.na, digits) {
  UseMethod('paste_freq')
}

#' @export
paste_freq.default <- function(...) warning('Count data of unknown type. [check: count]')

# Worker function
.paste_freq <- function(count, total, percent.sign, digits) {
  if (total == 0 | is.null(total) | is.null(count)) "--"
  else if (count < 1) paste0('0 (0', if (percent.sign) '%' else NULL, ')')
  else {
    percentage <- round((count / total) * 100, digits = digits)
    paste0(count, ' (', percentage, if (percent.sign) '%' else NULL, ')')
  }
}

#' @export
paste_freq.numeric <- function(count = NULL, total = NULL, percent.sign = TRUE, remove.na = TRUE, digits = 1) {
  .paste_freq(
    count = count,
    total = ifelse(is.numeric(total), total, NULL),
    percent.sign = percent.sign,
    digits = digits
  )
}

#' @export
paste_freq.logical <- function(count = NULL, total = NULL, percent.sign = TRUE, remove.na = TRUE, digits = 1) {
  .paste_freq(
    count = sum(count, na.rm = remove.na),
    total = ifelse(is.logical(total), sum(total, na.rm = remove.na), NULL),
    percent.sign = percent.sign,
    digits = digits
  )
}

#' @export
paste_freq.tbl_df <- function(count = NULL, total = NULL, percent.sign = TRUE, remove.na = TRUE, digits = 1) {
  .paste_freq(
    count = nrow(count),
    total = ifelse(tibble::is_tibble(total), nrow(total), NULL),
    percent.sign = percent.sign,
    digits = digits
  )
}

#' @title paste_median
#' @description Returns a human-readable median with inter-quartile
#' range from numeric data.
#' @param col Required. Vector/Column (numeric). Data to summarize.
#' @param less.than.one Optional. Logical. Indicates a median that rounds to 0 should
#' be printed as <1. Defaults to FALSE (0).
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @examples
#' paste_median(mtcars$mpg)
#' @export
paste_median <- function(col = NULL, less.than.one = FALSE, digits = 1) {
  if (is.null(col) | length(stats::na.omit(col)) < 1) '--'
  else {
    col_median <- round(x = stats::median(col, na.rm = TRUE), digits = digits)
    if (col_median < 1 & less.than.one) col_median <- '<1'
    col_upper <- round(x = stats::quantile(col, probs = c(0.75), na.rm = TRUE))
    col_lower <- round(x = stats::quantile(col, probs = c(0.25), na.rm = TRUE), digits = digits)
    paste0(col_median, ' [', col_lower, '-', col_upper, ']')
  }
}

#' @title paste_mean
#' @description Returns a human-readable mean with standard deviation
#' from numeric data.
#' @param col Required. Vector/Column (numeric). Data to summarize.
#' @param less.than.one Optional. Logical. Indicates a mean that rounds to 0 should
#' be printed as <1. Defaults to FALSE (0).
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @examples
#' paste_mean(mtcars$mpg)
#' @export
paste_mean <- function(col = NULL, less.than.one = FALSE, digits = 1) {
  if (is.null(col) | (length(stats::na.omit(col))) < 1) '--'
  else {
    col_mean <- round(x = mean(col, na.rm = TRUE), digits = digits)
    if (col_mean < 1 & less.than.one) col.mean <- '<1'
    col_sd <- round(x = stats::sd(col, na.rm = TRUE), digits = digits)
    paste0(col_mean, ' \u00B1', col_sd)
  }
}


#' @title paste_efs
#' @description Returns a human-readable event-free-survival from a survfit object
#' and a specified time point.
#' @param fit Required. survival::Surv() object. The time-to-event model of interest.
#' @param time Required. Numeric. Indicates duration of time. Units are whatever was used to
#' create the time-to-event model.
#' @param percent.sign Optional. Logical. Indicates percent sign should be printed
#' for frequencies. Defaults to TRUE.
#' @param digits Optional. Integer. Number of digits to round to. Defaults to 1.
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ 1, data = diabetic)
#' paste_efs(fit, 6)
#' @export
paste_efs <- function(fit = NULL, time = NULL, percent.sign = TRUE, digits = 1) {
  if (is.null(time) | class(fit) != 'survfit')
    stop('Missing necessary parameter. Look at \'time\' and \'fit\'.')
  results <- summary(fit, times = time)
  estimate <- round(results$surv * 100, digits = digits)
  lower <- round(results$lower * 100, digits = digits)
  upper <- round(results$upper * 100, digits = digits)
  paste0(estimate, if (percent.sign) '%' else NULL, ' [', lower, '-', upper, ']')
}
