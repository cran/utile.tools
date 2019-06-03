utils::globalVariables(c('n.risk'))

#' @title tabulate_model
#' @description Returns tabulated data from a model object. Useful for
#' graphing a fit or creating a useable coefficient table.
#' @param fit Required. survival::survfit() or survival::coxph() objects.
#' @param ... Optional. Miscellaneous parameters. Currently, only digits
#' and p.digits are used right now in the coxph method.
#' @return Returns tibble containing:
#'   \item{survfit()}{Graphable survival fit data for use
#'   in graphing packages.}
#'   \item{coxph()}{Organized parameters, levels, HR, intervals, and tests.}
#' @note survfit() methods adapted by the 'survminer' package function
#' surv_summary() [GLP-2].
#' @examples
#' library(dplyr)
#' library(survival)
#'
#' data_cgd <- as_tibble(cgd)
#'
#' # Survfit Object
#' tabulate_model(survfit(Surv(tstart, tstop, status) ~ sex, data = data_cgd))
#'
#' # Coxph Object
#' tabulate_model(coxph(Surv(tstart, tstop, status) ~ age + center + sex, data = data_cgd))
#' @export
tabulate_model <- function(fit, ...) {
  UseMethod('tabulate_model')
}

#' @export
tabulate_model.default <- function(fit, ...) warning(paste0('Object of class \'', class(fit), '\' not supported.'))

#' @export
tabulate_model.survfit <- function (fit, ...) {

  # Data preparation
  data_source <- eval(fit$call$data)
  data_surv <- dplyr::bind_cols(
    dplyr::filter_all(
      .tbl = tibble::as_tibble(unclass(fit)[c("time", "n.risk", "n.event", "n.censor")]),
      function(x) !is.na(x)
    ),
    tibble::as_tibble(unclass(fit)[c('surv', 'upper', 'lower')])
  )

  # Prepare strata data (if there is any)
  if (!is.null(fit$strata)) {
    data_surv$strata <- rep(names(fit$strata), fit$strata)
    variables <- intersect(
      unique(
        purrr::map_chr(
          data_surv$strata,
          function (x) {
            x <- unlist(stringr::str_split(x, '=|,\\s+'))
            x[seq(1, length(x), 2)]
          }
        )
      ),
      colnames(data_source)
    )
    for (variable in variables) {
      strata <- purrr::map_chr(
        data_surv$strata,
        function(x) {
          x <- unlist(stringr::str_split(x, "=|(\\s+)?,\\s+"))
          index <- grep(paste0("^", variable, "$"), x)
          stringr::str_trim(x[index+1])
        }
      )
      var_levels <- levels(data_source[, variable])
      if(!is.null(var_levels)) data_surv[[variable]] <- factor(strata, levels = var_levels)
      else data_surv[[variable]] <- as.factor(strata)
    }
  }

  # Connect to origin
  if("n.risk" %in% colnames(data_surv)) data_surv <- dplyr::arrange(data_surv, dplyr::desc(n.risk))
  if ('strata' %in% names(data_surv)) origin <- dplyr::distinct(.data = data_surv, strata, .keep_all = TRUE)
  else origin <- data_surv[1,]
  origin[intersect(c('time', 'n.censor', 'std.err', "n.event"), colnames(origin))] <- 0
  origin[c('surv', 'upper', 'lower')] <- 1.0
  data_surv <- dplyr::bind_rows(origin, data_surv)
  data_surv
}

#' @export
tabulate_model.coxph <- function(fit, ...) {

  digits <- 1
  p.digits <- 4
  percent.sign <- TRUE
  list2env(list(...), envir = environment())

  coeffs <- summary(fit)$coefficients
  levels <- fit$xlevels
  tests <- stats::anova(fit)

  # Create and return summary table
  purrr::map2_df(
    names(fit$assign), # Parameter names
    fit$assign, # Parameter positions in coefficients
    function(parameter, positions) {

      # Check if parameter categorical
      categorical <- parameter %in% names(levels)

      dplyr::bind_rows(

        # Reference rows for factor w/ >2 levels
        if (categorical & length(positions) > 1)
          dplyr::mutate_all(
            .tbl = tibble::tibble(
              Variable = parameter,
              Level = c(NA, levels[[parameter]][1]),
              `At Risk` = c(fit$n, NA),
              Events = c(
                utile.tools::paste_freq(
                  fit$nevent,
                  fit$n,
                  digits = digits,
                  percent.sign = percent.sign
                ),
                NA
              ),
              `HR 95%CI` = c(NA, '-ref-'),
              p = c(
                format.pval(
                  pv = tests[parameter, 'Pr(>|Chi|)'],
                  digits = digits,
                  eps = 1e-04,
                  nsmall = p.digits,
                  scientific = F
                ),
                NA
              )
            ),
            as.character
          ),

        # Create row(s) for each parameter/level
        purrr::imap_dfr(
          positions,
          function(position, index) {
            row <- coeffs[position,]
            hr <- if (!is.na(row['exp(coef)'])) round(row['exp(coef)'], digits = digits) else NA
            lower <-
              if (!any(is.na(c(row['coef'], row['se(coef)']))))
                round(exp(row['coef'] - (1.95 * row['se(coef)'])), digits = digits)
              else NA
            upper <-
              if (!any(is.na(c(row['coef'], row['se(coef)']))))
                round(exp(row['coef'] + (1.95 * row['se(coef)'])), digits = digits)
              else NA

            dplyr::mutate_all(
              .tbl = tibble::tibble(
                Variable = parameter,
                Level = if (categorical) levels[[parameter]][index + 1] else NA,
                `At Risk` = if (length(positions) == 1) fit$n else NA,
                Events =
                  if (length(positions) == 1)
                    utile.tools::paste_freq(
                      fit$nevent,
                      fit$n,
                      digits = digits,
                      percent.sign = percent.sign
                    )
                  else NA,
                `HR 95%CI` =
                  if (!any(is.na(c(hr, lower, upper))))
                    paste0(hr, ' [', lower, '-', upper, ']')
                  else NA,
                p =
                  if (!is.na(row['Pr(>|z|)']))
                    format.pval(
                      pv = row['Pr(>|z|)'],
                      digits = digits,
                      eps = 1e-04,
                      nsmall = p.digits,
                      scientific = F
                    )
                else NA
              ),
              as.character
            )
          }
        )
      )
    }
  )
}


#' @title tabulate_at_risk
#' @description Returns a risk table from a model object and specified time points.
#' @param fit Required. survival::survfit() object.
#' @param times Required. Numeric. One or vector of times to calculate for.
#' @return Tibble risk table.
#' @export
tabulate_at_risk <- function(fit = NULL, times = NULL) {
  fit_summary <- summary(fit, times = times)
  data_risk <- tibble::tibble(
    strata =
      if (is.null(fit$strata)) 'All'
      else {
        purrr::map_chr(
          fit_summary$strata,
          function(x) stringr::str_split(x, '=')[[1]][2]
        )
      },
    time = fit_summary$time,
    n.risk = fit_summary$n.risk,
  )
  data_risk
}


#' @title tabulate_logit
#' @description Legacy function. Returns list of summary statistics from
#' data for a logistic regression model. Functionality will eventually be replicated
#' by tabulate_model().
#' @param formula Required. Formula. Y ~ X1 + X2 + X3
#' @param data Required. Tibble. Data used by the formula.
#' @return Returns model object containing:
#'   \item{obsCount}{The model's observation count.}
#'   \item{paramCount}{The model's parameter count.}
#'   \item{AIC}{The model's AIC.}
#'   \item{AICc}{The model's AICc.}
#'   \item{parameters}{A data.frame (tibble) of the model's estimated parameter coefficients.}
#'   \item{ORs}{A data.frame (tibble) of model parameters' odds ratios and confidence intervals calculated using Likelihood Ratio Tests.}
#'   \item{LRTs}{A data.frame (tibble) of Likelihood Ratio Test results for each model parameter.}
#'   \item{model}{The logistical regression model object.}
#' @export
tabulate_logit <- function(formula = NULL, data = NULL) {
  if (is.null(formula) | is.null(data))
    stop('Parameter missing. Take a look at \'forumla\' and \'data\'.')
  regression <- stats::glm(formula, data = data, family = 'binomial')
  AIC <- as.numeric(regression$aic)
  sampleSize <- as.numeric(length(regression[['fitted.values']]))
  parameterCount <- as.numeric(length(regression$coefficients))
  terms <- all.vars(regression$formula)
  terms[1] <- '(Intercept)'
  outcomes <- tibble::as.tibble(list(actual = regression$y, predicted = round(regression$fitted, 2)))

  list(
    obsCount = sampleSize,
    paramCount = parameterCount,
    AIC = AIC,
    AICc = AIC + (((2 * (parameterCount^2)) + (2 * parameterCount))/(sampleSize - parameterCount - 1)),
    parameters = tibble::add_column(
      .data = dplyr::select(
        .data = tibble::as.tibble(summary(regression)$coefficients),
        -dplyr::one_of(c('Std. Error', 'z value', 'Pr(>|z|)'))
      ),
      'Coefficients' = names(regression$coefficients),
      .before=1
    ),
    ORs = tibble::add_column(
      .data = tibble::as.tibble(
        exp(
          cbind(
            oddsRatio = stats::coef(regression),
            stats::confint(regression)
          )
        )
      ),
      'Coefficient' = names(regression$coefficients),
      .before=1
    ),
    LRTs = dplyr::slice(
      .data = tibble::add_column(
        .data = dplyr::select(
          .data = tibble::as.tibble(stats::drop1(regression, test="LRT")),
          -dplyr::one_of(c('Df', 'Deviance', 'AIC'))
        ),
        'Coefficient' = terms,
        .before=1
      ),
      -1
    ),
    model <- regression
  )
}
