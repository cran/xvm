#' summarize xvg data
#' compute basic summary statistics (mean, sd, min, median, max)
#' for each variable in one or more xvg_data objects.
#'
#' @param xvg_data a list of class 'xvg_data' or a list containing multiple 'xvg_data' objects, as returned by \code{read_xvg()}.
#' @param merge_results logical, whether to combine results from multiple objects (default: FALSE).
#' When TRUE, results will include a 'group' column identifying the source.
#' @return a data.frame with columns:
#'   \describe{
#'     \item{group}{(Optional) Source identifier when processing multiple objects with merge_results=TRUE.}
#'     \item{variable}{Name of the variable (column) in the xvg data.}
#'     \item{mean}{Arithmetic mean of that variable.}
#'     \item{sd}{Standard deviation.}
#'     \item{min}{Minimum value.}
#'     \item{median}{Median value.}
#'     \item{max}{Maximum value.}
#'   }
#' @importFrom stats sd median
#' @examples
#' path <- system.file("extdata/rmsd.xvg", package = "xvm")
#' xvg_data <- read_xvg(path)
#' summary_xvg(xvg_data)
#'
#' @export
summary_xvg <- function(xvg_data, merge_results = FALSE) {
  if (inherits(xvg_data, "xvg_data")) {
    df <- xvg_data$data
    vars <- names(df)[-1L]
    stats_list <- lapply(vars, function(v) {
      x <- df[[v]]
      data.frame(
        variable = v,
        mean     = mean(x, na.rm = TRUE),
        sd       = stats::sd(x, na.rm = TRUE),
        min      = min(x, na.rm = TRUE),
        median   = stats::median(x, na.rm = TRUE),
        max      = max(x, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
    stats_df <- do.call(rbind, stats_list)
    rownames(stats_df) <- NULL
    return(stats_df)
  }

  if (is.list(xvg_data) && length(xvg_data) > 0) {
    first_element <- xvg_data[[1]]
    if (inherits(first_element, "xvg_data")) {
      result_list <- list()
      for (i in seq_along(xvg_data)) {
        current_xvg <- xvg_data[[i]]
        if (!inherits(current_xvg, "xvg_data")) {
          warning("Item ", i, " is not of class 'xvg_data', skipping.")
          next
        }
        df <- current_xvg$data
        vars <- names(df)[-1L]
        stats_list <- lapply(vars, function(v) {
          x <- df[[v]]
          data.frame(
            group    = names(xvg_data)[i],
            variable = v,
            mean     = mean(x, na.rm = TRUE),
            sd       = stats::sd(x, na.rm = TRUE),
            min      = min(x, na.rm = TRUE),
            median   = stats::median(x, na.rm = TRUE),
            max      = max(x, na.rm = TRUE),
            stringsAsFactors = FALSE
          )
        })
        stats_df <- do.call(rbind, stats_list)
        result_list[[names(xvg_data)[i]]] <- stats_df
      }

      if (merge_results) {
        merged_df <- do.call(rbind, result_list)
        rownames(merged_df) <- NULL
        return(merged_df)
      } else {
        return(result_list)
      }
    } else {
      if (is.list(first_element) && !is.null(names(xvg_data)) &&
          any(sapply(first_element, function(x) inherits(x, "xvg_data")))) {
        result_list <- list()
        for (name in names(xvg_data)) {
          current_item <- xvg_data[[name]]
          if (!is.list(current_item) ||
              !any(sapply(current_item, function(x) inherits(x, "xvg_data")))) {
            warning("Item '", name, "' does not contain xvg_data objects, skipping.")
            next
          }

          current_results <- list()
          for (inner_name in names(current_item)) {
            inner_xvg <- current_item[[inner_name]]
            if (!inherits(inner_xvg, "xvg_data")) {
              next
            }
            df <- inner_xvg$data
            vars <- names(df)[-1L]
            stats_list <- lapply(vars, function(v) {
              x <- df[[v]]
              data.frame(
                group    = paste0(name, ":", inner_name),
                variable = v,
                mean     = mean(x, na.rm = TRUE),
                sd       = stats::sd(x, na.rm = TRUE),
                min      = min(x, na.rm = TRUE),
                median   = stats::median(x, na.rm = TRUE),
                max      = max(x, na.rm = TRUE),
                stringsAsFactors = FALSE
              )
            })
            stats_df <- do.call(rbind, stats_list)
            current_results[[inner_name]] <- stats_df
          }

          if (length(current_results) > 0) {
            if (merge_results) {
              result_list[[name]] <- do.call(rbind, current_results)
            } else {
              result_list[[name]] <- current_results
            }
          }
        }

        if (merge_results) {
          if (length(result_list) > 0) {
            merged_df <- do.call(rbind, result_list)
            rownames(merged_df) <- NULL
            return(merged_df)
          } else {
            warning("No valid xvg_data objects found to summarize.")
            return(NULL)
          }
        } else {
          return(result_list)
        }
      } else {
        stop("summary_xvg() expects an object of class 'xvg_data' or a list containing 'xvg_data' objects")
      }
    }
  } else {
    stop("summary_xvg() expects an object of class 'xvg_data' or a list containing 'xvg_data' objects")
  }
}
