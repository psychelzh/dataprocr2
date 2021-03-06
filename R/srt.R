#' Calculates index scores for Simple Reaction Time game
#'
#' Mean reaction time is returned.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A [tibble][tibble::tibble-package] contains following values:
#'   \item{mrt}{Mean reaction time}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
srt <- function(data, ...) {
  vars_output <- c("percent_valid", "mrt")
  vars_required <- tibble::tribble(
    ~field, ~name,
    "name_rt", "RT"
  )
  vars_matched <- match_data_vars(data, vars_required)
  if (is.null(vars_matched)) {
    return(
      rlang::set_names(
        rep(NA, length(vars_output)),
        nm = vars_output
      ) %>%
        tibble::as_tibble_row() %>%
        tibble::add_column(is_normal = FALSE)
    )
  }
  tibble(data) %>%
    dplyr::summarise(
      percent_valid = mean(.data$RT > 100),
      mrt = mean(.data$RT[.data$RT > 100]),
      is_normal = .data$percent_valid >= 0.8
    )
}
