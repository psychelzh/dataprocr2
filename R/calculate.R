#' Calculate indices for a single game
#'
#' Given a game and a function name, `calculate()` extract data of that game and
#' calculate indices accordingly.
#'
#' @author Liang Zhang (psychelzh@outlook.com)
#' @param data The dataset used to calculate indices.
#' @param game_id The id of game to be processed.
#' @param prep_fun A name (or symbol) of function.
#' @param ... Further input for the processing function.
#' @return A [tibble][tibble::tibble-package]. A new variable called "indices",
#'   each of which contains a [tibble][tibble::tibble-package] row of calculated
#'   indices, is added to the data.
#' @importFrom rlang .env
#' @export
calculate <- function(data, game_id, prep_fun, ...) {
  # extract all the valid of current game
  cur_game_data <- data %>%
    dplyr::filter(.data$game_id == .env$game_id) %>%
    dplyr::filter(purrr::map_lgl(.data$game_data, jsonlite::validate))
  # if no data found, no further processing is needed
  if (nrow(cur_game_data) == 0) {
    return(NULL)
  }
  # use `prep_fun` to calculate
  cur_game_data %>%
    dplyr::mutate(
      indices = purrr::map(
        .data$game_data,
        ~ prep_fun(jsonlite::fromJSON(.x), ...)
      )
    )
}
