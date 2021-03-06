#' Calculates index scores for games related to conflict effect
#'
#' Count of correct responses per minute and switch cost of correct response and
#' reaction time are all included. The switch cost is the mean difference
#' between repeat trials and switch trials. There is also one so-called general
#' switch cost, which is the mean difference between repeat trials and pure
#' blocks.
#'
#' @param data Raw data of class `data.frame`.
#' @param ... Other input argument for future expansion.
#' @return A [tibble][tibble::tibble-package] contains following values:
#'   \item{rc_all}{Count of correct responses per minute for all blocks.}
#'   \item{rc_pure}{Count of correct responses per minute for pure blocks.}
#'   \item{rc_mixed}{Count of correct responses per minute for mixed blocks.}
#'   \item{switch_cost_rc_gen}{General switch cost (based on count of correct
#'     responses).}
#'   \item{mrt_pure}{Mean reaction time for non-mixed blocks.}
#'   \item{mrt_repeat}{Mean reaction time for repeat trials.}
#'   \item{mrt_switch}{Mean reaction time for switch trials.}
#'   \item{switch_cost_gen_rt}{General switch cost (based on mean reaction
#'     times).}
#'   \item{switch_cost_spe_rt}{Specific switch cost (based on mean reaction
#'     times).}
#'   \item{is_normal}{Checking result whether the data is normal.}
#' @export
switchcost <- function(data, ...) {
  vars_output <- c(
    "rc_all", "rc_mixed", "rc_pure", "switch_cost_rc_gen",
    "mrt_pure", "mrt_repeat", "mrt_switch",
    "switch_cost_rt_gen", "switch_cost_rt_spe"
  )
  vars_required <- tibble::tribble(
    ~field, ~name,
    "name_block", "Block",
    "name_task", "Task",
    "name_switch", "Type",
    "name_acc", "ACC",
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
  # summarize information of each block
  block_info <- data %>%
    dplyr::mutate(n_blocks = dplyr::n_distinct(.data$Block)) %>%
    dplyr::group_by(.data$n_blocks, .data$Block) %>%
    dplyr::summarise(
      has_no_response = all(.data$ACC == -1),
      type_block = ifelse(
        all(
          is.na(.data$Type) |
            .data$Type %in% c("preswitch", "postswitch", "Pure", "")
        ),
        "pure", "mixed"
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      dur = dplyr::case_when(
        .data$n_blocks == 5 ~ 1,
        .data$n_blocks >= 6 & .data$type_block == "pure" ~ 0.5,
        .data$n_blocks >= 6 & .data$type_block == "mixed" ~ 1,
        TRUE ~ NA_real_
      )
    )
  data_adj <- data %>%
    # set as wrong for responses that are too quick
    dplyr::mutate(acc_adj = ifelse(.data$RT >= 100, .data$ACC, 0))
  if (any(block_info$has_no_response)) {
    warning("At least one block has no response.")
    return(
      rlang::set_names(
        rep(NA, length(vars_output)),
        nm = vars_output
      ) %>%
        tibble::as_tibble_row() %>%
        tibble::add_column(is_normal = FALSE)
    )
  }
  switch_cost <- calc_switch_cost(
    data_adj, block_info,
    name_acc = "acc_adj"
  )
  validation <- data_adj %>%
    dplyr::summarise(nt = dplyr::n(), nc = sum(.data$acc_adj == 1)) %>%
    dplyr::transmute(
      is_normal = .data$nc > stats::qbinom(0.95, .data$nt, 0.5) &&
        !any(block_info$has_no_response)
    )
  tibble(switch_cost, validation)
}
