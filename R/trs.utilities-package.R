#' @description Helpers functions to reproduce results from the TRS project.
#' @keywords internal
#' @importFrom rlang .data := %||%
#' @importFrom rlang quos quo enquo exprs parse_exprs fn_fmls
#' @importFrom rlang is_formula is_empty is_false is_true
#' @importFrom rlang abort caller_env
#' @importFrom tidyselect all_of any_of starts_with ends_with matches contains
#' @importFrom tidyselect vars_select_helpers
#' @importFrom dplyr mutate summarize across select pull distinct arrange relocate
#' @importFrom dplyr filter between rowwise slice_sample
#' @importFrom dplyr group_by ungroup group_vars
#' @importFrom dplyr bind_cols left_join
#' @importFrom dplyr n vars desc if_else
#' @importFrom dplyr rename rename_with
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_abline geom_line geom_vline geom_hline geom_segment
#' @importFrom ggplot2 geom_point geom_errorbar geom_histogram
#' @importFrom ggplot2 geom_ribbon geom_smooth
#' @importFrom ggplot2 facet_wrap facet_grid as_labeller label_parsed
#' @importFrom ggplot2 scale_color_manual scale_colour_brewer scale_fill_manual
#' @importFrom ggplot2 scale_shape_manual scale_y_continuous scale_alpha_manual
#' @importFrom ggplot2 theme element_blank element_text margin unit
#' @importFrom ggplot2 coord_cartesian labs xlab
#' @importFrom tibble tibble as_tibble rowid_to_column
#' @importFrom tidyr pivot_wider nest unnest drop_na
#' @importFrom readr write_rds read_rds read_csv
#' @importFrom purrr map map2 imap pluck list_rbind
#' @importFrom stats setNames density median quantile var model.matrix
#' @importFrom janitor clean_names make_clean_names
#' @importFrom glue glue
#' @importFrom utils modifyList
#' @importFrom colorspace darken
"_PACKAGE"
