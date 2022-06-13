#' @description Package containing helper functions to reproduce results from the TRS project.
#' @keywords internal
#' @importFrom rlang .data is_formula is_empty quos quo enquo :=
#' @importFrom dplyr %>% mutate summarize select across filter between rowwise group_by ungroup group_vars if_else pull distinct bind_cols arrange left_join rename slice_sample n vars tibble as_tibble all_of any_of starts_with ends_with matches rename_with
#' @importFrom ggplot2 ggplot aes geom_abline geom_ribbon geom_line geom_point geom_errorbar geom_vline geom_hline geom_segment facet_grid as_labeller scale_color_manual scale_fill_manual scale_shape_manual scale_y_continuous theme element_blank element_text margin unit xlab
#' @importFrom tidyr pivot_wider nest unnest drop_na
#' @importFrom readr write_rds read_rds read_csv
#' @importFrom purrr map_df map map2
#' @importFrom stats setNames density median quantile var model.matrix
#' @importFrom janitor clean_names make_clean_names
#' @importFrom glue glue
#' @importFrom toolkit has_metachr has_names sniff_dfs round_nearest proportion standardize parse_formula parse_num make_dir make_list delta string_replace string_replace_all string_extract string_extract_all string_strip string_clean uc_first first_word to_snake_case vec_to_chr_class list_files extract_file_name line_0 theme_elesic theme_blank_y
"_PACKAGE"
