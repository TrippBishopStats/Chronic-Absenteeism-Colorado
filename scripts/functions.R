### Helper functions for analysis

################################################################################
### Look for missing school districts in a given dataset.                    ###
################################################################################
extract_missing_district <- function(df_source, df_target) {
  
  distinct_school_year <- df_target |> 
    distinct(school_year) |> 
    pull()
  
  missing_sd <- df_source |> 
    anti_join(df_target, by="district_code") |> 
    mutate(
      school_year = distinct_school_year,
      absentee_students = NA,
      total_students = NA,
      chronically_absent_rate = NA
    )
  
  bind_rows(df_target, missing_sd) |> arrange(district_code)
}

################################################################################
### Sum district codes for a given dataset.                                  ###
################################################################################
compute_district_total <- function(data) {
  data |> 
    distinct(district_code) |> 
    mutate(
      district_code = as.numeric(district_code)
    ) |> 
    summarise(
      district_check = sum(district_code)
    ) |> 
    pull()
}

################################################################################
### Formats a table width reasonable default values.                         ###
################################################################################
format_table <- function(table, table_title="", table_sub_title="") {
  table_id <- table$`_options` |> filter(parameter == "table_id") |> select(value) |> pull()
  table |> tab_header(
    title = table_title,
    subtitle = table_sub_title
  ) |> 
  fmt_number(drop_trailing_zeros = TRUE, decimals = 1) |> 
  opt_css(
    css = paste("#", table_id, " .gt_table {width: 100%;}", sep="")
  )
}
