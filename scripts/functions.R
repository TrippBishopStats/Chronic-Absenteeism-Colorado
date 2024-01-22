### Helper functions for analysis

extract_missing_district <- function(df_source, df_target) {
  missing_sd <- df_source |> 
    anti_join(df_target, by="district_code") |> 
    mutate(
      absentee_students = NA,
      total_students = NA,
      chronically_absent_rate = NA
    )
  
  bind_rows(df_target, missing_sd) |> arrange(district_code)
}