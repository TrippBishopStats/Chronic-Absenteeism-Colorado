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
### Histogram of district chronic absentee rates.                            ###
################################################################################
generate_absentee_rate <- function(data) {
  school_year <- data |> distinct(school_year) |> pull()
  data |>
    filter(!is.na(chronically_absent_rate)) |> 
    ggplot(aes(x=chronically_absent_rate)) +
    geom_histogram(binwidth = 0.05, colour="white", fill=standard_chart_colour) +
    labs(
      title = "Distribution of district chronic absenteeism",
      subtitle = paste("School year", school_year),
      x="Chronically absent rate",
      y="Count"
    )
}

################################################################################
### Histogram of district total enrollments.                                 ###
################################################################################
generate_total_enrollment <- function(data) {
  school_year <- data |> distinct(school_year) |> pull()
  data |>
    filter(!is.na(total_students)) |> 
    ggplot(aes(x=total_students)) +
    geom_histogram(colour="white", fill=standard_chart_colour) +
    labs(
      title = "Distribution of district student enrollment",
      subtitle = paste("School year",school_year),
      x="Student enrollment",
      y="Count"
    )
}

################################################################################
### Histogram of district absentee students.                                 ###
################################################################################
generate_absentee_enrollment <- function(data) {
  school_year <- data |> distinct(school_year) |> pull()
  data |>
    filter(!is.na(absentee_students)) |> 
    ggplot(aes(x=absentee_students)) +
    geom_histogram(colour="white", fill=standard_chart_colour) +
    labs(
      title = "Distribution of district chronic absenteeism",
      subtitle = paste("School year", school_year),
      x="Absentee students",
      y="Count"
    )
}