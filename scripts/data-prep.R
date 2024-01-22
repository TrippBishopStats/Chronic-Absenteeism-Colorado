library(tidyverse)
library(janitor)
library(readxl)
rm(list=ls())

# The first N rows of the spreadsheet are not useful. The actual headers start
# on line N+1 and the data follows from there.
df_2016 <- read_xlsx("data/2016-2017_ChronicAbsenteeism.xlsx", skip = 8)
df_2017 <- read_xlsx("data/2017-2018_ChronicAbsenteeism.xlsx", skip = 8)
df_2018 <- read_xlsx("data/2018-2019_ChronicAbsenteeism.xlsx", skip = 4)
df_2019 <- read_xlsx("data/2019-2020_ChronicAbsenteeism.xlsx", skip = 4)
df_2020 <- read_xlsx("data/2020-2021_ChronicAbsenteeism.xlsx", skip = 4)
df_2021 <- read_xlsx("data/2021-2022_ChronicAbsenteeism.xlsx", skip = 4)
df_2022 <- read_xlsx("data/2022-2023_ChronicAbsenteeism.xlsx", skip = 4)

# Let's take a quick look at the field names for each file.

# 2016 is different to all other datasets. It has the fewest number of fields.
names(df_2016)

# 2017 & 2018 have the same fields
names(df_2017)
names(df_2018)

# 2019 - 2021 have the same fields except the student count field which includes
# the school year. It sounds like Pre-K students are not factored in to
# absenteeism rates. In the previous 3 datasets the grades included in the
# accounting are mentioned in the field name, but in these 3 data sets that is
# not the case. Furthermore, these datasets are missing the `Truancy Rate` field
# that is present in the previous three datasets.
names(df_2019)
names(df_2020)
names(df_2021)

# 2022
names(df_2022)

# The analysis is going to be about chronic absenteeism, so the truancy fields
# can be dropped. Since there is a lot of variability in the fields across
# datasets, it will be easier to just remove these fields since they won't be
# included in any analysis.

rename_cols <- c("absentee_students"="number_of_students_with_chronic_absenteeism", 
                 "total_students"="student_fall_k_12_enrollment",
                 "chronically_absent_rate" = "percent_chronically_absent")

df_2016 <- df_2016 |> 
  clean_names() |> 
  select(-truancy_rate) |> 
  rename(all_of(rename_cols)) |> 
  mutate(school_year = "2016-2017") |> 
  select(school_year, everything())

df_2017 <- df_2017 |> 
  clean_names() |> 
  select(-truancy_rate, -starts_with("students")) |>
  rename(all_of(rename_cols))

names(2016) == names(2017)

# 2016 & 2017 are normalised now.

df_2018 <- df_2018 |> 
  clean_names() |> 
  select(-truancy_rate, -starts_with("students")) |>
  rename(all_of(rename_cols[1:2]))

names(2017) == names(2018)

# 2018 is normalised now.

df_2019 <- df_2019 |> 
  clean_names() |> 
  select(-starts_with("students")) |>
  rename(all_of(rename_cols[1])) |> 
  rename("total_students"="x2019_2020_student_count")

names(2018) == names(2019)

# 2019 is normalised now.

df_2020 <- df_2020 |> 
  clean_names() |> 
  select(-starts_with("students")) |>
  rename(all_of(rename_cols[1])) |> 
  rename("total_students"="x2020_2021_student_count")

names(2019) == names(2020)

# 2020 is normalised now.

df_2021 <- df_2021 |> 
  clean_names() |> 
  select(-starts_with("students")) |>
  rename(all_of(rename_cols[1])) |> 
  rename("total_students"="x2021_2022_student_count")

names(2020) == names(2021)

# 2021 is normalised now.

# this last dataset is a bit different to those that precede it. Main challenge
# is that the total student count and absentee student counts need to be swapped
# so that this dataset is consistent with the others after the standard
# structural transformations have been applied.

df_2022 <- df_2022 |> 
  clean_names() |> 
  select(-pk_12_student_count) |>
  rename(all_of(c("total_students"="k_12_student_count_used_in_chronic_absent_rate",
                  "absentee_students" = "chronic_absent_count"))) |> 
  relocate(absentee_students, .before=total_students)

# write CSV files to output folder
write_csv(df_2016, file="output/2016-2017_chronic_absenteeism.csv")



