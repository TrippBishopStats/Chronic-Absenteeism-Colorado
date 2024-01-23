################################################################################
### Dataset processing workflow.
################################################################################

source("scripts/functions.R")

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


# Now that we have normalised the dataframes, we need to eliminate unnecessary
# rows and remove any duplicates.

# A quick visual inspection of the 2016 data shows that there are 177 school
# districts in the file. There are an additional 7 records in the table that are
# non-geographical school districts. These will be eliminated from this
# analysis. In addition, there is a "State Totals" row in the file that will
# also be removed.

invalid_county_codes <- c("90","98","STATE TOTALS")

df_2016 <- df_2016 |> 
  filter(!county_code %in% invalid_county_codes)

# There are now 177 school districts in the dataframe.

# Now we will look at the 2017-2018 school year data. This file has the same
# invalid districts but it also has some extra rows that do not contain data.
# They are just artifacts of the Excel spreadsheets that are the original source
# of the data.

df_2017 <- df_2017 |> 
  filter(!county_code %in% invalid_county_codes & !is.na(county_code))

# There are now 178 records in the dataset. It seems like there is a school
# district missing from the 2016 data. We can do an anti-join to locate the
# school district.

df_2017 |> 
  anti_join(df_2016, by="district_code")

# This shows that the Karval district is missing from the 2016 data. We will add
# the record to 2016 and put NA for the student data fields.

df_2016 <- extract_missing_district(df_2017, df_2016)

# The missing school district has now been added to the dataframe.

# Now, examine the 2018 school year dataset.
df_2018 |> 
  filter(!county_code %in% invalid_county_codes & !is.na(county_code)) |> 
  count()

# Like 2016, there are now only 177 school districts, we can use the same
# technique to identify which district is missing.

df_2018_tmp <- df_2018 |> 
  filter(!county_code %in% invalid_county_codes & !is.na(county_code))

df_2017 |> 
  anti_join(df_2018_tmp, by="district_code")

# The CHERAW 31 district is missing. Let's add it to the 2018 dataset.
df_2018 <- extract_missing_district(df_2017, df_2018_tmp)
rm(df_2018_tmp)

# Now there are 178 districts in the dataset.

# Moving on to 2019. There are new challenges here. The totals row is now in a
# different format and the previous filtering methods won't work, so we'll use a
# custom approach for this school year.
df_2019 |> 
  filter(!county_code %in% invalid_county_codes & county_code != "COLORADO TOTAL") |> 
  count()

# There are 178 school districts, so no more processing is required.
df_2019 <- df_2019 |> 
  filter(!county_code %in% invalid_county_codes & county_code != "COLORADO TOTAL")

# Now let's look at 2020. The pattern here is similar to 2019, but the totals
# column is now labeled "State Total", so we need to modify approach slightly.
df_2020 |> 
  filter(!county_code %in% invalid_county_codes & county_code != "State Total") |> 
  count()

df_2020_tmp <- df_2020 |> 
  filter(!county_code %in% invalid_county_codes & county_code != "State Total")

# Again, we are missing a school district. Which one is it?
df_2017 |> 
  anti_join(df_2020_tmp, by="district_code")

# This time, it's the Holly RE-3 school district that is missing.
df_2020 <- extract_missing_district(df_2017, df_2020_tmp)
rm(df_2020_tmp)

# Moving on to the 2021 school year dataset. This one is different yet again,
# but patterns that we have used before can be applied to clean this up.
df_2021 |> 
  filter(!county_code %in% invalid_county_codes & 
          county_code != "State Total*" & 
          !is.na(county_code)) |> 
  count()

df_2021 <- df_2021 |> 
  filter(!county_code %in% invalid_county_codes & 
           county_code != "State Total*" & 
           !is.na(county_code))

# This results in 178 school districts, so we can move on.

# The last dataset, the 2022-2023 school year. It looks similar in layout to
# 2020.

df_2022 |> 
  filter(!county_code %in% invalid_county_codes & county_code != "State Total") |> 
  count()

df_2022 <- df_2022 |> 
  filter(!county_code %in% invalid_county_codes & county_code != "State Total")

# There are now 178 records, so this dataset is ready.

# All of the dataset are now ready. They all contain an record for each school
# district.

# Finally, write CSV files to output folder for each dataset so that they can be
# reused without have to reapply the processing steps.
write_csv(df_2016, file="output/2016-2017_chronic_absenteeism.csv")
write_csv(df_2017, file="output/2017-2018_chronic_absenteeism.csv")
write_csv(df_2018, file="output/2018-2019_chronic_absenteeism.csv")
write_csv(df_2019, file="output/2019-2020_chronic_absenteeism.csv")
write_csv(df_2020, file="output/2020-2021_chronic_absenteeism.csv")
write_csv(df_2021, file="output/2021-2022_chronic_absenteeism.csv")
write_csv(df_2022, file="output/2022-2023_chronic_absenteeism.csv")
