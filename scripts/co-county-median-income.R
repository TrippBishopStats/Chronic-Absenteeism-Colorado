library(rvest)

# get the list of distinct county names
counties <- df_master |> 
  distinct(county_name) |> 
  pull()

# create the dataframe with the Adams county data in it.
county_url <- "https://www.neilsberg.com/insights/adams-county-co-median-household-income/"
content <- read_html(county_url)
tables <- content|> html_table(fill = TRUE)
df_county_median_income <- tables[[1]]
df_county_median_income <- df_county_median_income[-1,]
df_county_median_income <- df_county_median_income %>% clean_names()
# add the county name to the data
df_county_median_income <- df_county_median_income |> 
  filter(year > 2015) |> 
  mutate(
    county_name = "Adams"
  )

# now loop over the other counties and exact median data for the rest of the
# counties
for(county in tail(counties, n=62)) {
  str_county <- str_to_lower(str_replace(county, "\\s","-"))
  county_url <- paste("https://www.neilsberg.com/insights/",str_county, "-county-co-median-household-income/",sep="")
  content <- read_html(county_url)
  tables <- content|> html_table(fill = TRUE)
  median_incom_table <- tables[[1]]
  median_incom_table <- median_incom_table[-1,]
  median_incom_table <- median_incom_table %>% clean_names()
  median_incom_table <- median_incom_table |> 
    filter(year > 2015) |> 
    mutate(
      county_name = county
    )
  # append the new data to the main county dataframe
  df_county_median_income <- bind_rows(df_county_median_income,median_incom_table)
  print(paste(county, "successfully parsed.", sep=" "))
}

# move the county column to the "front" of the dataframe.
df_county_median_income <- df_county_median_income |> 
  relocate(county_name, .before=year)

# save the data locally
write_csv(df_county_median_income, "output/CO-county-median-incomes-2016-2021.csv")

# clean up intermediate objects
rm(content, tables, county_url, counties)
