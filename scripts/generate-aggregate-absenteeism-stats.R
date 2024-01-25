df_master |> 
  group_by(district_size, school_year) |> 
  summarise(
    avg_rate = sum(absentee_students, na.rm = TRUE)/sum(total_students, na.rm = TRUE)
  ) |> 
  write_csv(file = "output/weighted-avg-district-size-absenteeism.csv")
