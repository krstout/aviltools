az <- county_table("county", 2018, "AZ")

az$race_all_n <- scale(az$race_all, scale = T)
summary(az$race_all_n)

azn <- scale(az$race_all)

az$race_all_n <- (az$race_all-min(az$race_all))/(max(az$race_all)-min(az$race_all))
summary(az$race_all_n)

# probably better to normalize variables rather than scale...not completely sure
