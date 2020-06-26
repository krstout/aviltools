
az <- county_table("county", 2018, "AZ")

az$race_all_n <- scale(az$race_all, scale = T)
summary(az$race_all_n)

azn <- scale(az$race_all)

az$race_all_n <- (az$race_all-min(az$race_all))/(max(az$race_all)-min(az$race_all))
summary(az$race_all_n)

# probably better to normalize variables rather than scale...not completely sure

d1 <- abs(outer(az$p.white, az$p.white, '-'))
d2 <- abs(outer(az$p.black, az$p.black, '-'))
d3 <- abs(outer(az$p.aian, az$p.aian, '-'))
d4 <- abs(outer(az$p.asian, az$p.asian, '-'))
d5 <- abs(outer(az$p.nhpi, az$p.nhpi, '-'))
d6 <- abs(outer(az$p.other, az$p.other, '-'))
d7 <- abs(outer(az$p.race2, az$p.race2, '-'))

matrix.list <- list(d1, d2, d3, d4, d5, d6, d7)

d8 <- Reduce('+', matrix.list)
