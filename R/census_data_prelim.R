per <- function(x, y) {
    p <- round(((x/y)*100), 2)
    return(p)
  }

  ### Race - Table B02001
race <- tidycensus::get_acs(geography = "tract", table = "B02001",
                            year = 2018, state = "AZ", county = "Maricopa",
                            geometry = FALSE, output = "wide", cache_table = TRUE)

  # Remove and rename variables
race <- dplyr::mutate(race,
                        geoid     = GEOID,
                        name      = NAME,
                        race_all  = B02001_001E,
                        white     = B02001_002E,
                        black     = B02001_003E,
                        aian      = B02001_004E,
                        asian     = B02001_005E,
                        nhpi      = B02001_006E,
                        other     = B02001_007E,
                        race2     = B02001_008E,
                        p.white = per(white, race_all),
                        p.black = per(black, race_all),
                        p.aian  = per(aian, race_all),
                        p.asian = per(asian, race_all),
                        p.nhpi  = per(nhpi, race_all),
                        p.other = per(other, race_all),
                        p.race2 = per(race2, race_all))
race <- race[,c(23:39),]

# Save race data to table
final.census.data <- race

### Sex by Age - Table B01001

age <- tidycensus::get_acs(geography = "tract", table = "B01001",
                            year = 2018, state = "AZ", county = "Maricopa",
                            geometry = FALSE, output = "wide", cache_table = TRUE)

# Calculates age totals by adding male & female variables per age group
# Remove and rename variables
age <- dplyr::mutate(age,
                       geoid          = GEOID,
                       age_total      = B01001_001E,
                       age.under18    = B01001_003E + B01001_027E + B01001_004E + B01001_028E +
                                        B01001_005E + B01001_029E + B01001_006E + B01001_030E,
                       age.18to24     = B01001_007E + B01001_031E + B01001_008E + B01001_032E + B01001_009E +
                                        B01001_033E + B01001_010E + B01001_034E,
                       age.25to34     = B01001_011E + B01001_035E + B01001_012E + B01001_036E,
                       age.35to44     = B01001_013E + B01001_037E + B01001_014E + B01001_038E,
                       age.45to54     = B01001_015E + B01001_039E + B01001_016E + B01001_040E,
                       age.55to64     = B01001_017E + B01001_041E + B01001_018E + B01001_042E +
                                        B01001_019E + B01001_043E,
                       age.65over     = B01001_020E + B01001_044E + B01001_021E + B01001_045E +
                                        B01001_022E + B01001_046E + B01001_023E + B01001_047E +
                                        B01001_024E + B01001_048E + B01001_025E + B01001_049E,
                       p.age.under18  = per(age.under18, age_total),
                       p.age.18to24   = per(age.18to24, age_total),
                       p.age.25to34   = per(age.25to34, age_total),
                       p.age.35to44   = per(age.35to44, age_total),
                       p.age.45to54   = per(age.45to54, age_total),
                       p.age.55to64   = per(age.55to64, age_total),
                       p.age.65over   = per(age.65over, age_total))
age <- age[,c(101:116)]

final.census.data <- dplyr::left_join(final.census.data, age, by = "geoid")

  ### Hispanic/Latino - Table B03003
hisp <- tidycensus::get_acs(geography = geographic.level, table = "B03003",
                              year = year, state = state, county = county,
                              geometry = FALSE, output = "wide", cache_table = TRUE)

hisp <- dplyr::mutate(hisp,
                        geoid          = GEOID,
                        total_hispanic = B03003_001E,
                        not_hispanic   = B03003_002E,
                        hispanic       = B03003_003E,
                        p.hispanic     = per(hispanic, not_hispanic))
hisp <- hisp[,c(9, 12, 13),]

final.census.data <- dplyr::left_join(final.census.data, hisp, by = "geoid")

  ### Median Income - Table B19013

medinc <- tidycensus::get_acs(geography = geographic.level, table = "B19013",
                                year = year, state = state, county = county,
                                geometry = FALSE, output = "wide", cache_table = TRUE)

medinc <- dplyr::mutate(medinc,
                          geoid = GEOID,
                          medinc = B19013_001E)
medinc <- medinc[,c(5, 6)]

final.census.data <- dplyr::left_join(final.census.data, medinc, by = "geoid")

### k nearest neighbor
library(class)
pr <- knn(dia_train,dia_test,cl=dia_target, k = 5)

# Really lazy way of doing k nearest neighbor is to add absolute value of differences between demographics
# For example |county1$p.white - county2$p.white| + |county1$p.black - county2$p.black| + ... = value
# Lowest value would be the most similar
# Possible to do this for all 67 FL counties? So a 67x67 matrix with 0s on diagonal. Would need to group somehow.

