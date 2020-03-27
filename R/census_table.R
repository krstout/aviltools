#' Census Data Function
#'
#' This function creates percentages.
#' @param geographic.level The geographic level for census data. Usually "county", "tract", or "block group".
#' @param year Census data year.
#' @param state Two letter abbreviation for state.
#' @param county Desired county for data.
#' @keywords census data
#' @export
#' @examples
#' census_table("tract", 2018, "AZ", "Maricopa")

census_table <- function(geographic.level, year, state, county) {
  per <- function(x, y) {
    p <- round(((x/y)*100), 2)
    return(p)
  }

  ### Race - Table B02001
  race <- tidycensus::get_acs(geography = geographic.level, table = "B02001",
                              year = year, state = state, county = county,
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
                        per.white = per(white, race_all),
                        per.black = per(black, race_all),
                        per.aian  = per(aian, race_all),
                        per.asian = per(asian, race_all),
                        per.nhpi  = per(nhpi, race_all),
                        per.other = per(other, race_all),
                        per.race2 = per(race2, race_all))
  race <- race[,c(23:39),]

  # Save race data to table
  final.census.data <- race

  ### Sex by Age - Table B01001

  age <- tidycensus::get_acs(geography = geographic.level, table = "B01001",
                             year = year, state = state, county = county,
                             geometry = FALSE, output = "wide", cache_table = TRUE)

  # Calculates age totals by adding male & female variables per age group
  # Remove and rename variables
  age <- dplyr::mutate(age,
                       geoid          = GEOID,
                       age_total_pop  = B01001_001E,
                       age_over18     = B01001_001E - (B01001_003E + B01001_027E + B01001_004E + B01001_028E +
                                                         B01001_005E + B01001_029E + B01001_006E + B01001_030E),
                       age_18to24     = B01001_007E + B01001_031E + B01001_008E + B01001_032E + B01001_009E +
                         B01001_033E + B01001_010E + B01001_034E,
                       per.over18     = per(age_over18, age_total_pop),
                       per.young      = per(age_18to24, age_total_pop))
  age <- age[,c(101:106)]

  final.census.data <- dplyr::left_join(final.census.data, age, by = "geoid")

  ### Hispanic/Latino - Table B03003
  hisp <- tidycensus::get_acs(geography = geographic.level, table = "B03003",
                              year = year, state = state, county = county,
                              geometry = FALSE, output = "wide", cache_table = TRUE)

  hisp <- dplyr::mutate(hisp,
                        geoid     = GEOID,
                        total_his = B03003_001E,
                        not_his   = B03003_002E,
                        his       = B03003_003E,
                        per.his   = per(his, total_his))
  hisp <- hisp[,c(9:13),]

  final.census.data <- dplyr::left_join(final.census.data, hisp, by = "geoid")
  return(final.census.data)
}
