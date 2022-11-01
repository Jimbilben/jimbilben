#' Set Census Regions
#'
#' Set variables in your R environment representing different regions of the USA from the Census https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf and the states that comprise them. This is necessary for weighting and MRP.
#'
#' @param set If TRUE, this will define the region variables to your R environment, set to TRUE by default
#' @param print If TRUE, this will just print out the region definitions to your R console, set to TRUE by default
#'
#' @export
set_census_regions <- function(set = TRUE, print = FALSE) {

  northeast <- c("Connecticut", "CT",
                 "Maine", "ME",
                 "Massachusetts", "MA",
                 "New Hampshire", "NH",
                 "Rhode Island", "RI",
                 "Vermont", "VT",
                 "New Jersey", "NJ",
                 "New York", "NY",
                 "Pennsylvania", "PA")

  midwest <- c("Indiana", "IN",
               "Illinois", "IL",
               "Michigan", "MI",
               "Ohio", "OH",
               "Wisconsin", "WI",
               "Iowa", "IA",
               "Kansas", "KS",
               "Minnesota", "MN",
               "Missouri", "MO",
               "Nebraska", "NE",
               "North Dakota", "ND",
               "South Dakota", "SD")

  south <- c("Delaware", "DE",
             "District of Columbia", "Washington DC", "DC", 'Washington (DC)', 'Washington, DC', 'Washington (District of Columbia)',
             "Florida", "FL",
             "Georgia", "GA",
             "Maryland", "MD",
             "North Carolina", "NC",
             "South Carolina", "SC",
             "Virginia", "VA",
             "West Virginia", "WV",
             "Alabama", "AL",
             "Kentucky", "KY",
             "Mississippi", "MS",
             "Tennessee", "TN",
             "Arkansas", "AR",
             "Louisiana", "LA",
             "Oklahoma", "OK",
             "Texas", "TX")

  west <- c("Arizona", "AZ",
            "Colorado", "CO",
            "Idaho", "ID",
            "New Mexico", "NM",
            "Montana", "MT",
            "Utah", "UT",
            "Nevada", "NV",
            "Wyoming", "WY",
            "Alaska", "AK",
            "California", "CA",
            "Hawaii", "HI",
            "Oregon", "OR",
            "Washington", "WA")


  if(print == TRUE) {

    print(glue::glue("Northeast = {northeast}"))
    print(glue::glue("Midwest = {midwest}"))
    print(glue::glue("South = {south}"))
    print(glue::glue("West = {west}"))

    print("Source: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf")
  }

  if(set == TRUE) {
    midwest <<- midwest
    west <<- west
    northeast <<- northeast
    south <<- south
  }

}
