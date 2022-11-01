#' Set Census Divisions
#'
#' Set variables in your R environment representing different divisions of the USA from the Census https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf and the states that comprise them. This is necessary for weighting and MRP.
#'
#' @param set If TRUE, this will define the region variables to your R environment, set to TRUE by default
#' @param print If TRUE, this will just print out the region definitions to your R console, set to TRUE by default
#'
#' @export
set_census_divisions <- function(set = TRUE, print = FALSE) {

  pacific <- c("Alaska", "AK",
               "California", "CA",
               "Hawaii", "HI",
               "Oregon", "OR",
               "Washington", "WA")
  mountain <- c("Arizona", "AZ",
                "Colorado", "CO",
                "Idaho", "ID",
                "New Mexico", "NM",
                "Montana", "MT",
                "Utah", "UT",
                "Nevada", "NV",
                "Wyoming", "WY")
  enc <- c("Indiana", "IN",
           "Illinois", "IL",
           "Michigan", "MI",
           "Ohio", "OH",
           "Wisconsin", "WI")
  wsc <- c("Arkansas", "AR",
           "Louisiana", "LA",
           "Oklahoma", "OK",
           "Texas", "TX")
  wnc <- c("Iowa", "IA",
           "Kansas", "KS",
           "Minnesota", "MN",
           "Missouri", "MO",
           "Nebraska", "NE",
           "North Dakota", "ND",
           "South Dakota", "SD")
  esc <- c("Alabama", "AL",
           "Kentucky", "KY",
           "Mississippi", "MS",
           "Tennessee", "TN")
  satlantic <- c("Delaware", "DE",
                 "District of Columbia", "Washington DC", "DC", 'Washington (DC)', 'Washington, DC', 'Washington (District of Columbia)',
                 "Florida", "FL",
                 "Georgia", "GA",
                 "Maryland", "MD",
                 "North Carolina", "NC",
                 "South Carolina", "SC",
                 "Virginia", "VA",
                 "West Virginia", "WV")
  matlantic <- c("New Jersey", "NJ",
                 "New York", "NY",
                 "Pennsylvania", "PA")
  newengland <- c("Connecticut", "CT",
                  "Maine", "ME",
                  "Massachusetts", "MA",
                  "New Hampshire", "NH",
                  "Rhode Island", "RI",
                  "Vermont", "VT")


  if(print == TRUE) {

    print(glue::glue("pacific = {pacific}"))
    print(glue::glue("mountain = {mountain}"))
    print(glue::glue("enc = {enc}"))
    print(glue::glue("wsc = {wsc}"))
    print(glue::glue("wnc = {wnc}"))
    print(glue::glue("esc = {esc}"))
    print(glue::glue("satlantic = {satlantic}"))
    print(glue::glue("matlantic = {matlantic}"))
    print(glue::glue("newengland = {newengland}"))

    print("Source: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf")
  }

  if(set == TRUE) {
    pacific <<- pacific
    mountain <<- mountain
    enc <<- enc
    wsc <<- wsc
    esc <<- esc
    satlantic <<- satlantic
    matlantic <<- matlantic
    newengland <<- newengland
    wnc <<- wnc
  }

}
