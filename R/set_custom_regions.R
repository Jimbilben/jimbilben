#' Set Custom Regions
#'
#' Set variables in your R environment representing different regions of the USA and the states that comprise them. This is necessary for weighting and MRP.
#'
#' @param set If TRUE, this will define the region variables to your R environment, set to TRUE by default
#' @param print If TRUE, this will just print out the region definitions to your R console, set to TRUE by default
#'
#' @export
set_custom_regions <- function(set = TRUE, print = FALSE) {

  midwest <- c('Illinois', 'IL', 'Indiana', 'IN', 'Iowa', 'IA', 'Michigan', 'MI', 'Minnesota', 'MN',
                'Ohio', 'OH', 'Pennsylvania', 'PA', 'Wisconsin', 'WI')
  mountains <- c('Alaska', 'AK', 'Idaho', 'ID', 'Kansas', 'KS', 'Montana', 'MT', 'Nebraska', 'NE',
                  'North Dakota', 'ND', 'South Dakota', 'SD', 'Utah', 'UT', 'Wyoming', 'WY', 'Oklahoma', 'OK')
  northeast <- c('Connecticut', 'CT', 'Delaware', 'DE', 'District of Columbia (DC)',
                  'District of Columbia', 'DC', 'Washington (District of Columbia)', 'Washington (DC)',
                  'Maine', 'ME', 'Maryland', 'MD', 'Massachusetts', 'MA', 'New Hampshire', 'NH',
                  'New Jersey', 'NJ', 'New York', 'NY', 'Rhode Island', 'RI', 'Vermont', 'VT')
  pacific <- c('California', 'CA', 'Hawaii', 'HI', 'Oregon', 'OR', 'Washington', 'WA', 'Guam', 'GU',
                'Puerto Rico', 'PR', 'Virgin Islands', 'VI')
  south <- c('Missouri', 'MO', 'Tennessee', 'TN', 'Alabama', 'AL', 'Arkansas', 'AR', 'Kentucky', 'KY',
              'Louisiana', 'LA', 'Mississippi', 'MS', 'Texas', 'TX', 'Virginia', 'VA', 'West Virginia', 'WV')
  southwest <- c('Arizona', 'AZ', 'Colorado', 'CO', 'Nevada', 'NV', 'New Mexico', 'NM')
  southeast <- c('Florida', 'FL', 'Georgia', 'GA', 'North Carolina', 'NC', 'South Carolina', 'SC')

  if(print == TRUE) {
    print(c(midwest,
            mountains,
            northeast,
            pacific,
            south,
            southwest,
            southeast))
  }

  if(set == TRUE) {
    midwest <<- midwest
    mountains <<- mountains
    northeast <<- northeast
    pacific <<- pacific
    south <<- south
    southwest <<- southwest
    southeast <<- southeast
  }

}
