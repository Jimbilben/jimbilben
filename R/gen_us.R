#' gen_us
#'
#' Generate a weighting target (technically, a weights::wpct() object) for a specified variable in the US population. Run the function without any specification to see your options.
#'
#' @param target_variable A character argument indicating for which variable to specify the weighting target. The default "options" will just print the possible variables you can choose.
#' @param .moving_average Numeric, whole numbers only, 1 by default. Indicating how many of the past quarters to average over when getting weights for partyid, with proportions provided from Gallup.
#' @param .gallup_url The input URL needed to get to the quarterly ratings - it is necessary to contact Gallup to get the URL.
#' @param partyid_as_ind_other If TRUE, partyid will be percentages based on collapsing other parties and don't know responses into Independent. If FALSE, it will work with only Democrat, Independent, and Other specifically and you must code all other responses as NA.
#' @param show_levels If TRUE (the default), print the expected levels of the variable to be weighted. Your levels in your data must match these or later weighting stages will not work. If FALSE, it will not print these levels.
#' @param .stop_date Defaults to NULL - the most recent available date. Character string of the form 'YYYY QQ': the date at which to stop the partyid collection, if e.g., you want to weight for data from Q2 2023 then '2023 Q2'.

#' @export

gen_us <- function(target_variable = "options",
                   .moving_average = 1,
                   .gallup_url = "insert your gallup URL",
                   .stop_date = NULL,
                   partyid_as_ind_other = TRUE,
                   show_levels = TRUE) {

  if(target_variable == "options") {
    print(glue::glue('Your variable weighting options are...'))
    print(glue::glue('From the ACS5: "education", "education_collapse", "sex", "male", "race", "race_collapse", "age", "age_alt", "income_ces"/"income", "income_ces_2"/"income_2", "income_ces_3"/"income_3/income_ces_three", "region", "division" (division is not recommended)'))
    print(glue::glue('From Gallup: "partyid"'))
    print(glue::glue('From the GSS: "spanking", "trust", "bible", "polviews", "conmedic", "consci"'))
    print(glue::glue('From misc sources: "urban_rural_suburban"'))
    print(glue::glue('From Pew Internet Frequency Update: "intfreq", "intfreq_collapse"'))
    print(glue::glue('From the Federal Election Commission and US Elections Project: "vote_2020", "vote_2020_reduced", "vote_2020_reduced_2"'))
  }

  if(target_variable == "education") {

    education_weights <- weights::wpct(c('Less than high school', 'Graduated from high school', 'Some college, no degree', 'Graduated from college', 'Completed graduate school'),
                                       c(.116, .275, .306, .190, .112))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Less than high school', 'Graduated from high school', 'Some college, no degree', 'Graduated from college', 'Completed graduate school'), collapse = ', ')}"))
    }

    return(education_weights)

  }
  else if(target_variable == "education_collapse") {

    education_collapse_weights <- weights::wpct(c('High school or less', 'Some college, no degree', 'Graduated from college', 'Completed graduate school'),
                                                c(.391, .306, .190, .112))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('High school or less', 'Some college, no degree', 'Graduated from college', 'Completed graduate school'), collapse = ', ')}"))
    }

    return(education_collapse_weights)

  }
  else if(target_variable == "sex") {

    sex_weights <- weights::wpct(c('Male', 'Female'),
                                 c(.487, .513))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Male', 'Female'), collapse = ', ')}"))
    }

    return(sex_weights)

  }
  else if(target_variable == "male") {

    male_weights <- weights::wpct(c(.5, -.5),
                                  c(.487, .513))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c(.5, -.5), collapse = ', ')}"))
    }

    return(male_weights)

  }
  else if(target_variable == "race") {

    race_weights <- weights::wpct(c('Asian or Asian American', 'Black or African American', 'Hispanic or Latino', 'White or Caucasian', 'Other'),
                                  c(.0573, .120, .161, .630, .0313))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Asian or Asian American', 'Black or African American', 'Hispanic or Latino', 'White or Caucasian', 'Other'), collapse = ', ')}"))
    }

    return(race_weights)

  }
  else if(target_variable == "race_collapse") {

    race_collapse_weights <- weights::wpct(c('Black or African American', 'Hispanic or Latino', 'White or Caucasian', 'Other'),
                                           c(.120, .161, .630, .0886))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Black or African American', 'Hispanic or Latino', 'White or Caucasian', 'Other'), collapse = ', ')}"))
    }

    return(race_collapse_weights)

  }
  else if(target_variable == "age") {

    age_weights <- weights::wpct(c("18-24", "25-44", "45-64", "65+"),
                                 c(.120, .343, .330, .207))

    if(show_levels == TRUE) {
      print(glue::glue('Levels for {target_variable}: {paste(c("18-24", "25-44", "45-64", "65+"), collapse = "; ")}'))
    }

    return(age_weights)

  }
  else if(target_variable == "age_alt" | target_variable == "age_alternative") {

    age_alt_weights <- weights::wpct(c("18-24", "25-39", "40-54", "55+"),
                                     c(.120, .264, .243, .373))

    if(show_levels == TRUE) {
      print(glue::glue('Levels for {target_variable}: {paste(c("18-24", "25-39", "40-54", "55+"), collapse = "; ")}'))
    }

    return(age_alt_weights)

  }
  else if(target_variable == "income" | target_variable == "income_ces") {

    income_ces_weights <- weights::wpct(c('Under $20,000', 'Between $20,000 and $49,999', 'Between $50,000 and $79,999', 'Between $80,000 and $99,999', 'Between $100,000 and $150,000', 'Over $150,000'),
                                        c(.0965, .214, .203, .110, .183, .194))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Under $20,000', 'Between $20,000 and $49,999', 'Between $50,000 and $79,999', 'Between $80,000 and $99,999', 'Between $100,000 and $150,000', 'Over $150,000'), collapse = ', ')}"))
    }

    return(income_ces_weights)

  }
  else if(target_variable == "income_2" | target_variable == "income_ces_2") {

    income_ces_2_weights <- weights::wpct(c('Under 20000', 'Between 20000 and 49999', 'Between 50000 and 79999', 'Between 80000 and 99999', 'Between 100000 and 150000', 'Over 150000'),
                                          c(.0965, .214, .203, .110, .183, .194))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Under 20000', 'Between 20000 and 49999', 'Between 50000 and 79999', 'Between 80000 and 99999', 'Between 100000 and 150000', 'Over 150000'), collapse = ', ')}"))
    }

    return(income_ces_2_weights)

  }
  else if(target_variable == "income_3" | target_variable == "income_ces_3" | target_variable == "income_ces_three") {

    income_ces_three_weights <- weights::wpct(c('a', 'b', 'c', 'd', 'e', 'f'),
                                              c(.0965, .214, .203, .110, .183, .194))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('a', 'b', 'c', 'd', 'e', 'f'), collapse = ', ')}"))
    }

    return(income_ces_three_weights)

  }
  else if(target_variable == "income_hilo") {

    income_hilo_weights <- weights::wpct(c('Low', 'High'),
                                         c(.6235, .3765))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Low', 'High'), collapse = ', ')}"))
    }

    return(income_hilo_weights)

  }
  else if(target_variable == "income_new") {

    income_new_weights <- weights::wpct(c('a', 'b', 'c', 'd', 'e', 'f'),
                                        c(.0965, .214, .203, .110, .183, .194))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('a', 'b', 'c', 'd', 'e', 'f'), collapse = ', ')}"))
    }

    return(income_new_weights)

  }
  else if(target_variable == "region") {

    region_weights <- weights::wpct(c('Midwest', 'Northeast', 'South', 'West'),
                                    c(.208, .175, .379, .237))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Midwest', 'Northeast', 'South', 'West'), collapse = ', ')}"))
    }

    return(region_weights)

  }
  else if(target_variable == "division") {

    division_weights <- weights::wpct(c('East North Central', 'East South Central', 'Mid Atlantic', 'Mountain', 'New England', 'Pacific', 'South Atlantic', 'West North Central', 'West South Central'),
                                      c(.144, .0584, .128, .0740, .0470, .163, .202, .0646, .119))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('East North Central', 'East South Central', 'Mid Atlantic', 'Mountain', 'New England', 'Pacific', 'South Atlantic', 'West North Central', 'West South Central'), collapse = ', ')}"))
    }

    return(division_weights)

  }
  else if(target_variable == "trust") {

    gss_trust_2018_weights <-
      weights::wpct(c("Can trust people", "Can't be too careful"),
                    c(.331, .669))

    if(show_levels == TRUE) {
      print(glue::glue('Levels for {target_variable}: {paste(c("Can trust people", "Can\'t be too careful"), collapse = "; ")}'))
    }

    return(gss_trust_2018_weights)

  }
  else if(target_variable == "spanking") {

    gss_spanking_2021_weights <-
      weights::wpct(c('Strongly disagree','Disagree', 'Agree', 'Strongly agree'),
                    c(.170, .308, .364, .158))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Strongly disagree','Disagree', 'Agree', 'Strongly agree'), collapse = ', ')}"))
    }

    return(gss_spanking_2021_weights)

  }
  else if(target_variable == "bible") {

    gss_bible_2018_weights <-
      weights::wpct(c('Word of God', 'Inspired word', 'Book of fables'),
                    c(.310, .473, .217))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Word of God', 'Inspired word', 'Book of fables'), collapse = ', ')}"))
    }

    return(gss_bible_2018_weights)

  }
  else if(target_variable == "polviews" | target_variable == "lib_con" | target_variable == "left_right" | target_variable == "libcon") {

    gss_polviews_2021_weights <-
      weights::wpct(c('Conservative', 'Moderate', 'Liberal'),
                    c(.320, .347, .333))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Conservative', 'Moderate', 'Liberal'), collapse = ', ')}"))
    }

    return(gss_polviews_2021_weights)

  }
  else if(grepl("urban", target_variable) | grepl("rural", target_variable)) {

    urban_suburban_rural_weights <-
      weights::wpct(c('Urban', 'Suburban', 'Rural'),
                    c(.31, .55, .14))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Urban', 'Suburban', 'Rural'), collapse = ', ')}"))
    }

    return(urban_suburban_rural_weights)

  }
  else if(target_variable == "intfreq") {

    intfreq_weights <-
      weights::wpct(c('Almost constantly', 'Several times a day', 'Once a day or less', 'Less than several times a week'),
                    c(.31, .48, .1, .11))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Almost constantly', 'Several times a day', 'Once a day or less', 'Less than several times a week'), collapse = ', ')}"))
    }

    return(intfreq_weights)

  }
  else if(target_variable == "intfreq_collapse") {

    intfreq_collapse_weights <-
      weights::wpct(c('Almost constantly', 'Several times a day', 'Once a day or less'),
                    c(.31, .48, .21))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Almost constantly', 'Several times a day', 'Once a day or less'), collapse = ', ')}"))
    }

    return(intfreq_collapse_weights)

  }
  else if(target_variable == "conmedic" | grepl("medicine", target_variable)) {

    gss_conmedic_2021_weights <-
      weights::wpct(c('Hardly any', 'Only some', 'A great deal'),
                    c(.0924, .506, .402))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Hardly any', 'Only some', 'A great deal'), collapse = ', ')}"))
    }

    return(gss_conmedic_2021_weights)

  }
  else if(target_variable == "consci" | grepl("science", target_variable)) {

    gss_consci_2021_weights <-
      weights::wpct(c("Hardly any", "Only some", "A great deal"),
                    c(.0663, .430, .504))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Hardly any', 'Only some', 'A great deal'), collapse = ', ')}"))
    }

    return(gss_consci_2021_weights)

  }
  else if(target_variable == "partyid") {

    print(glue::glue("Using the get_gallup() function to retrieve the latest party identification numbers, averaging over the last {.moving_average} quarters..."))

    partyid_tibble <-
      jimbilben::get_gallup(.moving_average, gallup_url = .gallup_url, stop_date = .stop_date)

    if(partyid_as_ind_other == TRUE) {
      partyid_weights <-
        weights::wpct(c('Republican', 'Independent', 'Democrat'),
                      c(partyid_tibble[[1, "prop_rep_other"]], partyid_tibble[[1, "prop_ind_other"]], partyid_tibble[[1, "prop_dem_other"]]))
    }
    else {

      print("For partyid, make sure that anything the user did not directly specify as Republican, Independent, or Democrat, is categorised as NA.")

      partyid_weights <- # to use this, you would need to make anything other than these options an NA
        weights::wpct(c("Republican", "Independent", "Democrat"),
                      c(partyid_tibble[[1, "prop_rep"]], partyid_tibble[[1, "prop_ind"]], partyid_tibble[[1, "prop_dem"]]))
    }

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Republican', 'Independent', 'Democrat'), collapse = ', ')}"))
    }

    return(partyid_weights)

  }

  else if(target_variable == "vote_2020") {

    vote_weights <- weights::wpct(c('Biden', 'Trump', "Other", "Did not vote"),
                                  c(.342, .312, .012, .334))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Biden', 'Trump', 'Other', 'Did not vote'), collapse = ', ')}"))
      print("Remember that people who would not have been old enough to vote should not be given NA")
      "https://www.electproject.org/2020g"
      "https://www.fec.gov/resources/cms-content/documents/federalelections2020.pdf"
    }

    return(vote_weights)

  }

  else if(target_variable == "vote_2020_reduced") {

    vote_weights <- weights::wpct(c('Biden', 'Trump', "Other or Did not vote"),
                                  c(.342, .312, .346))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Biden', 'Trump', 'Other or Did not vote'), collapse = ', ')}"))
      print("Remember that people who would not have been old enough to vote should not be given NA")
      "https://www.electproject.org/2020g"
      "https://www.fec.gov/resources/cms-content/documents/federalelections2020.pdf"
    }

    return(vote_weights)

  }

  else if(target_variable == "vote_2020_reduced_2") {

    vote_weights <- weights::wpct(c('Biden', 'Trump', "Did not vote"),
                                  c(.346, .316, .338))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Biden', 'Trump', 'Other or Did not vote'), collapse = ', ')}"))
      print("Remember that people who would not have been old enough to vote should not be given NA")
      "https://www.electproject.org/2020g"
      "https://www.fec.gov/resources/cms-content/documents/federalelections2020.pdf"
    }

    return(vote_weights)

  }

}
