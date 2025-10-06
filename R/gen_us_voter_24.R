#' gen_us_voter_24
#'
#' Generate a weighting target (technically, a weights::wpct() object) for a specified variable in the US population of registered voters. This is based on weighting from the 2024 CCES. Run the function without any specification to see your options.
#'
#' @param target_variable A character argument indicating for which variable to specify the weighting target. The default "options" will just print the possible variables you can choose.
#' @param partyid_as_ind_other If TRUE, partyid will be percentages based on collapsing other parties and don't know responses into Independent. If FALSE, it will work with only Democrat, Independent, and Republican specifically and you must code all other responses as NA.
#' @param show_levels If TRUE (the default), print the expected levels of the variable to be weighted. Your levels in your data must match these or later weighting stages will not work. If FALSE, it will not print these levels.

#' @export
gen_us_voter_24 <- function(target_variable = "options",
                            partyid_as_ind_other = TRUE,
                            show_levels = TRUE) {

  if(target_variable == "options") {
    print(glue::glue('Your variable weighting options are...'))
    print(glue::glue('From the 2022 Cooperative Election study: "education_collapse", "gender", "is_man", "race_collapse", "age", "age_alt", "fam_income", "fam_income_collapse, "region", "urbancity", "pew_bornagain", "partyid", "presvote_2020_three", "presvote_2020_four"'))
  }

  if(target_variable == "education_collapse") {

    education_collapse_weights <- weights::wpct(c('High school or less', 'Some college, no degree', 'Graduated from college', 'Completed graduate school'),
                                                c(.328, .292, .241, .138))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('High school or less', 'Some college, no degree', 'Graduated from college', 'Completed graduate school'), collapse = '; ')}"))
    }

    return(education_collapse_weights)

  }
  else if(target_variable == "gender") {

    gender_weights <- weights::wpct(c('Man', 'Woman', 'Other identification'),
                                    c(.465, .526, .009))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Man', 'Woman', 'Other identification'), collapse = '; ')}"))
    }

    return(gender_weights)

  }
  else if(target_variable == "gender_bin") {

    gender_bin_weights <- weights::wpct(c('Man', 'Woman'),
                                        c(.469, .531))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Man', 'Woman'), collapse = '; ')}"))
    }

    return(gender_bin_weights)

  }
  else if(target_variable == "is_man") {

    is_man_weights <- weights::wpct(c(.5, -.5),
                                    c(.465, .535))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c(.5, -.5), collapse = '; ')}"))
    }

    return(is_man_weights)

  }
  else if(target_variable == "race_collapse") {

    race_collapse_weights <- weights::wpct(c('Black or African American', 'Hispanic or Latino', 'White or Caucasian', 'Other'),
                                           c(.117, .106, .711, .066))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Black or African American', 'Hispanic or Latino', 'White or Caucasian', 'Other'), collapse = '; ')}"))
    }

    return(race_collapse_weights)

  }
  else if(target_variable == "age") {

    age_weights <- weights::wpct(c("18-24", "25-44", "45-64", "65+"),
                                 c(.058, .324, .353, .265))

    if(show_levels == TRUE) {
      print(glue::glue('Levels for {target_variable}: {paste(c("18-24", "25-44", "45-64", "65+"), collapse = "; ")}'))
    }

    return(age_weights)

  }
  else if(target_variable == "age_fine") {

    age_fine_weights <- weights::wpct(c("18-24", "25-34", "35-44", "45-64", "65+"),
                                      c(.058, .145, .179, .353, .265))

    if(show_levels == TRUE) {
      print(glue::glue('Levels for {target_variable}: {paste(c("18-24", "25-34", "35-44", "45-64", "65+"), collapse = "; ")}'))
    }

    return(age_fine_weights)

  }
  else if(target_variable == "age_alt" | target_variable == "age_alternative") {

    age_alt_weights <- weights::wpct(c("18-24", "25-39", "40-54", "55+"),
                                     c(.058, .225, .221, .496))

    if(show_levels == TRUE) {
      print(glue::glue('Levels for {target_variable}: {paste(c("18-24", "25-39", "40-54", "55+"), collapse = "; ")}'))
    }

    return(age_alt_weights)

  }
  else if(target_variable == "fam_income_collapse") {

    fam_income_collapse_weights <- weights::wpct(c('Under $20,000', 'Between $20,000 and $49,999', 'Between $50,000 and $79,999', 'Between $80,000 and $99,999', 'Between $100,000 and $149,999', 'At or above $150,000'),
                                                 c(0.118, 0.272, 0.231, 0.0955, 0.162, 0.118))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Under $20,000', 'Between $20,000 and $49,999', 'Between $50,000 and $79,999', 'Between $80,000 and $99,999', 'Between $100,000 and $149,999', 'At or above $150,000'), collapse = '; ')}"))
    }

    return(fam_income_collapse_weights)

  }
  else if(target_variable == "region") {

    region_weights <- weights::wpct(c('Midwest', 'Northeast', 'South', 'West'),
                                    c(0.222, 0.177, 0.371, .230))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Midwest', 'Northeast', 'South', 'West'), collapse = '; ')}"))
    }

    return(region_weights)

  }
  else if(grepl("urban", target_variable) | grepl("rural", target_variable) | grepl("city", target_variable) | grepl("suburb", target_variable)) {

    urbancity_weights <-
      weights::wpct(c('City', 'Rural area', 'Suburb', 'Town'),
                    c(.252, .203, .406, .140))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for urbancity: {paste(c('City', 'Rural area', 'Suburb', 'Town'), collapse = '; ')}"))
    }

    return(urbancity_weights)

  }

  else if(target_variable == "pew_bornagain" | target_variable == "evangelical") {

    pew_bornagain_weights <-
      weights::wpct(c('No', 'Yes'),
                    c(.673, .327))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('No', 'Yes'), collapse = '; ')}"))
    }

    return(pew_bornagain_weights)

  }


  else if(target_variable == "partyid") {

    if(partyid_as_ind_other == TRUE) {
      partyid_weights <-
        weights::wpct(c('Republican', 'Independent', 'Democrat'),
                      c(.313, .323, .364))
    }
    else {

      print("For partyid, make sure that anything the user did not directly specify as Republican, Independent, or Democrat, is categorised as NA.")

      partyid_weights <- # to use this, you would need to make anything other than these options an NA
        weights::wpct(c("Republican", "Independent", "Democrat"),
                      c(.318, .319, .363))
    }

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Republican', 'Independent', 'Democrat'), collapse = '; ')}"))
    }

    return(partyid_weights)

  }

  else if(target_variable == "presvote_2020_three") {

    presvote_2020_three_weights <-
      weights::wpct(c('Joe Biden', 'Donald Trump', 'Did not vote'),
                    c(.487, .436, .077))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Joe Biden', 'Donald Trump', 'Did not vote'), collapse = '; ')}"))
    }

    return(presvote_2020_three_weights)

  }

  else if(target_variable == "presvote_2020_four") {

    presvote_2020_four_weights <-
      weights::wpct(c('Joe Biden', 'Donald Trump', 'Did not vote', 'Other candidate'),
                    c(.476, .426, .075, .023))

    if(show_levels == TRUE) {
      print(glue::glue("Levels for {target_variable}: {paste(c('Joe Biden', 'Donald Trump', 'Did not vote', 'Other candidate'), collapse = '; ')}"))
    }

    return(presvote_2020_four_weights)

  }

}
