#' Get Gallup
#'
#' Retrieve up to date US political party affiliation numbers from Gallup. You are now required to request access to quarterly party affiliation data from Gallup, and can then enter the appropriate URL for the data. Usage of the data must involve attribution to Gallup, including when this is just used to weight survey results - see the bottom of the page here: https://news.gallup.com/poll/15370/party-affiliation.aspx
#'
#' @param moving_average Numeric input indicating how many quarters to average over. Defaults to 1.
#' @param gallup_url The input URL needed to get to the quarterly ratings - it is necessary to contact Gallup to get the URL.
#' @param stop_date Defaults to NULL, i.e., using the most recent available data. Character string of the form 'YYYY QQ': the date at which to stop the partyid collection, if e.g., you want to weight for data from Q2 2023 then '2023 Q2'

#'
#' @export

get_gallup <- function(moving_average = 1,
                       gallup_url = "you need to request the URL from gallup",
                       stop_date = NULL) {

  gallup_page_main <-
    httr::GET(gallup_url) %>%
    httr::content("text", encoding = "ISO-8859-1") %>%
    strsplit(., '[[:space:]]') %>%
    unlist()

  print("Step 1")

  # find the elements that refer to another address and refer to datawrapper
  datawrapper_locations <-
    gallup_page_main[which(grepl("datawrapper", gallup_page_main) & grepl("https", gallup_page_main))]

  print(glue::glue("Step 2"))

  # the first element is the table we are interested in getting
  # now go to that site, and get its elements
  gallup_page_with_data <-
    httr::GET(datawrapper_locations[1]) %>%
    httr::content("text", encoding = "ISO-8859-1") %>%
    strsplit(., '[[:space:]]') %>%
    unlist()

  print(glue::glue("Step 3"))

  # among these elements is the window.location.href, which is the correct link to the working
  # and most recent webpage with just the table
  # Extract the specific https part of that element call
  data_location <-
    gallup_page_with_data[which(grepl("window.location.href", gallup_page_with_data))] %>%
    strsplit(., "\\'") %>%
    unlist()

  data_page_for_warning <- data_location[2]

  # Make a GET request and capture the content
  check_response <- httr::GET(data_page_for_warning, httr::config(followlocation = FALSE))

  # Extract content as text
  check_content <- httr::content(check_response, as = "text")

  # Alternatively, capture from the window.location.href
  if (grepl("window.location.href", check_content)) {
    redirected_url <- sub(".*window.location.href='([^']+)'.*", "\\1", check_content)
  }
  else {
    redirected_url <- data_page_for_warning
  }

  print(glue::glue("Step 4"))
  print(redirected_url)
  # append the name of the dataset that the page refers to
  # and then you have go the direct location of the csv file that is used
  data_location <-
    paste(redirected_url, "dataset.csv", sep = "")

  # now read the csv file - it is \t delimited
  gallup_data <-
    read.csv(file = data_location, sep = "\t") %>%
    tibble::as_tibble() %>%
    dplyr::rename("og_date" = X,
           "og_Republican" = Republican,
           "og_Independent" = Independent,
           "og_Democrat" = Democrat)

  print(glue::glue("Step 5"))

  # the first row is just the %age symbol, so remove this
  # and add the specified moving average
  gallup_data <-
    gallup_data[-1, ] %>%
    tidyr::separate(og_date,
             into = c("year", "quarter"), sep = " ", remove = FALSE) %>%
    dplyr::mutate(Republican = RcppRoll::roll_mean(as.numeric(og_Republican), n = moving_average, align = "left", fill = NA),
           Independent = RcppRoll::roll_mean(as.numeric(og_Independent), n = moving_average, align = "left", fill = NA),
           Democrat = RcppRoll::roll_mean(as.numeric(og_Democrat), n = moving_average, align = "left", fill = NA),
           Independent_other = 100 - (Republican + Democrat),
           prop_rep = Republican / (Republican + Independent + Democrat),
           prop_ind = Independent / (Republican + Independent + Democrat),
           prop_dem = Democrat / (Republican + Independent + Democrat),
           prop_rep_other = Republican / (Republican + Independent_other + Democrat),
           prop_ind_other = Independent_other / (Republican + Independent_other + Democrat),
           prop_dem_other = Democrat / (Republican + Independent_other + Democrat),
           year = as.numeric(year),
           quarter = parse_number(quarter)) %>%
    dplyr::relocate(og_date, Republican:prop_dem_other) %>%
    dplyr::arrange(-year, -quarter)

  # we now want to make sure we can use older dates for weighting as well
  gallup_data <-
    if(is.null(stop_date)) {
      gallup_data
    } else {
      stop_row <- which(gallup_data$og_date == stop_date)
      gallup_data <-
        gallup_data[stop_row:nrow(gallup_data), ]
    }

  gallup_data <-
    gallup_data %>%
    tibble::rowid_to_column("order")

  warning(glue::glue("Do you want to check where I got the data? I used this URL: {redirected_url}, and the csv is: {data_location}"))

  return(gallup_data)

}
