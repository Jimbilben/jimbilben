#' Get Gallup
#'
#' Retrieve uptodate US political party affiliation numbers from Gallup.
#'
#' @param moving_average Numeric input indicating how many months to average over. Defaults to 3.
#' @param custom_url An alternative input URL in the event that the standard URL changes/does not work
#' @param stop_date Character string of the form YYYY-MM-DD: the date at which to stop the partyid collection, if e.g., you want to weight for data from December 2023 then 2023-12-01

#'
#' @export

get_gallup <- function(moving_average = 3,
                       custom_url = NULL,
                       stop_date = NULL) {

  if(is.null(custom_url)) {
    gallup_url <- "https://news.gallup.com/poll/15370/party-affiliation.aspx"
  }
  else(
    gallup_url <- custom_url
  )

  # navigate to the page where the table is
  # get a list of different elements on the site
  gallup_page_main <-
    httr::GET(gallup_url) %>%
    httr::content("text", encoding = "ISO-8859-1") %>%
    strsplit(., '[[:space:]]') %>%
    unlist()

  print(glue::glue("Step 1"))

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
    str_split(., "\\'") %>%
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
    as_tibble() %>%
    rename("og_date" = X,
           "og_Republican" = Republican,
           "og_Independent" = Independent,
           "og_Democrat" = Democrat)

  print(glue::glue("Step 5"))

  # the first row is just the %age symbol, so remove this
  # and add the specified moving average
  gallup_data <-
    gallup_data[-1, ] %>%
    mutate(Republican = RcppRoll::roll_mean(as.numeric(og_Republican), n = moving_average, align = "left", fill = NA),
           Independent = RcppRoll::roll_mean(as.numeric(og_Independent), n = moving_average, align = "left", fill = NA),
           Democrat = RcppRoll::roll_mean(as.numeric(og_Democrat), n = moving_average, align = "left", fill = NA),
           Independent_other = 100 - (Republican + Democrat),
           prop_rep = Republican / (Republican + Independent + Democrat),
           prop_ind = Independent / (Republican + Independent + Democrat),
           prop_dem = Democrat / (Republican + Independent + Democrat),
           prop_rep_other = Republican / (Republican + Independent_other + Democrat),
           prop_ind_other = Independent_other / (Republican + Independent_other + Democrat),
           prop_dem_other = Democrat / (Republican + Independent_other + Democrat),
           year = parse_number(og_date),
           month = case_when(grepl("jan", tolower(og_date)) ~ "Jan",
                             grepl("feb", tolower(og_date)) ~ "Feb",
                             grepl("mar", tolower(og_date)) ~ "Mar",
                             grepl("apr", tolower(og_date)) ~ "Apr",
                             grepl("may", tolower(og_date)) ~ "May",
                             grepl("jun", tolower(og_date)) ~ "Jun",
                             grepl("jul", tolower(og_date)) ~ "Jul",
                             grepl("aug", tolower(og_date)) ~ "Aug",
                             grepl("sep", tolower(og_date)) ~ "Sep",
                             grepl("oct", tolower(og_date)) ~ "Oct",
                             grepl("nov", tolower(og_date)) ~ "Nov",
                             grepl("dec", tolower(og_date)) ~ "Dec"),
           day = 1,
           date = lubridate::ymd(paste(year, "/", month, "/", day, sep = ""))) %>%
    dplyr::relocate(date, Republican:prop_dem_other)

  # we now want to make sure we can use older dates for weighting as well
  gallup_data <-
    if(is.null(stop_date)) {
      gallup_data
    }
  else {
    gallup_data %>%
      dplyr::filter(date <= lubridate::as_date(stop_date))
  }

  gallup_data$order <- nrow(gallup_data):1

  warning(glue::glue("Do you want to check where I got the data? I used this URL: {redirected_url}, and the csv is: {data_location}"))

  return(gallup_data)

}
