#' Set colors
#'
#' Set a range of colors as objects in the environment
#'
#' @param print Logical, whether or not to print the names of the colors that have been set
#' @export

set_colors <- function(print = TRUE) {

  rp_blue <<- "#327291"
  rp_yellow <<- "#f2b831"
  light_yellow <<- "#efd394"
  light_blue <<- "#98cbe4"


  dark_blue <<- "#184f6a"
  mid_blue <<- "#5594b2"
  pale_blue <<- "#c9e7f7"

  old_mid_blue <<- "#4686a4"

  dust_pink <<- "#ffb1a6"
  light_dust_pink <<- "#ffcfc8"
  dark_dust_pink <<- "#de8579"

  dark_yellow <<- "#de9f10"

  fire_red <<- "#ea5b21"
  light_fire_red <<- "#ee703d"
  dark_fire_red <<- "#b9441e"

  light_teal <<- "#ccf9ee"
  teal <<- "#90dac8"
  dark_teal <<- "#3d8f7b"

  pink <<- "#ffc1c1"
  sky_blue <<- "#afe1ff"
  army_green <<- "#7aa277"
  golden_brown <<- "#8a6d2a"
  maroon <<- "#ba4b4b"

  republican_red <<- "#E81B23"
  democrat_blue <<- "#00AEF3"

  conservative_blue <<- "#0087dc"
  labour_red <<- "#d50000"
  libdem_yellow <<- "#FDBB30"
  reform_blue <<- "#23BBD1"



  if(print == TRUE) {
    print("rp_blue, mid_blue, old_mid_blue, light_blue, pale_blue, dark_blue")
    print("rp_yellow, dark_yellow, light_yellow")
    print("dust_pink, light_dust_pink, dark_dust_pink")
    print("fire_red, light_fire_red, dark_fire_red")
    print("light_teal, teal, dark_teal")
    print("pink, sky_blue, army_green, golden_brown, maroon")
    print("republican_red, democrat_blue")
    print("conservative_blue, labour_red, libdem_yellow, reform_blue")
  }

}
