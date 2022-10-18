# Update ------------------------------------------------------------------

# Update documentation
devtools::document()

# Load the package
devtools::load_all()
devtools::unload()

# Install the dev version
devtools::install()

# Create new function -----------------------------------------------------

usethis::use_r("test")

# Add dependency ----------------------------------------------------------
