
capsim_login <- function (remote_driver) {
  login_url   <- "https://ww2.capsim.com/login/"
  landing_url <- "https://ww5.capsim.com/pcommunity/courses"

  remote_driver$navigate(login_url)
  cat("Please log in from web browser...")

  while(remDr$getCurrentUrl() != landing_url) {
    Sys.sleep(0.5)
  }
  cat("Login complete.\n")
}
