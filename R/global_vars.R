
get_current_industry <- function() {
  get0("current_industry", envir = .GlobalEnv, ifnotfound = "0")
}

set_current_industry <- function(industry) {
  assign("current_industry", industry, envir = .GlobalEnv)
}

get_industry_cookies <- function() {
  get0("industry_cookies", envir = .GlobalEnv)
}

set_industry_cookies <- function(cookies) {
  assign("industry_cookies", cookies, envir = .GlobalEnv)
}
