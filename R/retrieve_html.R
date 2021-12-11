retrieve_courier <- function (industry, round) {
  round <- as.character(round)
  resp <- list()
  for (i in industry) {
    cat(sprintf("Retrieving Capstone Courier for Industry %s...", industry[industry == i]))
    resp[[i]] <- list()
    for (r in round) {
      courier_url <- sprintf(courier_url_tmpl, r, i)
      resp[[i]][[r]] <- get_response(courier_url, i)
    }
    cat("complete.\n")
  }
  return(resp)
}

retrieve_annual <- function (industry, round) {
  round <- as.character(round)
  resp <- list()
  for (i in industry) {
    cat(sprintf("Retrieving Annual Statements for Industry %s...", industry[industry == i]))
    resp[[i]] <- list()
    companies <- read_industry_companies(i)
    company  <- companies[companies$status != "Inactive", "company"]
    for (r in round) {
      resp[[i]][[r]] <- list()
      for (m in company) {
        annual_url <- sprintf(annual_url_tmpl, r, i, m)
        resp[[i]][[r]][[m]] <- get_response(annual_url, i)
      }
    }
    cat("complete.\n")
  }
  return(resp)
}

retrieve_decision <- function (industry) {
  resp <- list()
  for (i in industry) {
    cat(sprintf("Retrieving Decision History for Industry %s...", industry[industry == i]))
    resp[[i]] <- list()
    companies <- read_industry_companies(i)
    company  <- companies[companies$status == "Participant", "company"]
    for (m in company) {
      decision_url <- sprintf(decision_url_tmpl, m)
      resp[[i]][[m]] <- get_response(decision_url, i)
    }
    cat("complete.\n")
  }
  return(resp)
}

retrieve_dashboard <- function (industry) {
  resp <- list()
  for (i in industry) {
    dashboard_url <- sprintf(redirect_url_tmpl, i)
    resp[[i]] <- get_response(dashboard_url, i)
  }
  return(resp)
}

capsim_login <- function () {
  srd <- get0("selenium_remote_driver", envir = .GlobalEnv)
  if (is.null(srd)) {
    open_remote_driver()
    srd <- get("selenium_remote_driver", envir = .GlobalEnv)
  }

  srd$client$navigate(login_url)
  cat(sprintf("Please log in from web browser (%s)...", srd$client$browserName))
  while(srd$client$getCurrentUrl() != landing_url) {
    Sys.sleep(0.5)
  }
  cat("login complete.\n")
  srd$client$getAllCookies()
}

navigate_to_industry <- function (industry) {
  srd <- get_remote_driver()
  if (is.null(srd)) {
    srd <- open_remote_driver()
  }

  industry_url <- sprintf(industry_url_tmpl, industry)
  redirect_url <- sprintf(redirect_url_tmpl, industry)
  srd$client$navigate(industry_url)
  if (!(srd$client$getCurrentUrl() %in% c(industry_url, redirect_url))) {
    capsim_login()
    srd$client$navigate(industry_url)
  }
  if (!(srd$client$getCurrentUrl() %in% c(industry_url, redirect_url)))
    stop(sprintf("Could not navigate to URL: %s", industry_url))

  set_current_industry(industry)
  cookies <- srd$client$getAllCookies()
  set_industry_cookies(cookies)
  return(cookies)
}

# get_cookies <- function (capsim_url) {
#   srd <- get_remote_driver()
#   if (is.null(srd)) {
#     srd <- open_remote_driver()
#   }
#
#   srd$client$navigate(capsim_url)
#   if (srd$client$getCurrentUrl() != capsim_url) {
#     capsim_login()
#     srd$client$navigate(capsim_url)
#   }
#   if (srd$client$getCurrentUrl() != capsim_url)
#     stop(sprintf("Could not navigate to URL: %s", capsim_url))
#
#   srd$client$getAllCookies()
# }

get_response <- function (capsim_url, industry) {
  if (industry != get_current_industry()) {
    set_current_industry(industry)
    cookies <- navigate_to_industry(industry)
  } else {
    cookies <- get_industry_cookies()
  }
  cookie_string <- paste(lapply(cookies, function(x) {
    paste0(x$name,"=",x$value)
  }), collapse = "; ")
  req <- httr2::request(capsim_url)
  if (length(cookie_string) > 0) {
    req <- httr2::req_headers(req, Cookie = cookie_string)
  }
  httr2::req_perform(req)
}
