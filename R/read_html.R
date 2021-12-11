#' @export
read_courier <- function (industry, round = "0") {
  round    <- as.character(round)
  resp     <- CapsimScrape:::retrieve_courier(industry, round)
  courier  <- list()
  for (i in names(resp)) {
    courier[[i]] <- list()
    for (r in names(resp[[i]])) {
      courier[[i]][[r]] <- CapsimScrape:::parse_courier(
        resp[[i]][[r]],
        industry = i, round = r
      )
    }
  }

  table_names <- names(courier[[1]][[1]])
  courier <- courier %>%
    unlist(recursive = FALSE) %>%
    unlist(recursive = FALSE)
  courier <- lapply(table_names, function(n) {
    do.call(rbind, courier[grep(paste0("\\.",n,"$"), names(courier))])
  })
  names(courier) <- table_names
  return(courier)
}

#' @export
read_annual <- function (industry, round = "0") {
  round    <- as.character(round)
  resp     <- retrieve_annual(industry, round)
  annual   <- list()
  for (i in names(resp)) {
    annual[[i]] <- list()
    for (r in names(resp[[i]])) {
      annual[[i]][[r]] <- list()
      for (m in names(resp[[i]][[r]])) {
        annual[[i]][[r]][[m]] <- parse_annual(
          resp[[i]][[r]][[m]],
          industry = i, round = r, company = m
        )
      }
    }
  }

  table_names <- names(annual[[1]][[1]][[1]])
  annual <- annual %>%
    unlist(recursive = FALSE) %>%
    unlist(recursive = FALSE) %>%
    unlist(recursive = FALSE)
  annual <- lapply(table_names, function(n) {
    do.call(rbind, annual[grep(paste0("\\.",n,"$"), names(annual))])
  })
  names(annual) <- table_names
  return(annual)
}

#' @export
read_decision <- function (industry, round = "0") {
  round    <- as.character(round)
  resp     <- retrieve_decision(industry)
  decision <- list()
  for (i in names(resp)) {
    decision[[i]] <- list()
    for (m in names(resp[[i]])) {
      decision[[i]][[m]] <- parse_decision(
        resp[[i]][[m]],
        industry = i, company = m
      )
    }
  }

  table_names <- names(decision[[1]][[1]])
  decision <- decision %>%
    unlist(recursive = FALSE) %>%
    unlist(recursive = FALSE)
  decision <- lapply(table_names, function(n) {
    x <- do.call(rbind, decision[grep(paste0("\\.",n,"$"), names(decision))])
    x[x$round %in% round,]
  })
  names(decision) <- table_names
  return(decision)
}

#' @import rvest httr2
#' @export
read_industry_companies <- function (industry) {
  dashboard_resp <- retrieve_dashboard(industry)[[1]]
  dashboard_html <- read_html(resp_body_string(dashboard_resp))
  company_elem <- html_element(dashboard_html, "div.UIbox:nth-child(7)")
  if (html_text2(html_element(company_elem, "h2")) != "Team Activity")
    stop(sprintf("Could not find companies for industry %s.", industry))

  company_table <- html_table(company_elem)
  data.frame(company = names(company_table)[-1], status = unlist(company_table[1,-1]))
}

