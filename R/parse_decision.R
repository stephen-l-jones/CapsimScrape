
parse_decision <- function (resp, industry, company) {
  elem <- list(
    cmpy = c("finance","human_resources"),
    cmpy_prod = c("product")
  )
  data  <- list()

  cat(sprintf("Parsing Industry %s Company %s...", industry, company))
  html_tables <- resp %>%
    resp_body_html() %>%
    html_elements("table")

  rounds <- resp %>%
    resp_body_html() %>%
    html_elements("span") %>%
    lapply(html_text) %>%
    lapply(str_extract_all, pattern = "Round: *?[0-8]") %>%
    unlist() %>%
    str_sub(-1, -1)

  index <- 1
  for (n in seq_along(html_tables)) {
    result <- parse_decision_table(html_tables[[n]])
    if (is.null(result))
      next

    data[[index]]      <- result[[1]]
    names(data)[index] <- names(result)[1]
    index <- index + 1
  }

  index <- which(names(data) %in% c("product","finance","human_resources"))
  data[index] <- update_decision(data[index], industry, rounds, company)

  decision <- list(
    cmpy = decision_cmpy(data[names(data) %in% elem$cmpy]),
    cmpy_prod = decision_cmpy_prod(data[names(data) %in% elem$cmpy_prod])
  )

  cat("complete.\n")
  return(decision)
}

#' @import httr2 rvest stringr magrittr
parse_decision_table <- function(html_node) {
  if (!is.na(html_element(html_node, "table"))) {
    return(NULL)
  }

  # Product (cmpy_prod)
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    str_detect("ProductName") %>%
    any()
  if (isTRUE(detected)) {
    return(list(product = parse_decision_product(html_node)))
  }

  # Finance (cmpy)
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    str_detect("FinanceFunction") %>%
    any()
  if (isTRUE(detected)) {
    return(list(finance = parse_decision_finance(html_node)))
  }

  # HR (cmpy)
  detected <- html_node %>%
    html_element("tr") %>%
    html_elements("td") %>%
    html_text2() %>%
    str_detect("TrainHrs") %>%
    any()
  if (isTRUE(detected)) {
    return(list(human_resources = parse_decision_human_resources(html_node)))
  }
  return(NULL)
}

parse_decision_product <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix() %>%
    t()
  colnames(x) <- x[1,]
  x <- x[-1, , drop = FALSE]
  x <- x[!(rownames(x) == "NA"),]
  data.frame(
    product_name         = rownames(x),
    pfmn_specified       = x[, "Performance"] %>% format_to_numeric(),
    size_specified       = x[, "Size"] %>% format_to_numeric(),
    mtbf_specified       = x[, "MTBFrdSpec"] %>% format_to_numeric(),
    unit_price           = x[, "Price"] %>% format_to_numeric(),
    promo_budget         = x[, "PromoBudget"] %>% format_to_numeric(),
    sales_budget         = x[, "SalesBudget"] %>% format_to_numeric(),
    unit_sales_forecast  = x[, "UnitSalesForecast"] %>% format_to_numeric(),
    production_ordered   = x[, "ProductionOrdered"] %>% format_to_numeric(),
    capacity_change      = x[, "CapacityChange"] %>% format_to_numeric(),
    automation_next_rnd  = x[, "AutomationNextRound"] %>% format_to_numeric()
  )
}

parse_decision_finance <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix()
  x <- x[,-1, drop = FALSE]
  data.frame(
    stock_issue          = x[, "StIssue"] %>% format_to_numeric(),
    stock_retire         = x[, "StRetire"] %>% format_to_numeric(),
    dividend_per_share   = x[, "Dividend"] %>% format_to_numeric(),
    short_debt_issue     = x[, "ShortDebt"] %>% format_to_numeric(),
    bond_retire          = x[, "BondRetir"] %>% format_to_numeric(),
    bond_issue           = x[, "BondIssue"] %>% format_to_numeric(),
    ar_terms             = x[, "AR"] %>% format_to_numeric(),
    ap_terms             = x[, "AP"] %>% format_to_numeric()
  )
}

parse_decision_human_resources <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix()
  colnames(x) <- x[1,]
  x <- x[-1, , drop = FALSE]
  data.frame(
    complement_1st_shift = x[1, "Complment"] %>% format_to_numeric(),
    complement_2nd_shift = x[2, "Complment"] %>% format_to_numeric(),
    recruiting_spend     = x[1, "RecrSpend"] %>% format_to_numeric(),
    training_hours       = x[1, "TrainHrs"] %>% format_to_numeric()
  )
}

update_decision <- function(data, industry, rounds, company) {
  index <- 1
  for (r in rounds) {
    data[names(data) == "product"][[index]] <- data.frame(
      industry = industry, round = r, company = company, data[names(data) == "product"][[index]]
    )
    data[names(data) == "finance"][[index]] <- data.frame(
      industry = industry, round = r, company = company, data[names(data) == "finance"][[index]]
    )
    data[names(data) == "human_resources"][[index]] <- data.frame(
      industry = industry, round = r, company = company, data[names(data) == "human_resources"][[index]]
    )
    index <- index + 1
  }
  return(data)
}

decision_cmpy <- function (data, industry, round, company) {
  finance <- do.call(rbind, data[names(data) == "finance"])
  hr      <- do.call(rbind, data[names(data) == "human_resources"])
  cmpy    <- data.frame(finance, hr)
  rownames(cmpy) <- NULL
  return(cmpy)
}

decision_cmpy_prod <- function (data) {
  cmpy_prod <- do.call(rbind, data[names(data) == "product"])
  rownames(cmpy_prod) <- NULL
  return(cmpy_prod)
}

