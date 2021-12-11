
parse_annual <- function (resp, industry, round, company) {
  elem <- list(
    cmpy = c("income_statement"),
    cmpy_prod = c("income_statement_product")
  )
  data  <- list()

  cat(sprintf("Parsing Industry %s Round %s Company %s...", industry, round, company))
  html_tables <- resp %>%
    resp_body_html() %>%
    html_elements("table")

  index <- 1
  for (n in seq_along(html_tables)) {
    result <- parse_annual_table(html_tables[[n]])
    if (is.null(result))
      next

    data[[index]]      <- result[[1]]
    names(data)[index] <- names(result)[1]
    index <- index + 1
  }

  index <- which(names(data) %in% c("income_statement_product","income_statement"))
  data[index] <- update_annual_income_statement(data[index])

  annual <- list(
    cmpy = annual_cmpy(data[names(data) %in% elem$cmpy], industry, round, company),
    cmpy_prod = annual_cmpy_prod(data[names(data) %in% elem$cmpy_prod], industry, round, company)
  )

  cat("complete.\n")
  return(annual)
}

#' @import httr2 rvest stringr magrittr
parse_annual_table <- function(html_node) {
  if (!is.na(html_element(html_node, "table"))) {
    return(NULL)
  }

  # Income statement product (cmpy_prod)
  detected <- html_node %>%
    html_element("tr") %>%
    html_elements("td") %>%
    html_text2() %>%
    .[1] %>%
    str_detect("Product Name:")
  if (isTRUE(detected)) {
    return(list(income_statement_product = parse_annual_income_statement_product(html_node)))
  }

  # Income statement (cmpy)
  detected <- html_node %>%
    html_elements("tr:nth-child(2)") %>%
    html_element("td") %>%
    html_text2() %>%
    str_detect("^Other")
  if (isTRUE(detected)) {
    return(list(income_statement = parse_annual_income_statement(html_node)))
  }
  return(NULL)
}

parse_annual_income_statement_product <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix()
  x <- x[!apply(x, 1, function(row) all(row %in% c("&nbsp","") | is.na(row))),]
  colnames(x) <- x[1,]
  x <- x[-1, , drop = FALSE]
  x <- x[, !is.na(colnames(x))]
  index <- grep("Total$", colnames(x))
  colnames(x)[index] <- "Total"
  x <- t(x)
  colnames(x) <- x[1,]
  index <- grep("Sales", colnames(x))
  colnames(x)[index[1]] <- "Product Sales"
  x <- x[-c(1,nrow(x)), , drop = FALSE]
  data.frame(
    product_name         = rownames(x),
    is_prod_sales        = x[, "Product Sales"] %>% format_to_numeric(),
    is_prod_labor        = x[, "Direct Labor"] %>% format_to_numeric(),
    is_prod_material     = x[, "Direct Material"] %>% format_to_numeric(),
    is_prod_inv_carry    = x[, "Inventory Carry"] %>% format_to_numeric(),
    is_prod_var_cost     = x[, "Total Variable"] %>% format_to_numeric(),
    is_prod_contr_margin = x[, "Contribution Margin"] %>% format_to_numeric(),
    is_prod_depreciation = x[, "Depreciation"] %>% format_to_numeric(),
    is_prod_sga          = x[, "SG&A: R&D"] %>% format_to_numeric(),
    is_prod_promo_cost   = x[, "Promotions"] %>% format_to_numeric(),
    is_prod_sales_cost   = x[, "Sales"] %>% format_to_numeric(),
    is_prod_admin_cost   = x[, "Admin"] %>% format_to_numeric(),
    is_period_cost       = x[, "Total Period"] %>% format_to_numeric(),
    is_net_margin        = x[, "Net Margin"] %>% format_to_numeric()
  )
}

parse_annual_income_statement <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix() %>%
    t()
  x <- x[,-1]
  colnames(x) <- x[1,]
  x <- x[2, , drop = FALSE]
  data.frame(
    is_other             = x[, "Other"] %>% format_to_numeric(),
    is_ebit              = x[, "EBIT"] %>% format_to_numeric(),
    is_interest_short    = x[, "Short Term Interest"] %>% format_to_numeric(),
    is_interest_long     = x[, "LongTerm Interest"] %>% format_to_numeric(),
    is_taxes             = x[, "Taxes"] %>% format_to_numeric(),
    is_profit_sharing    = x[, "Profit Sharing"] %>% format_to_numeric(),
    is_net_profit        = x[, "Net Profit"] %>% format_to_numeric()
  )
}

update_annual_income_statement <- function (data) {
  index <- which(data[["income_statement_product"]][,"product_name"] == "Total")
  isp_total <- data[["income_statement_product"]][index[1],]
  names(isp_total) <- names(isp_total) %>%
    str_remove("_prod")
  isp_total <- isp_total[-1]
  data[["income_statement"]] <- data.frame(isp_total, data[["income_statement"]])
  data[["income_statement_product"]] <- data[["income_statement_product"]][-index[1],]
  return(data)
}

annual_cmpy <- function (data, industry, round, company) {
  cmpy <- data[["income_statement"]]
  rownames(cmpy) <- NULL
  data.frame(industry = industry, round = round, company = company, cmpy)
}

annual_cmpy_prod <- function (data, industry, round, company) {
  cmpy_prod <- data[["income_statement_product"]]
  rownames(cmpy_prod) <- NULL
  data.frame(industry = industry, round = round, company = company, cmpy_prod)
}

