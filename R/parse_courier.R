
parse_courier <- function (resp, industry, round) {
  elem <- list(
    main = c("year_end","prime_rate"),
    segm = c("segment_stats"),
    cmpy = c("stock_market","bond_market","cash_flow_statement","balance_sheet",
             "income_statement","human_resources"),
    cmpy_bond = c("bond_market"),
    cmpy_prod = c("production","perceptual_map"),
    prod_segm = c("segment_products","market_share")
  )
  data  <- list()

  cat(sprintf("Parsing Industry %s Round %s...", industry, round))
  html_tables <- resp %>%
    resp_body_html() %>%
    html_elements("table")

  index <- 1
  for (n in seq_along(html_tables)) {
    result <- parse_courier_table(html_tables[[n]])
    if (is.null(result))
      next

    data[[index]]      <- result[[1]]
    names(data)[index] <- names(result)[1]
    index <- index + 1
  }

  index <- which(names(data) %in% c("segment_stats","segment_products"))
  data[index] <- update_courier_segment_products(data[index])
  index <- which(names(data) %in% c("market_share_title","market_share"))
  data[index] <- update_courier_market_share(data[index])
  index <- which(names(data) %in% c("perceptual_map_company","perceptual_map"))
  data[index] <- update_courier_perceptual_map(data[index])

  courier <- list(
    main = courier_main(data[names(data) %in% elem$main], industry, round),
    segm = courier_segm(data[names(data) %in% elem$segm], industry, round),
    cmpy = courier_cmpy(data[names(data) %in% elem$cmpy], industry, round),
    cmpy_bond = courier_cmpy_bond(data[names(data) %in% elem$cmpy_bond], industry, round),
    cmpy_prod = courier_cmpy_prod(data[names(data) %in% elem$cmpy_prod], industry, round),
    prod_segm = courier_prod_segm(data[names(data) %in% elem$prod_segm], industry, round)
  )

  cat("complete.\n")
  return(courier)
}

#' @import httr2 rvest stringr magrittr
parse_courier_table <- function(html_node) {
  if (!is.na(html_element(html_node, "table"))) {
    return(NULL)
  }

  # Year end (main)
  detected <- html_node %>%
    html_element("tr") %>%
    html_elements("td") %>%
    html_text2() %>%
    .[1] %>%
    str_detect("^Round:")
  if (isTRUE(detected)) {
    return(list(year_end = parse_courier_year_end(html_node)))
  }

  # Stock market summary (cmpy)
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    str_detect("Shares") %>%
    any()
  if (isTRUE(detected)) {
    return(list(stock_market = parse_courier_stock_market(html_node)))
  }

  # Bond market summary (cmpy)
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    str_detect("Series#") %>%
    any()
  if (isTRUE(detected)) {
    return(list(bond_market = parse_courier_bond_market(html_node)))
  }

  # Prime rate (main)
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    str_detect("Prime Rate") %>%
    any()
  if (isTRUE(detected)) {
    return(list(prime_rate = parse_courier_prime_rate(html_node)))
  }

  # Cash flow statement (cmpy)
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    str_detect("Cash Flow Statement Survey") %>%
    any()
  if (isTRUE(detected)) {
    return(list(cash_flow_statement = parse_courier_cash_flow_statement(html_node)))
  }

  # Balance sheet (compy)
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    str_detect("Balance Sheet Survey") %>%
    any()
  if (isTRUE(detected)) {
    return(list(balance_sheet = parse_courier_balance_sheet(html_node)))
  }

  # Income statement (cmpy)
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    str_detect("Income Statement Survey") %>%
    any()
  if (isTRUE(detected)) {
    return(list(income_statement = parse_courier_income_statement(html_node)))
  }

  # Production (cmpy_prod)
  detected <- html_node %>%
    html_element("tr") %>%
    html_elements("td") %>%
    html_text2() %>%
    str_detect("Plant Utiliz") %>%
    any()
  if (isTRUE(detected)) {
    return(list(production = parse_courier_production(html_node)))
  }

  # Segment stats (segm)
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    str_detect("(Traditional|Low End|High End|Performance|Size) Statistics") %>%
    any()
  if (isTRUE(detected)) {
    return(list(segment_stats = parse_courier_segment_stats(html_node)))
  }

  # Segment products (prod_segm)
  detected <- html_node %>%
    html_element("tr") %>%
    html_elements("td") %>%
    html_text2() %>%
    str_detect("Units Sold to Seg") %>%
    any()
  if (isTRUE(detected)) {
    return(list(segment_products = parse_courier_segment_products(html_node)))
  }

  # Market share title
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    .[1] %>%
    str_detect("Market Share in Units")
  if (isTRUE(detected)) {
    return(list(market_share_title = parse_courier_market_share_title(html_node)))
  }

  # Market share (prod_segm)
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    str_detect("(Trad|Low|High|Pfmn|Size)") %>%
    sum() %>%
    identical(5L)
  if (isTRUE(detected)) {
    return(list(market_share = parse_courier_market_share(html_node)))
  }

  # Perceptual map company
  detected <- html_node %>%
    html_elements("th[align=\"center\"]") %>%
    html_text2() %>%
    str_detect("(Andrews|Baldwin|Chester|Digby|Erie|Ferris)") %>%
    sum %>%
    identical(1L)
  if (isTRUE(detected)) {
    return(list(perceptual_map_company = parse_courier_perceptual_map_company(html_node)))
  }

  # Perceptual map (cmpy_prod)
  detected <- html_node %>%
    html_elements("th") %>%
    html_text2() %>%
    str_detect("Revised") %>%
    any()
  if (isTRUE(detected)) {
    return(list(perceptual_map = parse_courier_perceptual_map(html_node)))
  }

  # Human resources (cmpy)
  detected <- html_node %>%
    html_elements("tr:nth-child(2)") %>%
    html_element("td") %>%
    html_text2() %>%
    str_detect("Needed Complement") %>%
    any()
  if (isTRUE(detected)) {
    return(list(human_resources = parse_courier_human_resources(html_node)))
  }
  return(NULL)
}

parse_courier_year_end <- function (html_node) {
  x <- html_node %>%
    html_element("td") %>%
    html_text2() %>%
    str_remove("Round: . ")
  c(year_end = x)
}

parse_courier_stock_market <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix()
  data.frame(
    company              = x[, "Company"],
    stock_price_close    = x[, "Close"] %>% format_to_numeric(),
    stock_price_change   = x[, "Change"] %>% format_to_numeric(),
    outstanding_shares   = x[, "Shares"] %>% format_to_numeric(),
    market_cap           = x[, "MarketCap ($M)"] %>% format_to_numeric(),
    book_val_per_share   = x[, "Book Value Per Share"] %>% format_to_numeric(),
    earnings_per_share   = x[, "EPS"] %>% format_to_numeric(),
    dividend_per_share   = x[, "Dividend"] %>% format_to_numeric(),
    dividend_yield       = x[, "Yield"] %>% format_to_numeric(),
    price_to_earnings    = x[, "P/E"] %>% format_to_numeric()
  )
}

parse_courier_bond_market <- function (html_node) {
  x <-   x <- html_node %>%
    html_elements("th,td") %>%
    sapply(html_text2) %>%
    matrix(ncol = 8, byrow = TRUE)
  x <- x[!apply(x == "&nbsp", 1, all), !apply(x == "&nbsp", 2, all)]
  colnames(x) <- x[1,]
  x <- x[-1, , drop = FALSE]
  for (i in seq_along(x[, "Company"])) {
    if (x[i, "Company"] != "&nbsp") {
      company <- x[i, "Company"]
      x[i, "Company"] <- "&nbsp"
    } else {
      x[i, "Company"] <- company
    }
  }
  x <- x[!apply(x == "&nbsp", 1, all),]
  data.frame(
    company              = x[, "Company"],
    bond_series          = x[, "Series#"],
    bond_face_value      = x[, "Face"] %>% format_to_numeric(),
    bond_yield           = x[, "Yield"] %>% format_to_numeric(),
    bond_price_close     = x[, "Close$"] %>% format_to_numeric(),
    bond_rating          = x[, "S&P"]
  )
}

parse_courier_prime_rate <- function (html_node) {
  x <- html_node %>%
    html_text2() %>%
    str_extract("([0-9]|\\.){4,5}%") %>%
    format_to_numeric
  c(prime_rate = x)
}

parse_courier_cash_flow_statement <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix() %>%
    t()
  colnames(x) <- x[1,]
  x <- x[-1, , drop = FALSE]
  data.frame(
    company              = rownames(x),
    cfs_net_income       = x[, "Net Income (Loss)"] %>% format_to_numeric(),
    cfs_depreciation     = x[, "Depreciation"] %>% format_to_numeric(),
    cfs_extra_gain_loss  = x[, "Extraordinary gains/losses/writeoffs"] %>% format_to_numeric(),
    cfs_accts_payable    = x[, "Accounts payable"] %>% format_to_numeric(),
    cfs_inventory        = x[, "Inventory"] %>% format_to_numeric(),
    cfs_accts_receivable = x[, "Accounts receivable"] %>% format_to_numeric(),
    cfs_net_cash_ops     = x[, "Net cash from operations"] %>% format_to_numeric(),
    cfs_plant_improve    = x[, "Plant improvements (net)"] %>% format_to_numeric(),
    cfs_dividends_paid   = x[, "Dividends paid"] %>% format_to_numeric(),
    cfs_sales_stock      = x[, "Sales of common stock"] %>% format_to_numeric(),
    cfs_purch_stock      = x[, "Purchase of common stock"] %>% format_to_numeric(),
    cfs_borrow_long_debt = x[, "Cash from long term debt issued"] %>% format_to_numeric(),
    cfs_retire_long_debt = x[, "Early retirement of long term debt"] %>% format_to_numeric(),
    cfs_retire_curr_debt = x[, "Retirement of current debt"] %>% format_to_numeric(),
    cfs_borrow_curr_debt = x[, "Cash from current debt borrowing"] %>% format_to_numeric(),
    cfs_emergency_loan   = x[, "Cash from emergency loan"] %>% format_to_numeric(),
    cfs_net_cash_finance = x[, "Net cash from financing activities"] %>% format_to_numeric(),
    cfs_net_change_cash  = x[, "Net change in cash position"] %>% format_to_numeric()
  )
}

parse_courier_balance_sheet <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix() %>%
    t()
  colnames(x) <- x[1,]
  x <- x[-1, , drop = FALSE]
  data.frame(
    company              = rownames(x),
    bs_cash              = x[, "Cash"] %>% format_to_numeric(),
    bs_accts_receivable  = x[, "Accounts Receivable"] %>% format_to_numeric(),
    bs_inventory         = x[, "Inventory"] %>% format_to_numeric(),
    bs_curr_assets       = x[, "Total Current Assets"] %>% format_to_numeric(),
    bs_plant_equip       = x[, "Plant and equipment"] %>% format_to_numeric(),
    bs_accu_depreciation = x[, "Accumulated Depreciation"] %>% format_to_numeric(),
    bs_fixed_assets      = x[, "Total Fixed Assets"] %>% format_to_numeric(),
    bs_total_assets      = x[, "Total Assets"] %>% format_to_numeric(),
    bs_accts_payable     = x[, "Accounts Payable"] %>% format_to_numeric(),
    bs_curr_debt         = x[, "Current Debt"] %>% format_to_numeric(),
    bs_curr_liabilities  = x[, "Total Current Liabilities"] %>% format_to_numeric(),
    bs_long_debt         = x[, "Long Term Debt"] %>% format_to_numeric(),
    bs_total_liabilities = x[, "Total Liabilities"] %>% format_to_numeric(),
    bs_stock             = x[, "Common Stock"] %>% format_to_numeric(),
    bs_retained_earnings = x[, "Retained Earnings"] %>% format_to_numeric(),
    bs_total_equity      = x[, "Total Equity"] %>% format_to_numeric()
  )
}

parse_courier_income_statement <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix() %>%
    t()
  colnames(x) <- x[1,]
  x <- x[-1, , drop = FALSE]
  data.frame(
    company              = rownames(x),
    is_sales             = x[, "Sales"] %>% format_to_numeric(),
    is_variable_cost     = x[, "Variable Costs (Labor, Material, Carry)"] %>% format_to_numeric(),
    is_contrib_margin    = x[, "Contribution Margin"] %>% format_to_numeric(),
    is_depreciation      = x[, "Depreciation"] %>% format_to_numeric(),
    is_sga               = x[, "SGA (R&D, Promo, Sales, Admin)"] %>% format_to_numeric(),
    is_other             = x[, "Other (Fees, Writeoffs, TQM, Bonuses)"] %>% format_to_numeric(),
    is_ebit              = x[, "EBIT"] %>% format_to_numeric(),
    is_interest          = x[, "Interest (Short term, Long term)"] %>% format_to_numeric(),
    is_taxes             = x[, "Taxes"] %>% format_to_numeric(),
    is_profit_sharing    = x[, "Profit Sharing"] %>% format_to_numeric(),
    is_net_profit        = x[, "Net Profit"] %>% format_to_numeric()
  )
}

parse_courier_production <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix()
  x <- x[!apply(x, 1, function(row) all(row %in% c("&nbsp","") | is.na(row))),]
  colnames(x) <- x[1,]
  x <- x[-1, , drop = FALSE]
  data.frame(
    product_name         = x[, "Name"],
    primary_segment      = x[, "Primary Segment"],
    unit_sales           = x[, "Units Sold"] %>% format_to_numeric(),
    unit_inventory       = x[, "Unit Inven tory"] %>% format_to_numeric(),
    revision_date        = x[, "Revision Date"] %>% as.Date(format = "%m/%d/%Y"),
    age_year_end         = x[, "Age Dec.31"] %>% format_to_numeric(),
    mtbf                 = x[, "MTBF"] %>% format_to_numeric(),
    pfmn_coordinate      = x[, "Pfmn Coord"] %>% format_to_numeric(),
    size_coordinate      = x[, "Size Coord"] %>% format_to_numeric(),
    unit_price           = x[, "Price"] %>% format_to_numeric(),
    material_cost        = x[, "Material Cost"] %>% format_to_numeric(),
    labor_cost           = x[, "Labor Cost"] %>% format_to_numeric(),
    contribution_margin  = x[, "Contr. Marg."] %>% format_to_numeric(),
    overtime_2nd_shift   = x[, "2nd Shift & Over-time"] %>% format_to_numeric(),
    automation_next_rnd  = x[, "Auto mation Next Round"] %>% format_to_numeric(),
    capacity_next_rnd    = x[, "Capacity Next Round"] %>% format_to_numeric(),
    plant_utilization    = x[, "Plant Utiliz."] %>% format_to_numeric()
  )
}

parse_courier_segment_stats <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix() %>%
    t()
  colnames(x) <- x[1,]
  x <- x[-1, , drop = FALSE]
  data.frame(
    segment              = rownames(x) %>% str_remove(" Statistics"),
    industry_unit_demand = x[, "Total Industry Unit Demand"] %>% format_to_numeric(),
    industry_unit_sales  = x[, "Actual Industry Unit Sales"] %>% format_to_numeric(),
    segment_pct_total    = x[, "Segment % of Total Industry"] %>% format_to_numeric()
  )
}

parse_courier_segment_products <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix()
  colnames(x) <- x[1,]
  x <- x[-1, , drop = FALSE]
  data.frame(
    product_name         = x[, "Name"],
    market_share         = x[, "Market Share"] %>% format_to_numeric(),
    unit_sales_segment   = x[, "Units Sold to Seg"] %>% format_to_numeric(),
    revision_date        = x[, "RevisionDate"] %>% as.Date(format = "%m/%d/%Y"),
    product_stock_out    = x[, "Stock Out"],
    pfmn_coordinate      = x[, "Pfmn Coord"] %>% format_to_numeric(),
    size_coordinate      = x[, "Size Coord"] %>% format_to_numeric(),
    unit_price           = x[, "ListPrice"] %>% format_to_numeric(),
    mtbf                 = x[, "MTBF"] %>% format_to_numeric(),
    age_year_end         = x[, "Age Dec.31"] %>% format_to_numeric(),
    promo_budget         = x[, "Promo Budget"] %>% format_to_numeric(),
    cust_awareness       = x[, "Cust. Aware-ness"] %>% format_to_numeric(),
    sales_budget         = x[, "Sales Budget"] %>% format_to_numeric(),
    cust_accessibility   = x[, "Cust. Access-ibility"] %>% format_to_numeric(),
    dec_cust_survey      = x[, "Dec. Cust. Survey"] %>% format_to_numeric()
  )
}

parse_courier_market_share_title <- function (html_node) {
  x <- html_node %>%
    html_text() %>%
    str_extract("(Actual|Potential)")
  c(market_share_title = x)
}

parse_courier_market_share <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix()
  x <- x[!apply(x, 1, function(row) all(row %in% c("&nbsp","") | is.na(row))),]
  x <- x[x[,1] != "Total",]
  colnames(x) <- c("Name", sapply(
    x[1,-1],
    switch,
    Trad = "Traditional", Low = "Low End", High = "High End", Pfmn = "Performance", Size = "Size"
  ))
  x <- x[-(1:3), -7, drop = FALSE]
  data.frame(
    segment              = rep(colnames(x)[-1], each = nrow(x)),
    product_name         = rep(x[,"Name"], 5),
    market_share         = as.vector(x[,-1]) %>% format_to_numeric()
  )
}

parse_courier_perceptual_map_company <- function (html_node) {
  x <- html_node %>%
    html_text()
  c(perceptual_map_company = x)
}

parse_courier_perceptual_map <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix()
  data.frame(
    product_name         = x[, "Name"],
    pfmn_coordinate      = x[, "Pfmn"] %>% format_to_numeric(),
    size_coordinate      = x[, "Size"] %>% format_to_numeric(),
    revision_date        = x[, "Revised"] %>% as.Date(format = "%m/%d/%Y")
  )
}

parse_courier_human_resources <- function (html_node) {
  x <- html_node %>%
    html_table() %>%
    as.matrix()
  colnames(x) <- x[1,]
  x <- x[-1, , drop = FALSE]
  x <- x[!apply(x, 1, function(row) all(row %in% c("&nbsp","") | is.na(row))),]
  x <- x[, !apply(x, 2, function(col) all(is.na(col)))]
  x <- x %>% t()
  colnames(x) <- x[1,]
  x <- x[-1, 1:20, drop = FALSE]
  data.frame(
    company              = rownames(x),
    needed_complement    = x[, "Needed Complement"] %>% format_to_numeric(),
    complement           = x[, "Complement"] %>% format_to_numeric(),
    employees_1st_shift  = x[, "1st Shift Complement"] %>% format_to_numeric(),
    employees_2nd_shift  = x[, "2nd Shift Complement"] %>% format_to_numeric(),
    overtime_pct         = x[, "Overtime%"] %>% format_to_numeric(),
    turnover_rate        = x[, "Turnover Rate"] %>% format_to_numeric(),
    new_employees        = x[, "New Employees"] %>% format_to_numeric(),
    separated_employees  = x[, "Separated Employees"] %>% format_to_numeric(),
    recruiting_spend     = x[, "Recruiting Spend"] %>% format_to_numeric(),
    training_hours       = x[, "Training Hours"] %>% format_to_numeric(),
    productivity_index   = x[, "Productivity Index"] %>% format_to_numeric(),
    recruiting_cost      = x[, "Recruiting Cost"] %>% format_to_numeric(),
    separation_cost      = x[, "Separation Cost"] %>% format_to_numeric(),
    training_cost        = x[, "Training Cost"] %>% format_to_numeric(),
    hr_admin_cost        = x[, "Total HR Admin Cost"] %>% format_to_numeric(),
    wage_rate            = x[, "Wages"] %>% format_to_numeric(),
    benefits             = x[, "Benefits"] %>% format_to_numeric(),
    profit_sharing       = x[, "Profit Sharing"] %>% format_to_numeric(),
    annual_raise         = x[, "Annual Raise"] %>% format_to_numeric()
  )
}

update_courier_segment_products <- function (data) {
  elem <- NA
  for (d in seq_along(data)) {
    if (names(data[d]) == "segment_stats") {
      elem <- data[[d]]
    }
    if (names(data[d]) == "segment_products") {
      data[[d]] <- data.frame(segment = unname(elem[1]), data[[d]])
      elem <- NA
    }
  }
  return(data)
}

update_courier_market_share <- function (data) {
  elem <- NA
  for (d in seq_along(data)) {
    if (names(data[d]) == "market_share_title") {
      elem <- data[[d]]
    }
    if (names(data[d]) == "market_share") {
      update_ndx <- which(names(data[[d]]) == "market_share")
      if (!is.na(elem) && elem == "Actual") {
        names(data[[d]])[update_ndx] <- paste0("market_share","_actual")
      } else if (!is.na(elem) && elem == "Potential") {
        names(data[[d]])[update_ndx] <- paste0("market_share","_potential")
      }
      elem <- NA
    }
  }
  return(data)
}

update_courier_perceptual_map <- function (data) {
  elem <- NA
  for (d in seq_along(data)) {
    if (names(data[d]) == "perceptual_map_company") {
      elem <- data[[d]]
    }
    if (names(data[d]) == "perceptual_map") {
      data[[d]] <- data.frame(company = unname(elem), data[[d]])
      elem <- NA
    }
  }
  return(data)
}

courier_main <- function (data, industry, round) {
  main <- data.frame(industry = industry, round = round, do.call(data.frame, data))
  rownames(main) <- NULL
  return(main)
}

courier_segm <- function (data, industry, round) {
  segm <- data.frame(industry = industry, round = round, do.call(rbind, data))
  rownames(segm) <- NULL
  return(segm)
}

courier_cmpy <- function (data, industry, round) {
  cmpy <- data[["income_statement"]] %>%
    merge(data[["balance_sheet"]], by = "company") %>%
    merge(data[["cash_flow_statement"]], by = "company") %>%
    merge(data[["stock_market"]], by = "company") %>%
    merge(data[["human_resources"]], by = "company")
  rownames(cmpy) <- NULL
  data.frame(industry = industry, round = round, cmpy)
}

courier_cmpy_bond <- function (data, industry, round) {
  cmpy_bond <- data[["bond_market"]]
  rownames(cmpy_bond) <- NULL
  data.frame(industry = industry, round = round, cmpy_bond)
}

courier_cmpy_prod <- function (data, industry, round) {
  index <- which(names(data) %in% "perceptual_map")
  perceptual_map <- do.call(rbind, data[index])
  cmpy_prod <- perceptual_map[, c("company","product_name")] %>%
    merge(data[["production"]], by = "product_name")
  rownames(cmpy_prod) <- NULL
  data.frame(industry = industry, round = round, cmpy_prod)
}

courier_prod_segm <- function (data, industry, round) {
  index <- which(names(data) %in% "segment_products")
  segment_products <- do.call(rbind, data[index])
  index <- which(names(data) %in% "market_share")
  prod_segm <- data[[index[1]]] %>%
    merge(data[[index[2]]], by = c("product_name","segment")) %>%
    merge(segment_products, by = c("product_name","segment"), all.x = TRUE)
  rownames(prod_segm) <- NULL
  data.frame(industry = industry, round = round, prod_segm)
}
