#' @import data.table
#' @export
read_capstone <- function (
  industry, round = "0", read = c("all","courier","annual","decision","none"), ...
) {

  industry <- industry
  round    <- as.character(round)
  read     <- match.arg(read)
  courier  <- list()
  annual   <- list()
  decision <- list()

  if (read %in% c("courier","all")) {
    courier <- read_courier(industry, round)
  } else if ("courier" %in% ...names()) {
    courier <- list(...)[["courier"]]
  }
  if (read %in% c("annual","all")) {
    annual <- read_annual(industry, round)
  } else if ("annual" %in% ...names()) {
    annual <- list(...)[["annual"]]
  }
  if (read %in% c("decision","all")) {
    decision <- read_decision(industry, round)
  } else if ("decision" %in% ...names()) {
    decision <- list(...)[["decision"]]
  }

  market_share <- function(industry_filter, round_filter) {
    if (missing(industry_filter)) {
      industry_filter <- industry
    }
    if (missing(round_filter)) {
      round_filter <- round
    } else {
      round_filter <- as.character(round_filter)
    }
    x <- data.table(courier$cmpy)
    x <- x[
      industry %in% industry_filter & round %in% round_filter,
      list(company, market_shr = is_sales / sum(is_sales)),
      by = list(industry,round)
    ][order(industry,company,round)]
    return(as.data.frame(x))
  }

  market_capitalization <- function(industry_filter, round_filter) {
    if (missing(industry_filter)) {
      industry_filter <- industry
    }
    if (missing(round_filter)) {
      round_filter <- round
    } else {
      round_filter <- as.character(round_filter)
    }
    x <- data.table(courier$cmpy)
    x <- x[
      industry %in% industry_filter & round %in% round_filter,
      list(market_cap = outstanding_shares * stock_price_close * 10^-3),
      keyby = list(industry,company,round)
    ]
    return(as.data.frame(x))
  }

  cumulative_profit <- function(industry_filter, round_filter) {
    if (missing(industry_filter)) {
      industry_filter <- industry
    }
    if (missing(round_filter)) {
      round_filter <- round
    } else {
      round_filter <- as.character(round_filter)
    }
    x <- data.table(courier$cmpy)
    x <- x[
      industry %in% industry_filter & round %in% round_filter,
      list(round, cum_profit = cumsum(is_net_profit)),
      keyby = list(industry,company)
    ]
    return(as.data.frame(x))
  }

  final_score <- function (
    industry_filter, end_round,
    min_score = 70, max_score = 100,
    benchmark_score = 95, benchmark_company = "Chester",
    intensity_adjustment = 0, intensity_factor = 1,
    cum_profit_weight = 50, market_shr_weight = 25, market_cap_weight = 25
  ) {
    if (missing(industry_filter)) {
      industry_filter <- industry
    }
    if (missing(end_round)) {
      end_round <- tail(round, 1)
    } else {
      end_round <- as.character(end_round)
    }
    end_round <- as.character(end_round)
    x <- data.table(courier$cmpy[, c(
      "industry","round","company","is_sales","is_net_profit","outstanding_shares","stock_price_close"
    )])
    x[, cum_profit := sum(is_net_profit), by = list(industry,company)]
    x <- x[round == end_round, list(
      company,
      cum_profit = pmax(0, cum_profit / max(cum_profit)),
      market_shr = is_sales / max(is_sales),
      market_cap = outstanding_shares * stock_price_close / max(outstanding_shares * stock_price_close)
    ), by = industry]
    x[, raw_score :=
        cum_profit * cum_profit_weight +
        market_shr * market_shr_weight +
        market_cap * market_cap_weight]
    x[, relative_score := raw_score / raw_score[company == benchmark_company], by = industry]
    x[, intensity := (mean(relative_score) + intensity_adjustment)^intensity_factor, by = industry]
    x[, adjusted_score := (relative_score - 1) * .3^(intensity^sign(1 - relative_score)) * 100 + benchmark_score]
    x[, final_score := pmin(max_score, pmax(min_score, adjusted_score, raw_score))]
    return(as.data.frame(x))
  }

  profitability <- function (industry_filter, round_filter) {
    if (missing(industry_filter)) {
      industry_filter <- industry
    }
    if (missing(round_filter)) {
      round_filter <- round
    } else {
      round_filter <- as.character(round_filter)
    }
    cols <- c("","","","","",
              "","","")
    cmpy <- setDT(courier$cmpy)[industry %in% industry_filter]
    cmpy[, ":="(
        sales   = is_sales,
        cogs    = is_variable_cost,
        sga     = is_sga + is_depreciation,
        nopat   = is_ebit * (1 - is_taxes / (is_ebit - is_interest)),
        ex_cash = excess_cash(bs_cash, bs_curr_liabilities, bs_curr_assets)
      )]
    cmpy[, ":="(
      avg_ic   = ((bs_total_assets - bs_accts_payable - ex_cash)
                  + (shift(bs_total_assets) - shift(bs_accts_payable) - shift(ex_cash))) / 2,
      avg_nwc  = ((bs_curr_assets - bs_accts_payable - ex_cash)
                  + (shift(bs_curr_assets) - shift(bs_accts_payable) - shift(ex_cash))) / 2,
      avg_ppe  = (bs_fixed_assets + shift(bs_fixed_assets)) / 2
    ), by = list(industry, company)]
    cmpy[, ":="(
      roic     = nopat / avg_ic,
      ros      = nopat / sales,
      cap_turn = sales / avg_ic,
      cogs_over_sales = cogs / sales,
      sga_over_sales  = sga / sales,
      nwc_over_sales  = avg_nwc / sales,
      ppe_over_sales  = avg_ppe / sales
    )]
    p <- cmpy[round != min(round) & round %in% round_filter, list(
      industry, round, company,
      sales, cogs, sga, nopat, ex_cash, avg_ic, avg_nwc, avg_ppe, roic, ros,
      cap_turn, cogs_over_sales, sga_over_sales, nwc_over_sales, ppe_over_sales
    )]

    return(as.data.frame(p))
  }

  value_composition <- function (
    industry_filter, round_filter, segment_level = FALSE, profit_type = c("net","nopat","operating")
  ) {
    if (missing(industry_filter)) {
      industry_filter <- industry
    }
    if (missing(round_filter)) {
      round_filter <- round
    } else {
      round_filter <- as.character(round_filter)
    }
    profit_type <- match.arg(profit_type)
    cmpy_prod_segm <- merge(
      merge(
        merge(
          setDT(courier$cmpy)[industry %in% industry_filter],
          setDT(courier$cmpy_prod),
          by = c("industry","round","company")
        ),
        setDT(courier$prod_segm)[!is.na(unit_sales_segment) & unit_sales_segment > 0, list(
          industry, round, product_name, segment, unit_sales_segment, dec_cust_survey,
          market_share_potential
        )],
        by = c("industry","round","product_name")
      ),
      segm,
      by = "segment"
    )
    cmpy_prod_segm[, ":="(
      prior_dec_cust_survey = prior_round(dec_cust_survey, round)
    ), by = list(industry, company, product_name, segment)]
    cmpy_prod_segm[, ":="(
      # value = ((segment_adjust2 + (prior_round(dec_cust_survey, round) + dec_cust_survey) / 2)
      #          * (1 - segment_adjust1)
      #          + (unit_price + .5 * as.numeric(round)) * segment_adjust1) * unit_sales_segment
      value = ((unit_price + .5 * as.numeric(round)) +
                 market_share_potential *
                 sum((dec_cust_survey + prior_dec_cust_survey)/2)) * unit_sales_segment
    ), by = list(industry, round, segment)]

    if (!segment) {
      cmpy_prod <- cmpy_prod_segm[, list(
        value         = sum(value, na.rm = TRUE),
        is_sales      = is_sales[1],
        profit        = switch(
          profit_type,
          operating = is_ebit[1],
          nopat     = (is_ebit * (1 - is_taxes / (is_ebit - is_interest)))[1],
          net       = is_net_profit[1]
        ),
        unit_sales    = unit_sales[1]
      ), by = list(industry, round, company, product_name)]
      cmpy <- cmpy_prod[, list(
        V = sum(value) / sum(unit_sales),
        P = is_sales[1] / sum(unit_sales),
        C = (is_sales[1] - profit[1]) / sum(unit_sales),
        d = sum(unit_sales)
      ), by = list(industry, round, company)]
      return(as.data.frame(cmpy[round %in% round_filter]))

    } else {
      cmpy_prod_segm <- merge(
        cmpy_prod_segm,
        setDT(annual$cmpy_prod),
        by = c("industry","round","company","product_name")
      )
      cmpy_prod_segm[, ":="(
        segment_sales = unit_price * unit_sales_segment,
        dir_costs     = (is_prod_sales - is_net_margin) * unit_sales_segment / sum(unit_sales_segment)
      ), by = list(industry, round, company, product_name)]
      cmpy_segm <- cmpy_prod_segm[, list(
        value              = sum(value),
        segment_sales      = sum(segment_sales),
        dir_costs          = sum(dir_costs),
        is_ebit            = is_ebit[1],
        is_taxes           = is_taxes[1],
        is_interest        = is_interest[1],
        is_net_profit      = is_net_profit[1],
        unit_sales_segment = sum(unit_sales_segment)
      ), by = list(industry, round, company, segment)]
      cmpy_segm[, ":="(
        ovh_costs = switch(
          profit_type,
          operating = rep(0, .N),
          nopat     = is_ebit * is_taxes / (is_ebit - is_interest) * segment_sales / sum(segment_sales),
          net       = (is_ebit - is_net_profit) * segment_sales / sum(segment_sales)
        )
      ), by = list(industry, round)]
      cmpy_segm <- cmpy_segm[, list(
        industry, round, company, segment,
        V = value / unit_sales_segment,
        P = segment_sales / unit_sales_segment,
        C = (dir_costs + ovh_costs) / unit_sales_segment,
        d = unit_sales_segment
      )]
      return(as.data.frame(cmpy_segm[round %in% round_filter]))
    }
  }

  capstone <- list(
    industry          = industry,
    round             = round,
    courier           = courier,
    annual            = annual,
    decision          = decision,
    cumulative_profit = cumulative_profit,
    market_share      = market_share,
    market_capitalization = market_capitalization,
    final_score       = final_score,
    profitability     = profitability,
    value_composition = value_composition
  )

  return(capstone)
}

prior_round <- function (x, round) {
  y <- x[match(as.character(as.numeric(round) - 1), x)]
  ifelse(is.na(y) & round == "0", x, ifelse(is.na(y), 0, y))
}

excess_cash <- function (cash, curr_liabil, curr_assets) {
  pmax(0, cash - pmax(0, curr_liabil - curr_assets + cash))
}

