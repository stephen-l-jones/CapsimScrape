login_url         <- "https://ww2.capsim.com/login/"
landing_url       <- "https://ww5.capsim.com/pcommunity/courses"
industry_url_tmpl <- "https://ww2.capsim.com/menuapp/courseMain.cfm?simid=%s"
redirect_url_tmpl <- "https://ww3.capsim.com/professor/portal/index.cfm?simid=%s&template=dashboard"
decision_url_tmpl <- "https://ww3.capsim.com/professor/portal/index.cfm?template=industryresults.decisionSummaries.History&teamname=%s&first=1"
courier_url_tmpl  <- "https://ww3.capsim.com/cgi-bin/ShowCourier.cfm?round=%s&simid=%s"
annual_url_tmpl   <- "https://ww3.capsim.com/cgi-bin/ShowCourierAnnual.cfm?round=%s&simid=%s&teamname=%s"

segm <- data.table::data.table(
  segment = c("Traditional","Low End","High End","Performance","Size"),
  segment_adjust1 = c(.23,.53,.09,.19,.09),
  segment_adjust2 = c(30,25,40,35,35)
)
