
format_to_numeric <- function (x) {
  x <- x %>%
    str_trim %>%
    str_remove_all("(\\$|,)")
  x[is.na(x)] <- "0"
  x[x == ""]  <- "0"
  x <- paste0(ifelse(str_detect(x, "^\\("), "-", ""), str_remove_all(x, "(\\(|\\))"))
  x <- as.numeric(str_remove(x, "\\%")) / ifelse(str_detect(x, "\\%"), 100, 1)
  return(x)
}
