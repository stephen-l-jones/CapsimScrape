open_remote_driver <- function (port = 4444L, browser = "firefox", verbose = FALSE) {
  assign(
    x     = "selenium_remote_driver",
    value = RSelenium::rsDriver(port = port, browser = browser, verbose = verbose),
    envir = .GlobalEnv
  )
  get_remote_driver()
}

#' @export
close_remote_driver <- function() {
  srd <- get0("selenium_remote_driver", envir = .GlobalEnv)
  if (!is.null(srd)) {
    srd$client$close()
    pid <- srd$server$process$get_pid()
    system(paste0("Taskkill /F /T" ," /PID ", pid))
    srd$server$stop()
    remove("selenium_remote_driver", envir = .GlobalEnv)
  }
  return(0)
}

get_remote_driver <- function() {
  get0("selenium_remote_driver", envir = .GlobalEnv)
}

close_all_ports <- function() {
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
}
