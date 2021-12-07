shell.exec(paste0(getwd(), "/start-server.bat"))
remote_driver <- RSelenium::remoteDriver(browserName = "firefox", port = 4444)
remote_driver$open()
