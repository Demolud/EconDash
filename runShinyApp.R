library(shiny)

# .libPaths("C:/RDASH/R-Portable/App/R-Portable/library")
# you need the full path to portable chrome
browser.path = file.path("C:/RDASH/GoogleChromePortable/GoogleChromePortable.exe")
options(browser = browser.path)
shiny::runApp("C:/RDASH/shiny",port=8888,launch.browser=TRUE)

