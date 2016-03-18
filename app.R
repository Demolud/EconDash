##Load Libraries
library(shiny)
library(shinydashboard)
library(quantmod)
library(forecast)
library(dygraphs)
library(rpivotTable)

data <- new.env()
r1start <- "2001-03-01" ##start of 1st recession within period 
r1end <- "2001-11-01" ##end of 1st recession within period 
r2start <- "2007-12-01" ##start of 2nd recession within period
r2end <- "2009-06-01" ##end of 2nd recession within period 
date.start <- "2000-01-01" ##start of period to be analyzed
date.end <- cut(Sys.Date(), "quarter") ##end of period to be analyzed, start of forecast.
date.end2 <- seq(as.Date(date.end), by = "quarter", length = 20)[20] ##end of forecast period

setInternet2(TRUE) 

tickers <- c('A191RO1Q156NBEA','HOUST','UNRATE','AEXUSEU','UMCSENT','INDPRO', 'MEHOINUSA672N','POILBREUSDM','CPIAUCSL', 'ALTSALES', 'PCE','USEHS', 'USPBS', 'USFIRE', 'USGOVT', 'USLAH', 'USINFO')
getSymbols(tickers
           , src = "FRED"  # needed!
           , from = date.start  # ignored
           , to = date.end  # ignored
           , env = data
           , adjust = TRUE
)
dtx <- data$A191RO1Q156NBEA
A <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$HOUST
B <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$UNRATE
C <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$AEXUSEU
D <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$UMCSENT
E <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$INDPRO
F <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$MEHOINUSA672N
G <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$POILBREUSDM
H <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$CPIAUCSL
I <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$ALTSALES
J <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$PCE
K <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$USEHS
L <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$USPBS
M <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$USFIRE
N <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$USGOVT
O <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$USLAH
P <- dtx[paste(date.start,date.end,sep="/")]
dtx <- data$USINFO
Q <- dtx[paste(date.start,date.end,sep="/")]


##Develop Forecast
AFCST <- predict(arima(A, order = c(3,0,0)), n.ahead = 20)
AFCST <- AFCST [ -c(2) ]
AFCST <- as.numeric(unlist(AFCST))
AFCST  <- data.frame(AFCST)
colnames(AFCST)[1] <- "A191RO1Q156NBEA"
ADF <- data.frame(A)
AALL <- rbind(ADF, AFCST)
rownames(AALL) <- NULL
AALL <- ts(AALL, start=c(2000, 01),frequency=4)

BFCST <- predict(arima(B, order = c(3,0,0)), n.ahead = 60)
BFCST <- BFCST [ -c(2) ]
BFCST <- as.numeric(unlist(BFCST))
BFCST  <- data.frame(BFCST)
colnames(BFCST)[1] <- "HOUST"
BDF <- data.frame(B)
BALL <- rbind(BDF, BFCST)
rownames(BALL) <- NULL
BALL <- ts(BALL, start=c(2000, 01),frequency=12)

CFCST <- predict(arima(C, order = c(3,0,0)), n.ahead = 60)
CFCST <- CFCST [ -c(2) ]
CFCST <- as.numeric(unlist(CFCST))
CFCST  <- data.frame(CFCST)
colnames(CFCST)[1] <- "UNRATE"
CDF <- data.frame(C)
CALL <- rbind(CDF, CFCST)
rownames(CALL) <- NULL
CALL <- ts(CALL, start=c(2000, 01),frequency=12)

DFCST <- predict(arima(D, order = c(3,0,0)), n.ahead = 5)
DFCST <- DFCST [ -c(2) ]
DFCST <- as.numeric(unlist(DFCST))
DFCST  <- data.frame(DFCST)
colnames(DFCST)[1] <- "AEXUSEU"
DDF <- data.frame(D)
DALL <- rbind(DDF, DFCST)
rownames(DALL) <- NULL
DALL <- ts(DALL, start=c(2000, 01),frequency=1)

EFCST <- predict(arima(E, order = c(3,0,0)), n.ahead = 60)
EFCST <- EFCST [ -c(2) ]
EFCST <- as.numeric(unlist(EFCST))
EFCST  <- data.frame(EFCST)
colnames(EFCST)[1] <- "UMCSENT"
EDF <- data.frame(E)
EALL <- rbind(EDF, EFCST)
rownames(EALL) <- NULL
EALL <- ts(EALL, start=c(2000, 01),frequency=12)

FFCST <- predict(arima(F, order = c(3,0,0)), n.ahead = 60)
FFCST <- EFCST [ -c(2) ]
FFCST <- as.numeric(unlist(FFCST))
FFCST  <- data.frame(FFCST)
colnames(FFCST)[1] <- "INDPRO"
FFCST$INDPRO <- FFCST$INDPRO+15 ##Adjusted to better fit history
FDF <- data.frame(F)
FALL <- rbind(FDF, FFCST)
rownames(FALL) <- NULL
FALL <- ts(FALL, start=c(2000, 01),frequency=12)

GFCST <- predict(arima(G, order = c(3,0,0)), n.ahead = 5)
GFCST <- GFCST [ -c(2) ]
GFCST <- as.numeric(unlist(GFCST))
GFCST  <- data.frame(GFCST)
colnames(GFCST)[1] <- "MEHOINUSA672N"
GDF <- data.frame(G)
GALL <- rbind(GDF, GFCST)
rownames(GALL) <- NULL
GALL <- ts(GALL, start=c(2000, 01),frequency=1)

HFCST <- predict(arima(H, order = c(3,0,0)), n.ahead = 60)
HFCST <- HFCST [ -c(2) ]
HFCST <- as.numeric(unlist(HFCST))
HFCST  <- data.frame(HFCST)
colnames(HFCST)[1] <- "POILBREUSDM"
HDF <- data.frame(H)
HALL <- rbind(HDF, HFCST)
rownames(HALL) <- NULL
HALL <- ts(HALL, start=c(2000, 01),frequency=12)

IFCST <- predict(arima(I, order = c(3,0,0)), n.ahead = 60)
IFCST <- IFCST [ -c(2) ]
IFCST <- as.numeric(unlist(IFCST))
IFCST  <- data.frame(IFCST)
colnames(IFCST)[1] <- "CPIAUCSL"
IDF <- data.frame(I)
IALL <- rbind(IDF, IFCST)
rownames(IALL) <- NULL
IALL <- ts(IALL, start=c(2000, 01),frequency=12)

JFCST <- predict(arima(J, order = c(3,0,0)), n.ahead = 60)
JFCST <- JFCST [ -c(2) ]
JFCST <- as.numeric(unlist(JFCST))
JFCST  <- data.frame(JFCST)
colnames(JFCST)[1] <- "ALTSALES"
JDF <- data.frame(J)
JALL <- rbind(JDF, JFCST)
rownames(JALL) <- NULL
JALL <- ts(JALL, start=c(2000, 01),frequency=12)

KFCST <- predict(arima(K, order = c(3,0,0)), n.ahead = 60)
KFCST <- KFCST [ -c(2) ]
KFCST <- as.numeric(unlist(KFCST))
KFCST  <- data.frame(KFCST)
colnames(KFCST)[1] <- "PCE"
KDF <- data.frame(K)
KALL <- rbind(KDF, KFCST)
rownames(KALL) <- NULL
KALL <- ts(KALL, start=c(2000, 01),frequency=12)

LFCST <- predict(arima(L, order = c(3,0,0)), n.ahead = 60)
LFCST <- LFCST [ -c(2) ]
LFCST <- as.numeric(unlist(LFCST))
LFCST  <- data.frame(LFCST)
colnames(LFCST)[1] <- "USEHS"
LDF <- data.frame(L)
LALL <- rbind(LDF, LFCST)
rownames(LALL) <- NULL
LALL <- ts(LALL, start=c(2000, 01),frequency=12)

MFCST <- predict(arima(M, order = c(3,0,0)), n.ahead = 60)
MFCST <- MFCST [ -c(2) ]
MFCST <- as.numeric(unlist(MFCST))
MFCST  <- data.frame(MFCST)
colnames(MFCST)[1] <- "USPBS"
MDF <- data.frame(M)
MALL <- rbind(MDF, MFCST)
rownames(MALL) <- NULL
MALL <- ts(MALL, start=c(2000, 01),frequency=12)

NFCST <- predict(arima(N, order = c(3,0,0)), n.ahead = 60)
NFCST <- NFCST [ -c(2) ]
NFCST <- as.numeric(unlist(NFCST))
NFCST  <- data.frame(NFCST)
colnames(NFCST)[1] <- "USFIRE"
NDF <- data.frame(N)
NALL <- rbind(NDF, NFCST)
rownames(NALL) <- NULL
NALL <- ts(NALL, start=c(2000, 01),frequency=12)

OFCST <- predict(arima(O, order = c(3,0,0)), n.ahead = 60)
OFCST <- OFCST [ -c(2) ]
OFCST <- as.numeric(unlist(OFCST))
OFCST  <- data.frame(OFCST)
colnames(OFCST)[1] <- "USGOVT"
ODF <- data.frame(O)
OALL <- rbind(ODF, OFCST)
rownames(OALL) <- NULL
OALL <- ts(OALL, start=c(2000, 01),frequency=12)

PFCST <- predict(arima(P, order = c(0,0,0)), n.ahead = 60)
PFCST <- PFCST [ -c(2) ]
PFCST <- as.numeric(unlist(PFCST))
PFCST  <- data.frame(PFCST)
colnames(PFCST)[1] <- "USLAH"
PFCST$USLAH <- PFCST$USLAH+2100 ##Adjusted to better fit history
PDF <- data.frame(P)
PALL <- rbind(PDF, PFCST)
rownames(PALL) <- NULL
PALL <- ts(PALL, start=c(2000, 01),frequency=12)

QFCST <- predict(arima(Q, order = c(3,0,0)), n.ahead = 60)
QFCST <- QFCST [ -c(2) ]
QFCST <- as.numeric(unlist(QFCST))
QFCST  <- data.frame(QFCST)
colnames(QFCST)[1] <- "USINFO"
QDF <- data.frame(Q)
QALL <- rbind(QDF, QFCST)
rownames(QALL) <- NULL
QALL <- ts(QALL, start=c(2000, 01),frequency=12)

WCW <- cbind(LALL,MALL,NALL,OALL,PALL,QALL)
colnames(WCW)[1] <- 'USEHS'
colnames(WCW)[2] <- 'USPBS'
colnames(WCW)[3] <- 'USFIRE'
colnames(WCW)[4] <- 'USGOVT'
colnames(WCW)[5] <- 'USLAH'
colnames(WCW)[6] <- 'USINFO'

all <- cbind(BALL,EALL,FALL,HALL,IALL,JALL,KALL,LALL,MALL,NALL,OALL,PALL,QALL)
date_string <- time(all)
date_num <- as.numeric(date_string)
year <- floor(date_num)
year_beginning <- as.POSIXct(paste0(year, '-01-01'))
year_end <- as.POSIXct(paste0(year+1, '-01-01'))
Date <- year_beginning + (date_num %% 1) * (year_end - year_beginning)
Date <- as.Date(Date, "%m/%d/%Y")
all <- data.frame(all)
Date <- data.frame(Date)
all <- cbind(Date, all)
all <- data.frame(all)
row.names(all) <- NULL

colnames(all)[2] <- 'HOUST'
colnames(all)[3] <- 'UMCSENT'
colnames(all)[4] <- 'INDPRO'
colnames(all)[5] <- 'POILBREUSDM'
colnames(all)[6] <- 'CPIAUCSL'
colnames(all)[7] <- 'ALTSALES'
colnames(all)[8] <- 'PCE'
colnames(all)[9] <- 'USEHS'
colnames(all)[10] <- 'USPBS'
colnames(all)[11] <- 'USFIRE'
colnames(all)[12] <- 'USGOVT'
colnames(all)[13] <- 'USLAH'
colnames(all)[14] <- 'USINFO'


pivot <- data.frame(all)

##Build UI
ui <- dashboardPage(
  dashboardHeader(title = "Economic Dashboard"
  ),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Economy", tabName = "Economy", icon = icon("th")),
      menuItem("Pivot", tabName = "Pivot", icon = icon("th"))
    )
  ),
  
  
  ## Body content
  dashboardBody(
    tabItems(
      ## First Tab Content: Economy
      tabItem(tabName = "Economy",
              box(
                title = "Real Gross Domestic Product", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph1",width="370px",height="228px")),
              box(
                title = "Housing Starts", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph2",width="370px",height="228px")),
              box(
                title = "Civilian Unemployment Rate", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph3",width="370px",height="228px")),
              box(
                title = "U.S./Euro Foreign Exchange Rate", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph4",width="370px",height="228px")),
              box(
                title = "University of Michigan Consumer Sentiment", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph5",width="370px",height="228px")),
              box(
                title = "Industrial Production Index", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph6",width="370px",height="228px")),
              box(
                title = "Real Median Household Income", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph7",width="370px",height="228px")),
              box(
                title = "Global price of Brent Crude", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph8",width="370px",height="228px")),
              box(
                title = "Consumer Price Index for All Urban Consumers", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph9",width="370px",height="228px")),
              box(
                title = "Light Weight Vehicle Sales", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph10",width="370px",height="228px")),
              box(
                title = "Personal Consumption Expenditures", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph11",width="370px",height="228px")),
              box(
                title = "White Collar Workers", status = "primary", solidHeader = TRUE,
                collapsible = TRUE, width = 3,
                dygraphOutput("dygraph12",width="370px",height="228px"))
      ),
      
      # Second tab content
      tabItem(tabName = "Pivot",
              rpivotTable(pivot, rows = "Date", cols="", aggregatorName="Sum", 
                          vals="HOUST", rendererName="Table"))
    )
  ))

##Build Server Actions
server <- function(input, output, session) {


  ##Economy
  
  output$dygraph1 <- renderDygraph({
       
    dygraph(AALL, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "Percent Change") %>% 
      dyRangeSelector()
  })
  
  output$dygraph2 <- renderDygraph({
    
    dygraph(BALL, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE, stackedGraph = TRUE)%>%
      dyLegend(width = 300)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "Thousands of Units") %>% 
      dyRangeSelector()
  })
  
  output$dygraph3 <- renderDygraph({
    
    dygraph(CALL, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "Percent") %>% 
      dyRangeSelector()
  })
  
  output$dygraph4 <- renderDygraph({
    
    
    dygraph(DALL, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "U.S. Dollars to One Euro") %>% 
      dyRangeSelector()
  })
  
  output$dygraph5 <- renderDygraph({
    
    dygraph(EALL, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "Index 1966:Q1=100") %>% 
      dyRangeSelector()
  })
  
  output$dygraph6 <- renderDygraph({
    
    dygraph(FALL, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "Index 2012=100") %>% 
      dyRangeSelector()
  })
  
  output$dygraph7 <- renderDygraph({
    
    dygraph(GALL, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "2014 CPI-U-RS") %>% 
      dyRangeSelector()
  })
  
  output$dygraph8 <- renderDygraph({
    
    dygraph(HALL, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "U.S. Dollars") %>% 
      dyRangeSelector()
  })
  
  output$dygraph9 <- renderDygraph({
    
    dygraph(IALL, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "Index 1982-1984=100") %>% 
      dyRangeSelector()
  })
  
  output$dygraph10 <- renderDygraph({
    
    dygraph(JALL, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "Millions of Units") %>% 
      dyRangeSelector()
  })
  
  output$dygraph11 <- renderDygraph({

    dygraph(KALL, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "Billions of Dollars") %>% 
      dyRangeSelector()
  })
  
  output$dygraph12 <- renderDygraph({
    
    dygraph(WCW, width = 300, group = "economy") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)%>%
      dyShading(from = r1start, to = r1end) %>%
      dyShading(from = r2start, to = r2end) %>%
      dyEvent(date.end, "Actual", labelLoc = "bottom") %>%
      dyAxis("y", label = "Thousands of Persons") %>% 
      dyRangeSelector()
  })
  

  session$onSessionEnded(function() { 
    stopApp()
    q("no") 
    
  })
  
}
shinyApp(ui = ui, server = server)