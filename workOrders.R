
#install.packages("shiny")
library("shiny")


#install.packages("plotly")
library("plotly")
#install.packages("plyr")
library("plyr")

print("hello world")

#work_o <- read.csv("C:/Users/thaninger/Desktop/Metrics/Monthly PM/Work orders 20180401.csv")
work_o <- read.csv("C:/Users/Ehren/Desktop/Trav/Work orders 20180401.csv")

summary(work_o)
colnames(work_o)

work_o1 <- work_o[,c("Work.Order.ID","Type","Site","Country","Due.Date","Engineer","Status")]

head(work_o1)

cal_year <- strtoi(format(as.Date(Sys.Date(), format="%Y-%m-%d"),"%Y"))
cal_month <- strtoi(format(as.Date(Sys.Date(), format="%Y-%m-%d"),"%m"))
cal_day <- strtoi(format(as.Date(Sys.Date(), format="%Y-%m-%d"),"%d"))


cal_quarter <- as.integer(substring(quarters.Date(as.Date(Sys.Date())),2))
VRT_q <- (cal_quarter + 2) %% 4 
if (VRT_q == 0) { VRT_q  = 4 }

quarterTransform <- function(month) {
  if (month <= 12) {q <- (2)}
  if (month <= 10) {q <- (1)}
  if (month <= 7) {q <- (4)}
  if (month <= 4) {q <- (3)}
  if (month <= 1) {q <- (2)}
  return(q)
}


start_year = cal_year 
if (cal_month < 8) {
  start_year = start_year - 1
}
start_month = 8
str(start_month)

# Aug 1 is the start of the year, can't just shift quarters, have to build custom function



#Update here
#################################
# will need to automate dates, 
# including 3 month segments for quarters


#start_date <- format(as.Date("2017-08-01","%Y%m%d"), "%Y-%m")
start_date_y <- as.Date("2017-08-01")
start_date_q <- as.Date("2018-02-01")
end_date_y <- format(as.Date(Sys.Date()))
end_date_q <- as.Date("2018-05-02")
engineers <- c("John Sousa", "John Griffis", "Cesar Tataje", "Jacob Maddox", "Daniel Holland")

#unique(work_o1$Engineer)
#start_date = as.Date("2017-08")
sapply(work_o1,class) 

work_o1$dueDate <- as.Date(work_o1$Due.Date, format = "%m/%d/%Y")
work_o1$dueYear <- as.integer(format(work_o1$dueDate, "%Y"))
work_o1$dueMonth <- as.integer(format(work_o1$dueDate, "%m"))
work_o1$dueQuarter <- as.integer(substring(quarters.Date(work_o1$dueDate),2))

#work_o1$dueVRTQuarter <- (work_o1$dueQuarter + 2) %% 4
#work_o1$dueVRTQuarter <- ifelse(work_o1$dueVRTQuarter == 0, 4, work_o1$dueVRTQuarter)

VRT_q <- (cal_quarter + 2) %% 4 
if (VRT_q == 0) { VRT_q  = 4 }

work_o1$dueDateYM <- format(as.Date(work_o1$dueDate), "%Y-%m")

work_o1$Open <- ifelse(work_o1$Status == 'Open', 1, 0)
work_o1$Closed <- ifelse(work_o1$Status == 'Closed', 1, 0)

work_o1$Engineer <- as.character(work_o1$Engineer)

head(work_o1)

#df$Month_Yr <- format(as.Date(df$Date), "%Y-%m")
###########################
###    YEARLY           ###
###########################

# Total PM's year to date, fiscal year is August 1. 
# Total PM's by engineer by August 1, closed vs open

head(work_o1$dueDate)

wo_y <- work_o1[(work_o1$dueDate > start_date_y) & (work_o1$dueDate <= end_date_y) & (work_o1$Engineer %in% (engineers)) & (work_o1$Type == 'PM') 
               ,c("Type","Country","Engineer","dueDate","Open","Closed")]



#wo = work_o1[(work_o1$dueDate > start_date_y) & (work_o1$Engineer %in% ('John Griffis')), c("Type","Country","Engineer","Due.Date","Open","Closed")]

wo_q <- work_o1[(work_o1$dueDate > start_date_q) & (work_o1$dueDate < end_date_q) & (work_o1$Engineer %in% (engineers)) & (work_o1$Type == 'PM') 
               ,c("Type","Country","Engineer","Due.Date","Open","Closed")]


wo_m <- work_o1[(work_o1$dueYear == cal_year) & (work_o1$dueMonth == cal_month) & (work_o1$Engineer %in% (engineers)) & (work_o1$Type == 'PM') 
                ,c("Type","Country","Engineer","Due.Date","Open","Closed")]


#wo_y1 <- aggregate(wo_y, by = list(wo_y$Type,wo_y$Country,wo_y$Engineer), FUN = sum, na.rm=TRUE)

sapply(wo_y1,class) 
#work_o1[work_o1$Engineer == 'John Griffis',]
#
#a <- wo_y[wo_y$Engineer == 'Daniel Holland',]

#c("Type","Country","Engineer")
wo_y1 <- ddply(wo_y, c("Engineer"), summarise,
               N    = length(Open),
               Open = sum(Open),
               Closed   = sum(Closed)
)

wo_q1 <- ddply(wo_q, c("Engineer"), summarise,
               N    = length(Open),
               Open = sum(Open),
               Closed   = sum(Closed)
)

wo_m1 <- ddply(wo_m, c("Engineer"), summarise,
               N    = length(Open),
               Open = sum(Open),
               Closed   = sum(Closed)
)

wo_y1$timeFrame <- 'Year'
wo_q1$timeFrame <- 'Quarter'
wo_m1$timeFrame <- 'Month'  

head(wo_y1)
head(wo_q1)



unique(wo_y1$Engineer)

wo_comb <- rbind(wo_y1,wo_q1,wo_m1) 
time_frames <- unique(wo_comb$timeFrame)
wo_comb

#### SHINY #######
#wo_y1$Engineer

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("event")
)

server <- function(input, output) {
  # remind me to fix this aaaaa
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    plot_ly(wo_y1, x = ~wo_y1$Engineer, y = ~Open, type = 'bar', name = 'Open') %>%
        add_trace(y= ~Closed, name = 'Closed') %>%
      add_trace(y= ~N, name = 'Total') %>%
      layout(yaxis = list(title = 'Count')) %>%
      layout(xaxis = list(title = 'Engineer'))
  })
  

  output$plot <- renderPlotly({
    plot_ly(wo_q1, x = ~wo_q1$Engineer, y = ~Open, type = 'bar', name = 'Open') %>%
      add_trace(y= ~Closed, name = 'Closed') %>%
      add_trace(y= ~N, name = 'Total') %>%
      layout(yaxis = list(title = 'Count')) %>%
      layout(xaxis = list(title = 'Engineer'))
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else "Hover on a point!"
  })
}

shinyApp(ui, server)


###########################
###        Shiny        ###
###########################

server <- function(input, output) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    wo_comb[wo_comb$timeFrame == input$time, ]
  })
  
  selectedData1 <- reactive({
    wo_comb[wo_comb$timeFrame == input$time1, ]
  })
  
  output$plot1 <- renderPlotly({
    plot_ly(selectedData(), x = ~selectedData()$Engineer, y = ~Open, type = 'bar', name = 'Open') %>%
      add_trace(y= ~Closed, name = 'Closed') %>%
      add_trace(y= ~N, name = 'Total') %>%
      layout(yaxis = list(title = 'Count')) %>%
      layout(xaxis = list(title = 'Engineer'))
  })
  
  output$plot2 <- renderPlotly({
    plot_ly(selectedData1(), x = ~selectedData1()$Engineer, y = ~Open, type = 'bar', name = 'Open') %>%
      add_trace(y= ~Closed, name = 'Closed') %>%
      add_trace(y= ~N, name = 'Total') %>%
      layout(yaxis = list(title = 'Count')) %>%
      layout(xaxis = list(title = 'Engineer'))
  })
  
}

ui <- fluidPage(
  titlePanel('Work Orders by Time Frame'),
  
  fluidRow(  
    column(3,
        wellPanel(selectInput('time', 'Time Frame', time_frames,
                selected=time_frames[1])
        )
      ),    
    column(9,
      plotlyOutput('plot1')
    )
  ),
  
  fluidRow(
    column(3,
           wellPanel(selectInput('time1', 'Time Frame', time_frames,
                                 selected=time_frames[1])
           )
    ),    
    column(9,
           plotlyOutput('plot2')
    )
  )
)

shinyApp(ui, server)

###########################
###               ###
###########################
head(iris)
###########################
###  Shiny Template     ###
###########################

server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

ui <- pageWithSidebar(
  headerPanel('Iris k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(iris)),
    selectInput('ycol', 'Y Variable', names(iris),
                selected=names(iris)[[2]]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

shinyApp(ui, server)

###########################
###               ###
###########################

###########################
###               ###
###########################


#head(work_o1)

#sort(work_o, by = "Type")

#work_o.order("Type")

#work_o2 <- work_o[order(c("Type","Country","Engineer", "Due.Date")),]


#wo_num <- work_o[,c("Type","Country","Engineer","Due.Date")]
#head(wo_num)
#wo <- aggregate(wo_num, by = list(work_o$Type,work_o$Country, work_o$Engineer), FUN = length)

#head(wo_num)
#head(wo_y)

# Total PM's year to date, fiscal year is August 1. 
# Total PM's by engineer by August 1, closed vs open

# Same for each engineer by quarter, Aug-sep-oct = Q1
#  PMs closed
#  PMs closed by engineer
#  PMs due in that period

# Same for current month open vs closed
# closed by engineer 
# due in that month
# Site is needed for this as well






