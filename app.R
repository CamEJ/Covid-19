
# Set up: load libraries ----
library(shiny)
#source("helpers.R")
library(readxl)
library(data.table)


# Reading in and organising dataset
# read in sheet one
data = read_excel("data/Covid-19_HSEdata.xlsx", sheet="ALL", na='NA')
data = na.omit(data) # remove rows with missing data ie NAs

# fix date format
data$Variable <- factor(data$Variable, levels = unique(data$Variable))

data$date <- as.Date(data$Date,
                     format = "%m/%d/%y")

data = data[order(as.Date(data$Date, format="%d/%m/%Y")),]

# assign order of variables to stay same as input
sumLong =  data[grep("Summary", data$Grouping),]
summaryData = dcast(sumLong, date ~ Variable, value.var = "Value")
summaryData$DailyPercentIncrease = with(summaryData, 
                                        (`Total number of cases` / lag(`Total number of cases`) - 1) * 100)




# Define UI ----
ui <- fluidPage(
  titlePanel("Visualisation of HSE Covid-19 data"),
   
  sidebarLayout(
    sidebarPanel(
      helpText("Inputs for visualisation"),
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Total number of cases", "Daily increase in cases (%)",
                              "Total number hospitalised",
                              "Total number of deaths", "Total number admitted to ICU",
                              "Total number of healthcare workers infected", "Median age of infected"),
                  selected = "Percent White"),
  
  ),
  mainPanel(
    p("Using data from ",
    a("the department of health.",
      href = "https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/")),
    br(),
    br(),
    br(),
    plotOutput("plot1") 
)
))


# Define server logic ----
server <- function(input, output) {
  output$plot1 <- renderPlot({
    ledata <- switch(input$var, 
                   "Total number of cases" = summaryData$`Total number of cases`,
                   "Daily increase in cases (%)" = summaryData$DailyPercentIncrease,
                   "Total number hospitalised" = summaryData$`Total number hospitalised`,
                   "Total number admitted to ICU" = summaryData$`Total number admitted to ICU`,
                   "Total number of deaths" = summaryData$`Total number of deaths`,
                   "Total number of healthcare workers infected" = summaryData$`Total number of healthcare workers`,
                   "Median age of infected" = summaryData$`Median age`)
    
    colz <- switch(input$var, 
                   "Total number of cases" = "#011C48",
                   "Daily increase in cases (%)" = "#006CA1",
                   "Total number hospitalised" = "#009D85",
                   "Total number admitted to ICU" = "#81006D",
                   "Total number of deaths" = "#910000",
                   "Total number of healthcare workers infected" ="#DD9AC5",
                   "Median age of infected" = "#E5BC46")
    

    ggplot(summaryData, aes(x=date, y=ledata)) +
                                geom_col(fill = colz, colour = "black")+
      labs(y="", x="Date") +theme_bw() +
      scale_x_date(breaks = scales::pretty_breaks(n = 9))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      theme(axis.text= element_text(colour="black", size=14),
            text = element_text(size=14))

    
  })
  
  
}

# fill=Variable)) + 
#geom_bar(stat="identity", colour="black") 

# Run the app ----
shinyApp(ui = ui, server = server)
    
    
    
    
    
    