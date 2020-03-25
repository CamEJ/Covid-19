
# Set up: load libraries ----
library(shiny)
#source("helpers.R")
library(readxl)
library(data.table)
library(tidyr)
library(ggplot2)
library(dplyr)

# Source helpers ----
source("setFactorOrder.R")

# Reading in and organising dataset
# read in sheet one
data = read_excel("data/Covid-19_HSEdata.xlsx", sheet="ALL", na='NA')
data = na.omit(data) # remove rows with missing data ie NAs
# keep variables in order of input file

data$Variable <- factor(data$Variable, levels = unique(data$Variable))

# fix date format
data$date <- as.Date(data$Date, format = "%m/%d/%y")

# ensure correct ordering of dates for calculating percentage change in infection numbers
out <- data[rev(order(as.Date(data$date))),]

# assign order of variables to stay same as input
sumLong =  out[grep("Summary", out$Grouping),]
# change from long to wide format to allow choice of variables
summaryData = pivot_wider(sumLong, names_from = Variable, values_from = c("Value"))
# remove unwanted vars
summaryData = summaryData[,-c(1:2)]

# percentage change in number of cases between days using: difference/original*100
summaryData = summaryData %>%
  mutate(DailyPercentIncrease = (lag(`Total number of cases`)-`Total number of cases`)/(`Total number of cases`) * 100) 

# percentage Healthcare workers amoung infected
summaryData$PercentHCW = with(summaryData, (`Total number of healthcare workers` / `Total number of cases`) * 100)

# percentage change in number of hospitalised between days using: difference/original*100
summaryData = summaryData %>%
  mutate(HospitalisedPercentIncrease = (lag(`Total number hospitalised`)-`Total number hospitalised`)/(`Total number hospitalised`) * 100) 

# get data for gender & age plot
GenAgeData =  data[grep("Gender|Age", data$Grouping),]
#GenAgeData$Variable <- factor(GenAgeData$Variable, levels = unique(data$Variable))


# Define UI ----
ui <- fluidPage(
  titlePanel("Visualisation of HSE Covid-19 data"),
   
  sidebarLayout(
    sidebarPanel(
      helpText("Choose data from the dropdown menu to display"),
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Total number of cases", "Daily increase in cases (%)",
                              "Total number hospitalised",
                              "Total number of deaths", "Total number admitted to ICU",
                              "Total number of healthcare workers infected", 
                              "Percentage healthcare workers amoung infected",
                              "Median age of infected"),
  )),
  
  mainPanel(
    p("Using data from ",
    a("the department of health.",
      href = "https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/")),
    br(),
    br(),
    br(),
    plotOutput("plot1") ,
    br(),
    br()
  )),
  
  sidebarLayout(
    sidebarPanel( 
      helpText("Use the dropdown menu to breakdown the number of cases by age or gender"),
      selectizeInput("mpgSelect", 
                     label = "Select age or gender", 
                     choices = c("Gender of those infected" = levels(as.factor(GenAgeData$Grouping))[3],
                                  "Age of those infected" = levels(as.factor(GenAgeData$Grouping))[1],
                                  "Age of those hospitalised" = levels(as.factor(GenAgeData$Grouping))[2]), 
                     selected = "Gender", 
                     multiple=FALSE),
  ),
    mainPanel(plotOutput('coolplot', height=500))
  )

)




# Define server logic ----
server <- function(input, output) {
  output$plot1 <- renderPlot({
    ledata <- switch(input$var, 
                   "Total number of cases" = summaryData$`Total number of cases`,
                   "Daily increase in cases (%)" = summaryData$DailyPercentIncrease,
                   "Total number hospitalised" = summaryData$`Total number hospitalised`,
                   "Daily increase in numbers hospitalised (%)" = summaryData$HospitalisedPercentIncrease,
                   "Total number admitted to ICU" = summaryData$`Total number admitted to ICU`,
                   "Total number of deaths" = summaryData$`Total number of deaths`,
                   "Total number of healthcare workers infected" = summaryData$`Total number of healthcare workers`,
                   "Percentage healthcare workers amoung infected" = summaryData$PercentHCW,
                   "Median age of infected" = summaryData$`Median age`)
    
    colz <- switch(input$var, 
                   "Total number of cases" = "#011C48",
                   "Daily increase in cases (%)" = "#006CA1",
                   "Total number hospitalised" = "#009D85",
                   "Daily increase in numbers hospitalised (%)" = "#81006D",
                   "Total number admitted to ICU" = "#81006D",
                   "Total number of deaths" = "#910000",
                   "Total number of healthcare workers infected" ="#DD9AC5",
                   "Percentage healthcare workers amoung infected" = "#3DC892",
                   "Median age of infected" = "#E5BC46")
    

    ggplot(summaryData, aes(x=date, y=ledata)) +
                                geom_col(fill = colz, colour = "black")+
      labs(y="", x="Date") +theme_bw() +
      scale_x_date(breaks = scales::pretty_breaks(n = 9))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      theme(axis.text= element_text(colour="black", size=14),
            text = element_text(size=14))

    
  })
  
  mpgSubset <- reactive({
    validate(
      need(input$mpgSelect != "", 'Please choose at least one feature.')
    )
    filter(GenAgeData, Grouping %in% input$mpgSelect)
  })
  output$coolplot<-renderPlot({
   ggplot(mpgSubset(), aes(x = date, y = Value, fill=Variable)) +
      geom_col(colour="black",group="Variable") +
      theme_bw() + labs(y="Number of people infected", x="\nDate", fill="")+
      scale_x_date(breaks = scales::pretty_breaks(n = 8))+
      scale_fill_manual(values=c( "#121212","#006CA1", "#910000", "#81006D", "#DD9AC5", "#E5BC46", 
                                  "#C16C00", "#009D85",  "#011C48","#5E3519",
                                  "#121212","#006CA1", "#910000", "#81006D"))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      theme(axis.text= element_text(colour="black", size=14),
            text = element_text(size=14),
            legend.position = "top") 
  })
  
  
}


# fill=Variable)) + 
#geom_bar(stat="identity", colour="black") 

# Run the app ----
shinyApp(ui = ui, server = server)
    
    
    
    
    
    
