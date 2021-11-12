library(plyr)
library(tidyverse)
library(janitor)
library(lubridate)
library(plotly)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(readxl)
library(shinyBS)
library(auth0)
library(aws.s3)
library(shinydisconnect)
library(shinyjs)
library(shinyWidgets)

# ---- Loading all data ----

# Cattle Data
"./data_raw/Live Cattle Futures Data - Sheet1.csv" %>% 
  read_csv() %>% 
  clean_names() %>% 
  mutate(date = date %>% mdy()) %>% 
  mutate(across(.cols = is.character, .fns = parse_number)) -> cattle_raw_data

# Big Mac data 
"./data_raw/big-mac-historical-source-data.csv" %>% 
  read_csv() %>% 
  clean_names() -> bm_historical

"./data_raw/big-mac-source-data.csv" %>% 
  read_csv() %>% 
  clean_names() -> bm_source

bm_historical %>% 
  rbind(bm_source %>% 
          select(-gdp_dollar)) -> bm_all_data

source("bm_functions.R")

ui <- tagList(
  shinyjs::useShinyjs(),
  tags$head(
    #tags$script(JS("setTimeout(function(){history.pushState({}, 'Page Title', '/big_mac');},30000);"))
  ),
  fluidPage(
    disconnectMessage(
      text = "Your session link has expired, Please reload",
      refresh = "Reload now"),
    
    navbarPage("Business Badies",
               theme = shinytheme("cerulean"),
               # Start Input Tab ----
               tabPanel("Main",
                        fluidRow(
                          #TabSet for displaying select files and pre-computed data tab
                          column(12,wellPanel(    
                            tabsetPanel(
                              tabPanel(
                                title = "Big Mac",
                                br(),
                                fluidPage(
                                  h1("Validation"), 
                                  column(6, 
                                         plotlyOutput("big_mac_graph")
                                         ),
                                  column(6,
                                         selectInput("choose_country", label = "Choose Country", choices = unique(bm_all_data$name), 
                                                     selected = "Britain"), 
                                         uiOutput("explanation")
                                         )
                                )
                              ),
                              tabPanel(
                                "Big Mac / Cattle Futures",
                                br(),
                                fluidPage(
                                  titlePanel("Price Comparisons"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput("country", "Choose Country", choices = c("Shrey","Khetrapal"))
                                    ),
                                    mainPanel(
                                      h1("First level title"),
                                      h2("Second level title"),
                                      h3("Third level title"),
                                      h4("Fourth level title"),
                                      h5("Fifth level title"),
                                      h6("Sixth level title")
                                    )
                                  )
                                )
                              )
                            ) 
                            
                          ) #well input panel ends
                          )
                        )
                        
               )
    )
    
  )
)






server <- function(input, output, session) {
  
  print(bm_all_data)
  
  output$big_mac_graph <- renderPlotly({
  
    
    bm_index_graphs(bm_all_data) -> p
    
    p %>% ggplotly(tooltip = "name")
    
    
  })
  
  output$explanation <- renderUI({
    
    req(input$choose_country)
    
    # Calculating the current USD price 
    bm_all_data %>% 
      filter(currency_code == "USD") %>% 
      filter(date == max(date)) %>% 
      pull(local_price) -> USD_current
    
    bm_all_data %>% 
      select(date, name, currency_code, local_price, dollar_ex) %>% 
      # calculating data for the graphs 
      mutate(implied_exchange_rate = local_price/USD_current) %>% 
      mutate(difference = implied_exchange_rate-dollar_ex) %>% 
      mutate(final = difference/dollar_ex) %>% 
      # filtering dates for 2020
      filter(date == max(date)) %>% 
      mutate(final = final * 100) -> data_for_explanation
    
    data_for_explanation %>% 
      filter(name == input$choose_country) -> filtered_data
    
    if(filtered_data$final < 1){
      check_value <- "undervalued"
    } else if(filtered_data$final == 1){
      check_value <- "same"
    } else {
      check_value <- "overvalued"
    }
    h3(paste0("A big mac costs ", filtered_data$local_price, " in ", input$choose_country,"'s local currency and ", USD_current," in the United States. The implied exchange rate is ", round(filtered_data$implied_exchange_rate,2),
              ". The difference between this and the actual exchange rate is, ", round(abs(filtered_data$difference),2), ", suggests the ", input$choose_country, " is ", round(abs(filtered_data$final),2),"% ", check_value))
  })
  
}

shinyApp(ui, server)

