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
"Live Cattle Futures Data - Sheet1.csv" %>% 
  read_csv() %>% 
  clean_names() %>% 
  mutate(date = date %>% mdy()) %>% 
  mutate(across(.cols = is.character, .fns = parse_number)) -> cattle_raw_data

# Big Mac data 
"big-mac-historical-source-data.csv" %>% 
  read_csv() %>% 
  clean_names() -> bm_historical

"big-mac-source-data.csv" %>% 
  read_csv() %>% 
  clean_names() -> bm_source

bm_historical %>% 
  select(date, local_price, currency_code) %>% 
  filter(currency_code == "USD") %>% 
  rbind(bm_source %>% 
          select(date, local_price, currency_code)) -> bm_all_data

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
  
}

shinyApp(ui, server)

