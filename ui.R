
# ui.R

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(highcharter)
library(shinycssloaders)

shinyUI(
  navbarPage("Airlines"
             , theme = shinythemes::shinytheme("flatly")
             , tags$li(class = "dropdown", tags$style(HTML("
                                                            @import url('//https://fonts.adobe.com/fonts/cronos?src=GoogleFonts#packages-section');
                                                            h1 {font-family: 'Cronos Pro';}")))
             , fluidRow(
               column(12, box(width = 12
                              , tabsetPanel(
                                tabPanel("Tendencias"
                                         , fluidRow(style = "height:10px")
                                         , fluidRow(column(2
                                                           , sidebarPanel(width = 12
                                                                          , fluidRow(column(12
                                                                                            , pickerInput(inputId = "airlinePicker"
                                                                                                          , label = "Select Airline: "
                                                                                                          , choices = c(unique(dtCarriers$Description),"Other Airlines")
                                                                                                          , options = list(
                                                                                                            `selected-text-format` = "count > 1"
                                                                                                            , `count-selected-text` = "{0} filtros seleccionados"
                                                                                                            , `actions-box` = TRUE
                                                                                                            , `deselect-all-text` = "Clear All"
                                                                                                            , `select-all-text` = "Select All")
                                                                                                          , multiple = TRUE
                                                                                                          , selected = c(unique(dtCarriers$Description)))
                                                                          ))
                                                                          , fluidRow(style = "height:10px")
                                                                          , fluidRow(column(12, sliderInput("yearsSlider", "Flights' Years:", 2000, 2008, c(2003,2007))))
                                                           ))
                                                    , column(10
                                                             , fluidRow(column(6, withSpinner(highchartOutput("flights_mnth"), type = 4))
                                                                        , column(6, withSpinner(highchartOutput("avgflights_mnth"), type = 4))
                                                             )
                                                             
                                                             , fluidRow(column(6, withSpinner(highchartOutput("successRate"), type = 4))
                                                                        , column(6, withSpinner(highchartOutput("delayRate"), type = 4))
                                                             )
                                                             , fluidRow(column(6, withSpinner(highchartOutput("cancelledType"), type = 4))
                                                                        , column(6, withSpinner(highchartOutput("airplaneModel"), type = 4))
                                                             )
                                                    ))
                                )
                                , tabPanel("Segmentación"
                                           , fluidRow(column(4, br(), switchInput(inputId = "Segmentacion_switch"
                                                                                      , label = "Carriers"
                                                                                      , value = TRUE
                                                                                      , onLabel = 'Top 7'
                                                                                      , offLabel = "All"
                                                                                      , onStatus = "primary"
                                                                                      , offStatus = "default"
                                                                                      , size = "default"), align = "center"))
                                           , fluidRow(column(12, withSpinner(highchartOutput("Segmentacion_plt"), type = 4)))
                                           , fluidRow(style = "height:20px")
                                           , fluidRow(column(2)
                                                      , column(8, withSpinner(tableOutput("Segmentacion_tbl"), type = 4))
                                                      , column(2))
                                )
                                , tabPanel("Proyecciones"
                                           , fluidRow(style = "height:10px")
                                           , fluidRow(column(12, pickerInput(inputId = "airlinePicker_FCs"
                                                                             , label = "Select Airline: "
                                                                             , choices = c("American Airlines Inc.", "Alaska Airlines Inc.", "Continental Air Lines Inc.", "United Air Lines Inc.","US Airways Inc.")
                                                                             , options = list(
                                                                               `selected-text-format` = "count > 1"
                                                                               , `count-selected-text` = "{0} filtros seleccionados"
                                                                               , `actions-box` = TRUE
                                                                               , `deselect-all-text` = "Clear All"
                                                                               , `select-all-text` = "Select All")
                                                                             , selected = "Alaska Airlines Inc."
                                                                             , multiple = FALSE)))
                                           , tabsetPanel(
                                             tabPanel("Flights"
                                                      , fluidRow(column(12, withSpinner(highchartOutput("FC1_plt"), type = 4)))
                                                      , fluidRow(column(12, withSpinner(highchartOutput("FC2_plt"), type = 4)))
                                                      , fluidRow(column(12, withSpinner(highchartOutput("FC3_plt"), type = 4)))
                                             )
                                             , tabPanel("Cancelled Flights"
                                                        , fluidRow(column(12, withSpinner(highchartOutput("FC4_plt"), type = 4)))
                                                        , fluidRow(column(12, withSpinner(highchartOutput("FC5_plt"), type = 4)))
                                                        , fluidRow(column(12, withSpinner(highchartOutput("FC6_plt"), type = 4)))
                                             )
                                             , tabPanel("On Time Flights"
                                                        , fluidRow(column(12, withSpinner(highchartOutput("FC7_plt"), type = 4)))
                                                        , fluidRow(column(12, withSpinner(highchartOutput("FC8_plt"), type = 4)))
                                                        , fluidRow(column(12, withSpinner(highchartOutput("FC9_plt"), type = 4)))
                                             )
                                           )
                                )
                                , tabPanel("Comparativo"
                                           , fluidRow(style = "height:10px")
                                           , fluidRow(column(12, withSpinner(highchartOutput("Comparativo_grp1", height = 600), type = 4)))
                                           , fluidRow(column(12, withSpinner(highchartOutput("Comparativo_grp2", height = 600), type = 4)))
                                           , fluidRow(column(12, withSpinner(highchartOutput("Comparativo_grp3", height = 600), type = 4)))
                                           )
                              )))
             )
  )
)
