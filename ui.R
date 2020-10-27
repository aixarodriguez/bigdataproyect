library(shiny)
library(shinydashboard)

shinyUI(
    dashboardPage(
        dashboardHeader(title = "Dashboard Aerolineas"),
        dashboardSidebar(),
        dashboardBody(
            fluidPage(
                tabsetPanel(
                    tabPanel("Graficas",
                        fluidRow(style = "height:20px"),
                        fluidRow(highchartOutput("grpAvgTime")),
                        fluidRow(style = "height:20px"),
                        fluidRow(highchartOutput("grpCncll")),
                        fluidRow(style = "height:20px"),
                        fluidRow(highchartOutput("gprNmbrFlights"))
                    ),
                    tabPanel("Datos",
                             fluidRow(style = "height:20px"),
                             fluidRow(align="right",
                                 downloadButton("dwDt", label = "Descargar", width="100%",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; padding-right: 20px;")),
                             fluidRow(style = "height:20px"),
                             fluidRow(dataTableOutput('dtDatos')))
                )
            )
        )
    ))
