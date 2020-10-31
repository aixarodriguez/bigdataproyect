# server.R

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(highcharter)
library(shinycssloaders)
library(wesanderson)
library(dplyr)
library(lubridate)
library(forecast)
library(tidyverse)
library(kableExtra)

shinyServer(function(input, output, session) {
  
  RV <- reactiveValues(bs_dt = dtRsm)
  
  DT_Airlines <- reactive({
    DF <- RV$bs_dt %>%
      filter( if (!("Other Airlines" %in% c(input$airlinePicker))) Description %in% c(input$airlinePicker) 
              else Description %in% unique(unique(RV$bs_dt$Description),input$airlinePicker[-pmatch("Other Airlines",input$airlinePicker)])) %>%
      filter( Year >= min(input$yearsSlider) ) %>% 
      filter( Year <= max(input$yearsSlider) ) %>%
      filter( Year != 2001)
    
    return(DF)
  })
  
  output$flights_mnth <- renderHighchart({
    
    DF <- DT_Airlines() %>%
      group_by(Year, Month) %>%
      summarize(flightsCant = sum(FlightsNum))
    
    number <- length(unique(DF$Year))
    pal <- wes_palette("BottleRocket2", number+7, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %B}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(paste("1900",DF$Month, "01")),
                                      y = DF$flightsCant,
                                      year = DF$Year),
                    # estado = forcats::fct_reorder(DF$Month, as.numeric(DF$Period), .desc = FALSE)),  
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%B', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.0f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%B'))%>%
      hc_title(text = "Flights per Year") %>%
      hc_subtitle(text = "Tendencia Mensual") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  output$avgflights_mnth <- renderHighchart({
    
    DF <- DT_Airlines() %>%
      mutate(Airline = case_when(
        Description %in% c(unique(dtCarriers$Description)) ~ Description
        , !(Description %in% c(unique(dtCarriers$Description))) ~ "Other Airlines"
      ) ) %>%
      group_by(Year, Airline) %>%
      summarize(flightsCant = sum(FlightsNum)/12)
    
    number <- length(unique(DF$Airline))
    pal <- wes_palette("BottleRocket2", number+7, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(paste(DF$Year,"01", "01")),
                                      y = DF$flightsCant,
                                      year = DF$Airline),
                    # estado = forcats::fct_reorder(DF$Month, as.numeric(DF$Period), .desc = FALSE)),  
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.0f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%Y'))%>%
      hc_title(text = "Average Flights per Month") %>%
      hc_subtitle(text = "Tendencia Anual") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  output$successRate <- renderHighchart({
    
    DF <- DT_Airlines() %>%
      group_by(Year, Cancelled) %>%
      summarize(flightsCant = sum(FlightsNum)) %>%
      mutate(Status = case_when(
        Cancelled == 0 ~ "Exitoso"
        , Cancelled == 1 ~ "Cancelado"
      )
      , Pct = round((flightsCant / sum(flightsCant))*100,1))
    
    number <- length(unique(DF$Cancelled))
    pal <- wes_palette("BottleRocket2", number+1, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "column", 
                    data = data.frame(x = ymd(paste(DF$Year,"01", "01")),
                                      y = DF$Pct,
                                      year = DF$Status),
                    # estado = forcats::fct_reorder(DF$Month, as.numeric(DF$Period), .desc = FALSE)),  
                    hcaes(x, y, group=year)) %>%
      hc_plotOptions(series = list(stacking = "percent")) %>%
      hc_legend(reversed = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.1f}%<b> <br>', 
                 dateTimeLabelFormats = list(day = '%Y'))%>%
      hc_title(text = "Flights Success Rate") %>%
      hc_subtitle(text = "Tendencia Anual") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  output$delayRate <- renderHighchart({
    
    DF <- DT_Airlines() %>%
      group_by(Year, TPArrival) %>%
      summarize(flightsCant = sum(FlightsNum)) %>%
      mutate(ArrivalType = case_when(
        TPArrival == "LATE ARRIVAL" ~ "Delayed"
        , TRUE ~ "On Time"
      )) %>%
      group_by(Year, ArrivalType) %>%
      summarize(flights = sum(flightsCant)) %>%
      mutate(Pct = round((flights / sum(flights))*100,1))
    
    number <- length(unique(DF$ArrivalType))
    pal <- wes_palette("BottleRocket2", number+1, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "column", 
                    data = data.frame(x = ymd(paste(DF$Year,"01", "01")),
                                      y = DF$Pct,
                                      year = DF$ArrivalType),
                    hcaes(x, y, group=year)) %>%
      hc_plotOptions(series = list(stacking = "percent")) %>%
      hc_legend(reversed = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.1f}%<b> <br>', 
                 dateTimeLabelFormats = list(day = '%Y'))%>%
      hc_title(text = "Flights Arrival Rate") %>%
      hc_subtitle(text = "Tendencia Anual") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  output$cancelledType <- renderHighchart({
    
    DF <- DT_Airlines() %>%
      filter(Cancelled == 1) %>%
      mutate(Status = case_when(
        CancellationCode == "A" ~ "Carrier"
        , CancellationCode == "B" ~ "Weather"
        , CancellationCode == "C" ~ "NAS"
        , CancellationCode == "D" ~ "Security"
        , TRUE ~ "TBD"
      )) %>%
      group_by(Year, Status) %>%
      summarize(flightsCant = sum(FlightsNum)) %>%
      mutate(Pct = round((flightsCant / sum(flightsCant))*100,1))
    
    number <- length(unique(DF$Status))
    pal <- wes_palette("BottleRocket2", number+4, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_yAxis(max=100)%>%
      hc_add_series(color=pal[number:1], type = "bar", 
                    data = data.frame(x = ymd(paste(DF$Year,"01", "01")),
                                      y = DF$Pct,
                                      year = DF$Status),
                    hcaes(x, y, group=year)) %>%
      hc_plotOptions(series = list(stacking = "normal")) %>%
      hc_legend(reversed = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.1f}%<b> <br>', 
                 dateTimeLabelFormats = list(day = '%Y'))%>%
      hc_title(text = "Cancelled Flights by Cancellation Code") %>%
      hc_subtitle(text = "Tendencia Anual") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  output$airplaneModel <- renderHighchart({
    
    DF <- DT_Airlines() %>%
      mutate(ModelInterval = case_when(
        is.na(PlaneYear) == TRUE ~ "TBD"
        , (PlaneYear <= 1970) ~ " - 1970"
        , (PlaneYear > 1970 & PlaneYear <= 1980) ~ "1971 - 1980"
        , (PlaneYear > 1980 & PlaneYear <= 1990) ~ "1981 - 1990"
        , (PlaneYear > 1990 & PlaneYear <= 2000) ~ "1991 - 2000"
        , (PlaneYear > 2000) ~ "2001 - 2008"
      )) %>%
      group_by(Year, ModelInterval) %>%
      summarize(flightsCant = sum(FlightsNum)) %>%
      mutate(Pct = round((flightsCant / sum(flightsCant))*100,1))
    
    number <- length(unique(DF$ModelInterval))
    pal <- wes_palette("BottleRocket2", number+4, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_yAxis(max=100)%>%
      hc_add_series(color=pal[number:1], type = "bar", 
                    data = data.frame(x = ymd(paste(DF$Year,"01", "01")),
                                      y = DF$Pct,
                                      year = DF$ModelInterval),
                    hcaes(x, y, group=year)) %>%
      hc_plotOptions(series = list(stacking = "normal")) %>%
      hc_legend(reversed = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.1f}%<b> <br>', 
                 dateTimeLabelFormats = list(day = '%Y'))%>%
      hc_title(text = "Flights per Plane Model") %>%
      hc_subtitle(text = "Tendencia Anual") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  
  output$Segmentacion_plt <- renderHighchart({
    
    DF <- segmentacion %>% 
      filter( if ( input$Segmentacion_switch == TRUE) Description %in% dtCarriers$Description
              else Description %in% unique(RV$bs_dt$Description))
    
    datos1_df <- DF %>% filter(X.KM.K.medias == 1) %>% select(Description, rateCancelled, rateLate, rateOnTime)
    datos2_df <- DF %>% filter(X.KM.K.medias == 2) %>% select(Description, rateCancelled, rateLate, rateOnTime)
    datos3_df <- DF %>% filter(X.KM.K.medias == 3) %>% select(Description, rateCancelled, rateLate, rateOnTime)
    
    pal <- wes_palette("BottleRocket2", 5, "continuous")
    highchart() %>%
      hc_chart(height = 400, renderTo = 'container', margin = 100, type = "scatter3d", animation = FALSE
               , options3d = list(enabled = TRUE, alpha = 20, beta = 30, depth = 300, viewDistance = 5, fitToPlot = FALSE
                                  , frame = list(bottom = list(size = 1, color = 'rgba(0,0,0,0.02)')
                                                 , back = list(size = 1, color = 'rgba(0,0,0,0.04)')
                                                 , side = list(size = 1, color = 'rgba(0,0,0,0.06)')
                                  ))) %>%
      hc_xAxis(min = 0, max = 0.3, gridLineWidth = 1) %>%
      hc_yAxis(min = 0, max = 1, title = NULL) %>%
      hc_zAxis(min = 0, max = 1, showFirstLabel = FALSE) %>%
      hc_add_series(name = 'Tipo A', color = pal[3], accessibility = list(exposeAsGroupOnly = TRUE), data = data.frame(x=datos1_df$rateCancelled, y=datos1_df$rateLate, z=datos1_df$rateOnTime, desc=datos1_df$Description)) %>%
      hc_add_series(name = 'Tipo B', color = pal[1], accessibility = list(exposeAsGroupOnly = TRUE), data = data.frame(x=datos2_df$rateCancelled, y=datos2_df$rateLate, z=datos2_df$rateOnTime, desc=datos2_df$Description)) %>%
      hc_add_series(name = 'Tipo C', color = pal[2], accessibility = list(exposeAsGroupOnly = TRUE), data = data.frame(x=datos3_df$rateCancelled, y=datos3_df$rateLate, z=datos3_df$rateOnTime, desc=datos3_df$Description)) %>%
      hc_plotOptions(scatter = list(width = 10, height = 10, depth = 10)) %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, 
                 pointFormat = '<span style="color:{point.color}">●</span> {point.desc}: <b>{point.x:,.2f}<b>,<b>{point.y:,.2f}<b>,<b>{point.z:,.2f}<b><br>') %>%
      hc_title(text = "Carrier Clustering") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  output$Segmentacion_tbl <- function(){ 
    Data <- segmentacion
    DF <- Data %>% 
      mutate(Cluster = case_when(
        X.KM.K.medias == 1 ~ "Tipo A"
        , X.KM.K.medias == 2 ~ "Tipo B"
        , X.KM.K.medias == 3 ~ "Tipo C"
      )
      , xOrder = case_when(
        Description %in% c(dtCarriers$Description) ~ 1
        , TRUE ~ 0
      )) %>%
      arrange(desc(xOrder), Description) %>%
      select(Description, Cluster, rateCancelled, rateLate, rateOnTime) 
    
    colnames(DF) <- c("Cluster", "Carrier", "% Cancelled", "% Delay", "% On Time")
    
    DF[1:nrow(DF),3]  <-  formatC(round(as.numeric(DF[,3]),2), big.mark=",")
    DF[1:nrow(DF),4]  <-  formatC(round(as.numeric(DF[,4]),2), big.mark=",")
    DF[1:nrow(DF),5]  <-  formatC(round(as.numeric(DF[,5]),2), big.mark=",")
    
    DF %>%
      kable("html", escape = F, align = "llcccc") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "bordered"), full_width = T, font_size = 15) %>%
      column_spec(1, bold = T) %>%
      row_spec(0, bold = T, background = "#00377B", color = "white")
  }
  
  
  RL_DT <- reactive({
    
    planes$fecha = paste("1/",as.numeric( planes$Month ),"/", planes$Year)
    planes$fct = as.Date(planes$fecha,format="%d/ %m / %Y")
    
    planes$on_time = planes$TPArrival == 'LATE ARRIVAL' | planes$TPArrival =='LATE DEPARTURE'
    
    plns = planes[planes$Year >= 2002,] %>%
      group_by(fct, Description) %>%
      summarise (Vuelos = sum(FlightsNum), Cancelados = sum(Cancelled)/sum(FlightsNum), on_time = 1- sum(on_time)/ sum(FlightsNum)  )
    
    return(plns)
  })
  
  output$FC1_plt <- renderHighchart({

    plns <- RL_DT()
    serieplanes = ts(plns$Vuelos[plns$Description == input$airlinePicker_FCs ], start=c(2002,1),frequency = 12)
    planes15 = window(serieplanes, start=2002,end=2015)
    pron1 = forecast(planes15,60)
    
    fc_df <- as.data.frame(pron1)
    graph_df <- union_all(
      fc_df %>%
      mutate(fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"), Vuelos = `Point Forecast`, kpi = "Forecast") %>% 
      select(fct,kpi,Vuelos)
      , plns %>% filter(Description == input$airlinePicker_FCs) %>% filter(fct <= as.Date("2008-02-01")) %>% mutate(kpi = "Real") %>% select(fct,kpi,Vuelos)
    )
    
    number <- length(unique(graph_df$kpi))
    pal <- wes_palette("BottleRocket2", 5, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(graph_df$fct),
                                      y = graph_df$Vuelos,
                                      year = graph_df$kpi),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = TRUE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%b-%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.0f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%b-%Y'))%>%
      hc_title(text = "Fligts Forecast") %>%
      hc_subtitle(text = input$airlinePicker_FCs) %>%
      hc_add_theme(hc_theme_elementary())

  })
  
  output$FC2_plt <- renderHighchart({

    plns <- RL_DT()
    serieplanes = ts(plns$Vuelos[plns$Description == input$airlinePicker_FCs ], start=c(2002,1),frequency = 12)
    planes15 = window(serieplanes, start=2002,end=2015)
    mod = auto.arima(planes15)
    pron2 = forecast(mod,60)
    
    fc_df <- as.data.frame(pron2)
    graph_df <- union_all(
      fc_df %>%
      mutate(fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"), Vuelos = `Point Forecast`, kpi = "Forecast") %>% 
      select(fct,kpi,Vuelos)
      , plns %>% filter(Description == input$airlinePicker_FCs) %>% filter(fct <= as.Date("2008-02-01")) %>% mutate(kpi = "Real") %>% select(fct,kpi,Vuelos)
    )
    
    number <- length(unique(graph_df$kpi))
    pal <- wes_palette("BottleRocket2", 5, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(graph_df$fct),
                                      y = graph_df$Vuelos,
                                      year = graph_df$kpi),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = TRUE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%b-%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.0f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%b-%Y'))%>%
      hc_title(text = "Fligts Forecast (Arima Method)") %>%
      hc_subtitle(text = input$airlinePicker_FCs) %>%
      hc_add_theme(hc_theme_elementary())

  })
  
  output$FC3_plt <- renderHighchart({

    plns <- RL_DT()
    serieplanes = ts(plns$Vuelos[plns$Description == input$airlinePicker_FCs ], start=c(2002,1),frequency = 12)
    planes15 = window(serieplanes, start=2002,end=2015)
    modHT = HoltWinters(planes15)
    pron3 = forecast(modHT,60)
    
    fc_df <- as.data.frame(pron3)
    graph_df <- union_all(
      fc_df %>%
      mutate(fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"), Vuelos = `Point Forecast`, kpi = "Forecast") %>% 
      select(fct,kpi,Vuelos)
      , plns %>% filter(Description == input$airlinePicker_FCs) %>% filter(fct <= as.Date("2008-02-01")) %>% mutate(kpi = "Real") %>% select(fct,kpi,Vuelos)
    )
    
    number <- length(unique(graph_df$kpi))
    pal <- wes_palette("BottleRocket2", 5, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(graph_df$fct),
                                      y = graph_df$Vuelos,
                                      year = graph_df$kpi),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = TRUE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%b-%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.0f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%b-%Y'))%>%
      hc_title(text = "Fligts Forecast (HoltWinters Method)") %>%
      hc_subtitle(text = input$airlinePicker_FCs) %>%
      hc_add_theme(hc_theme_elementary())

  })
  
  output$FC4_plt <- renderHighchart({

    plns <- RL_DT()
    serieplanesCan = ts(plns$Cancelados[plns$Description == input$airlinePicker_FCs ], start=c(2002,1),frequency = 12)
    planes15 = window(serieplanesCan, start=2002,end=2015)
    pronCan1 = forecast(planes15,60)
    
    fc_df <- as.data.frame(pronCan1)
    graph_df <- union_all(
      fc_df %>%
      mutate(fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"), Cancelados = `Point Forecast`, kpi = "Forecast") %>% 
      select(fct,kpi,Cancelados)
      , plns %>% filter(Description == input$airlinePicker_FCs) %>% filter(fct <= as.Date("2008-02-01")) %>% mutate(kpi = "Real") %>% select(fct,kpi,Cancelados)
    )
    
    number <- length(unique(graph_df$kpi))
    pal <- wes_palette("BottleRocket2", 5, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(graph_df$fct),
                                      y = graph_df$Cancelados,
                                      year = graph_df$kpi),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = TRUE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%b-%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.4f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%b-%Y'))%>%
      hc_title(text = "Cancelled Fligts Forecast") %>%
      hc_subtitle(text = input$airlinePicker_FCs) %>%
      hc_add_theme(hc_theme_elementary())

  })
  
  output$FC5_plt <- renderHighchart({

    plns <- RL_DT()
    serieplanesCan = ts(plns$Cancelados[plns$Description == input$airlinePicker_FCs ], start=c(2002,1),frequency = 12)
    planes15 = window(serieplanesCan, start=2002,end=2015)
    mod = auto.arima(planes15)
    pronCan2 = forecast(mod,60)
    
    fc_df <- as.data.frame(pronCan2)
    graph_df <- union_all(
      fc_df %>%
      mutate(fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"), Cancelados = `Point Forecast`, kpi = "Forecast") %>% 
      select(fct,kpi,Cancelados)
      , plns %>% filter(Description == input$airlinePicker_FCs) %>% filter(fct <= as.Date("2008-02-01")) %>% mutate(kpi = "Real") %>% select(fct,kpi,Cancelados)
    )
    
    number <- length(unique(graph_df$kpi))
    pal <- wes_palette("BottleRocket2", 5, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(graph_df$fct),
                                      y = graph_df$Cancelados,
                                      year = graph_df$kpi),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = TRUE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%b-%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.4f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%b-%Y'))%>%
      hc_title(text = "Cancelled Fligts Forecast (Arima Method)") %>%
      hc_subtitle(text = input$airlinePicker_FCs) %>%
      hc_add_theme(hc_theme_elementary())

  })
  
  output$FC6_plt <- renderHighchart({

    plns <- RL_DT()
    serieplanesCan = ts(plns$Cancelados[plns$Description == input$airlinePicker_FCs ], start=c(2002,1),frequency = 12)
    planes15 = window(serieplanesCan, start=2002,end=2015)
    modHT = HoltWinters(planes15)
    pronCan3 = forecast(modHT,60)
    
    fc_df <- as.data.frame(pronCan3)
    graph_df <- union_all(
      fc_df %>%
      mutate(fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"), Cancelados = `Point Forecast`, kpi = "Forecast") %>% 
      select(fct,kpi,Cancelados)
      , plns %>% filter(Description == input$airlinePicker_FCs) %>% filter(fct <= as.Date("2008-02-01")) %>% mutate(kpi = "Real") %>% select(fct,kpi,Cancelados)
    )
    
    number <- length(unique(graph_df$kpi))
    pal <- wes_palette("BottleRocket2", 5, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(graph_df$fct),
                                      y = graph_df$Cancelados,
                                      year = graph_df$kpi),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = TRUE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%b-%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.4f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%b-%Y'))%>%
      hc_title(text = "Cancelled Fligts Forecast (HoltWinters Method)") %>%
      hc_subtitle(text = input$airlinePicker_FCs) %>%
      hc_add_theme(hc_theme_elementary())

  })
  
  output$FC7_plt <- renderHighchart({

    plns <- RL_DT()
    serieplanesTime = ts(plns$on_time[plns$Description == input$airlinePicker_FCs ], start=c(2002,1),frequency = 12)
    planes15 = window(serieplanesTime, start=2002,end=2015)
    pronTime1 = forecast(planes15,60)
    
    fc_df <- as.data.frame(pronTime1)
    graph_df <- union_all(
      fc_df %>%
      mutate(fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"), on_time = `Point Forecast`, kpi = "Forecast") %>% 
      select(fct,kpi,on_time)
      , plns %>% filter(Description == input$airlinePicker_FCs) %>% filter(fct <= as.Date("2008-02-01")) %>% mutate(kpi = "Real") %>% select(fct,kpi,on_time)
    )
    
    number <- length(unique(graph_df$kpi))
    pal <- wes_palette("BottleRocket2", 5, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(graph_df$fct),
                                      y = graph_df$on_time,
                                      year = graph_df$kpi),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = TRUE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%b-%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.4f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%b-%Y'))%>%
      hc_title(text = "On Time Forecast") %>%
      hc_subtitle(text = input$airlinePicker_FCs) %>%
      hc_add_theme(hc_theme_elementary())

  })
  
  output$FC8_plt <- renderHighchart({

    plns <- RL_DT()
    serieplanesTime = ts(plns$on_time[plns$Description == input$airlinePicker_FCs ], start=c(2002,1),frequency = 12)
    planes15 = window(serieplanesTime, start=2002,end=2015)
    mod = auto.arima(planes15)
    pronTime2 = forecast(mod,60)
    
    fc_df <- as.data.frame(pronTime2)
    graph_df <- union_all(
      fc_df %>%
      mutate(fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"), on_time = `Point Forecast`, kpi = "Forecast") %>% 
      select(fct,kpi,on_time)
      , plns %>% filter(Description == input$airlinePicker_FCs) %>% filter(fct <= as.Date("2008-02-01")) %>% mutate(kpi = "Real") %>% select(fct,kpi,on_time)
    )
    
    number <- length(unique(graph_df$kpi))
    pal <- wes_palette("BottleRocket2", 5, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(graph_df$fct),
                                      y = graph_df$on_time,
                                      year = graph_df$kpi),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = TRUE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%b-%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.4f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%b-%Y'))%>%
      hc_title(text = "On Time Fligts Forecast (Arima Method)") %>%
      hc_subtitle(text = input$airlinePicker_FCs) %>%
      hc_add_theme(hc_theme_elementary())

  })
  
  output$FC9_plt <- renderHighchart({

    plns <- RL_DT()
    serieplanesTime = ts(plns$on_time[plns$Description == input$airlinePicker_FCs ], start=c(2002,1),frequency = 12)
    planes15 = window(serieplanesTime, start=2002,end=2015)
    modHT = HoltWinters(planes15)
    pronTime3 = forecast(modHT,60)
    
    fc_df <- as.data.frame(pronTime3)
    graph_df <- union_all(
      fc_df %>%
      mutate(fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"), on_time = `Point Forecast`, kpi = "Forecast") %>% 
      select(fct,kpi,on_time)
      , plns %>% filter(Description == input$airlinePicker_FCs) %>% filter(fct <= as.Date("2008-02-01")) %>% mutate(kpi = "Real") %>% select(fct,kpi,on_time)
    )
    
    number <- length(unique(graph_df$kpi))
    pal <- wes_palette("BottleRocket2", 5, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(graph_df$fct),
                                      y = graph_df$on_time,
                                      year = graph_df$kpi),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = TRUE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%b-%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.4f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%b-%Y'))%>%
      hc_title(text = "On Time Fligts Forecast (HoltWinters Method)") %>%
      hc_subtitle(text = input$airlinePicker_FCs) %>%
      hc_add_theme(hc_theme_elementary())

  })
  
  
  output$Comparativo_grp1 <- renderHighchart({
    
    plns <- RL_DT()
    
    fcs_dt <- NULL
    for (i in c("American Airlines Inc.", "Alaska Airlines Inc.", "Continental Air Lines Inc.", "United Air Lines Inc.","US Airways Inc.")) {
      
      serieplanes = ts(plns$Vuelos[plns$Description == i ], start=c(2002,1),frequency = 12)
      planes15 = window(serieplanes, start=2002,end=2015)
      pron = forecast(planes15,60)
      
      fc_df <- as.data.frame(pron)
      fc_df <- fc_df %>% mutate(Description = i, fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"))
      fcs_dt <- rbind( fcs_dt, fc_df )
      
    }
    
    DF <- union_all(
      fcs_dt %>%
        mutate(Vuelos = `Point Forecast`, kpi = "Forecast") %>% 
        select(fct,kpi,Vuelos,Description)
      , plns %>% 
        filter(Description %in% c("American Airlines Inc.", "Alaska Airlines Inc.", "Continental Air Lines Inc.", "United Air Lines Inc.","US Airways Inc.")) %>% 
        filter(fct <= as.Date("2008-02-01")) %>% 
        mutate(kpi = "Real") %>% 
        select(fct,kpi,Vuelos,Description)
    )
    
    number <- length(unique(DF$Description))
    pal <- wes_palette("BottleRocket2", number+7, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(DF$fct),
                                      y = DF$Vuelos,
                                      year = DF$Description),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.0f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%Y'))%>%
      hc_size(height = 600) %>%
      hc_title(text = "Flights Demand per Carrier") %>%
      hc_subtitle(text = "Tendencia Anual") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  output$Comparativo_grp2 <- renderHighchart({
    
    plns <- RL_DT()
    
    fcs_dt <- NULL
    for (i in c("American Airlines Inc.", "Alaska Airlines Inc.", "Continental Air Lines Inc.", "United Air Lines Inc.","US Airways Inc.")) {
      
      serieplanes = ts(plns$Cancelados[plns$Description == i ], start=c(2002,1),frequency = 12)
      planes15 = window(serieplanes, start=2002,end=2015)
      pron = forecast(planes15,60)
      
      fc_df <- as.data.frame(pron)
      fc_df <- fc_df %>% mutate(Description = i, fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"))
      fcs_dt <- rbind( fcs_dt, fc_df )
      
    }
    
    DF <- union_all(
      fcs_dt %>%
        mutate(Cancelados = `Point Forecast`, kpi = "Forecast") %>% 
        select(fct,kpi,Cancelados,Description)
      , plns %>% 
        filter(Description %in% c("American Airlines Inc.", "Alaska Airlines Inc.", "Continental Air Lines Inc.", "United Air Lines Inc.","US Airways Inc.")) %>% 
        filter(fct <= as.Date("2008-02-01")) %>% 
        mutate(kpi = "Real") %>% 
        select(fct,kpi,Cancelados,Description)
    )
    
    number <- length(unique(DF$Description))
    pal <- wes_palette("BottleRocket2", number+7, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(DF$fct),
                                      y = DF$Cancelados,
                                      year = DF$Description),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.4f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%Y'))%>%
      hc_size(height = 600) %>%
      hc_title(text = "Cancelled Flights per Carrier") %>%
      hc_subtitle(text = "Tendencia Anual") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
  output$Comparativo_grp3 <- renderHighchart({
    
    plns <- RL_DT()
    
    fcs_dt <- NULL
    for (i in c("American Airlines Inc.", "Alaska Airlines Inc.", "Continental Air Lines Inc.", "United Air Lines Inc.","US Airways Inc.")) {
      
      serieplanes = ts(plns$on_time[plns$Description == i ], start=c(2002,1),frequency = 12)
      planes15 = window(serieplanes, start=2002,end=2015)
      pron = forecast(planes15,60)
      
      fc_df <- as.data.frame(pron)
      fc_df <- fc_df %>% mutate(Description = i, fct = as.Date(paste("01",row.names(fc_df)),"%d %b%Y"))
      fcs_dt <- rbind( fcs_dt, fc_df )
      
    }
    
    DF <- union_all(
      fcs_dt %>%
        mutate(on_time = `Point Forecast`, kpi = "Forecast") %>% 
        select(fct,kpi,on_time,Description)
      , plns %>% 
        filter(Description %in% c("American Airlines Inc.", "Alaska Airlines Inc.", "Continental Air Lines Inc.", "United Air Lines Inc.","US Airways Inc.")) %>% 
        filter(fct <= as.Date("2008-02-01")) %>% 
        mutate(kpi = "Real") %>% 
        select(fct,kpi,on_time,Description)
    )
    
    number <- length(unique(DF$Description))
    pal <- wes_palette("BottleRocket2", number+7, "continuous")
    highchart() %>%
      hc_xAxis(type = "datetime", labels = list(format = '{value: %Y}')) %>%
      hc_add_series(color=pal[number:1], type = "line", 
                    data = data.frame(x = ymd(DF$fct),
                                      y = DF$on_time,
                                      year = DF$Description),
                    hcaes(x, y, group=year)) %>%
      hc_legend(reversed = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, xDateFormat = '%Y', 
                 pointFormat = '<span style="color:{point.color}">●</span> {series.name}: <b>{point.y:,.4f}<b> <br>', 
                 dateTimeLabelFormats = list(day = '%Y'))%>%
      hc_size(height = 600) %>%
      hc_title(text = "On Time Flights per Carrier") %>%
      hc_subtitle(text = "Tendencia Anual") %>%
      hc_add_theme(hc_theme_elementary())
    
  })
  
})
