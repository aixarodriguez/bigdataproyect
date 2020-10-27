library(shiny)

shinyServer(function(input, output) {

    output$grpAvgTime <- renderHighchart({
        dtRndr<-left_join(data.frame(Year=dtYears$Year),
                          data.frame(dtRsm %>%
                                         filter(Code %in% dtCarriers$Code & Year!=2001) %>%
                                         group_by(Year,Code,Description) %>%
                                         summarise(Cnts=sum(ArrDelay+DepDelay)/sum(FlightsNum))),
                          by="Year",all=TRUE)
        
        highchart() %>%
            hc_chart(animation = TRUE) %>%
            hc_title(text="Tiempo promedio anual de Delay") %>%
            hc_tooltip(crosshairs = TRUE, shared = TRUE,pointFormat='<span style="color:{series.color}">\u25CF </span>{series.name}: <b>{point.y:,.0f}</b><br/>') %>%
            hc_xAxis(categories = as.list(unique(dtRndr$Year))) %>%
            hc_add_series(data=dtRndr,type = "line",hcaes(y=Cnts,names=Description,group=Description)) %>%
            hc_add_theme(hc_theme_gridlight())
    })
    
    output$grpCncll <- renderHighchart({
        dtRndr<-dtRsm %>%
            filter(Code %in% dtCarriers$Code & Year!=2001 & Cancelled==1 & CancellationCode=='A') %>%
            group_by(Year,Code,Description) %>%
            summarise(Flights=sum(FlightsNum))
        
        highchart() %>%
            hc_chart(animation = TRUE) %>%
            hc_title(text="Cancelaciones Anuales por Problemas de la Aerolinea") %>%
            hc_tooltip(crosshairs = TRUE, shared = TRUE,pointFormat='<span style="color:{series.color}">\u25CF </span>{series.name}: <b>{point.y:,.0f}</b><br/>') %>%
            hc_xAxis(categories = as.list(unique(dtRndr$Year))) %>%
            hc_add_series(data=dtRndr,type = "line",hcaes(y=Flights,names=Description,group=Description)) %>%
            hc_add_theme(hc_theme_google())
        
    })
    
    output$gprNmbrFlights <- renderHighchart({
        dtRndr<-left_join(data.frame(Year=dtYears$Year),
                          data.frame(dtRsm %>%
                                         filter(Code %in% dtCarriers$Code & Year!=2001) %>%
                                         group_by(Year,Code,Description) %>%
                                         summarise(Cnts=sum(FlightsNum))),
                          by="Year",all=TRUE)
        
        highchart() %>%
            hc_chart(animation = TRUE) %>%
            hc_title(text="Total de vuelos Anuales") %>%
            hc_tooltip(crosshairs = TRUE, shared = TRUE,pointFormat='<span style="color:{series.color}">\u25CF </span>{series.name}: <b>{point.y:,.0f}</b><br/>') %>%
            hc_xAxis(categories = as.list(unique(dtRndr$Year))) %>%
            hc_add_series(data=dtRndr,type = "line",hcaes(y=Cnts,names=Description,group=Description)) %>%
            hc_add_theme(hc_theme_gridlight())
        
    })
    
    
    output$dtDatos <- renderDataTable({
        DT::datatable(dtRsm,options = list("pageLength" = 40,paging = TRUE,searching = TRUE))
    })
    
    output$dwDt <- downloadHandler(filename="dt_plns_rsmn.csv",content = function(file){
        dtRndr<-dtRsm
        if(nrow(dtRndr)!=0){
            write.csv(dtRndr, file)
        }
    })

})
