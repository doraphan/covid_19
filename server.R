server <- function(input, output, session) {
    output$map_cases <- renderPlotly({
        l <- list(color = toRGB("black"), width = 1)
        g <- list( showframe = FALSE,
                   showcoastlines = FALSE,
                   projection = list(type = 'Mercator')
        )
        g1 <- list(
            scope = 'world',
            projection = list(type = 'Mercator'),
            showframe = FALSE,
            showcoastlines = FALSE,
            showland = TRUE,
            landcolor = toRGB("gray85"),
            subunitwidth = 1,
            countrywidth = 1,
            subunitcolor = toRGB("white"),
            countrycolor = toRGB("white")
        )
        
    if(input$data_type =='total_cases' & input$map_type =='choropeth'){    
       p1 <-  plot_geo(cases_final_official) %>% 
            add_trace(
                z = ~V1, color = ~V1,
                locations = ~CODE, marker = list(line = l), hoverinfo = "text",
                text = ~paste(cases_final_official$Country,":", format(cases_final_official$V1, big.mark=",", scientific=FALSE), "cases")
            ) %>%
            colorbar(title = 'Total number') %>%
            layout(
                title = 'COVID confirmed cases in countries', 
                geo = g,
                annotations = 
                           list(x = 1, y = -0.1, text = 'Source:<a href="https://ourworldindata.org/coronavirus-source-data">Our World Data</a>', 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=13))
            )}
    else if(input$data_type =='total_deaths' & input$map_type =='choropeth'){
        p2 <-  plot_geo(deaths_final_official) %>% 
            add_trace(
                z = ~V1, color = ~V1, colors = "Blues",
                 locations = ~CODE, marker = list(line = l), hoverinfo = "text",
                text = ~paste(deaths_final_official$Country,":", format(deaths_final_official$V1, big.mark=",", scientific=FALSE),"deaths")
            ) %>%
            colorbar(title = 'Total number') %>%
            layout(
                title = 'COVID deaths in countries', 
                geo = g,
                annotations = 
                    list(x = 1, y = -0.1, text = 'Source:<a href="https://ourworldindata.org/coronavirus-source-data">Our World Data</a>', 
                         showarrow = F, xref='paper', yref='paper', 
                         xanchor='right', yanchor='auto', xshift=0, yshift=0,
                         font=list(size=13))
            )}
    else if (input$data_type =='total_cases' & input$map_type =='bubble'){
        p3 <- plot_geo(cases_final_1) %>%
         add_markers(
            x = ~longitude, y = ~latitude, size = ~V1, color = ~V1, hoverinfo = "text",
            text = ~paste(cases_final_1$Country,":", format(cases_final_1$V1, big.mark=",", scientific=FALSE), "cases")
        ) %>% 
            layout(
                title = 'COVID confirmed cases in countries', 
                geo = g1,
                annotations = 
                    list(x = 1, y = -0.1, text = 'Source:<a href="https://ourworldindata.org/coronavirus-source-data">Our World Data</a>', 
                         showarrow = F, xref='paper', yref='paper', 
                         xanchor='right', yanchor='auto', xshift=0, yshift=0,
                         font=list(size=13))
            )
    }
    else {
        p4 <- plot_geo(deaths_final_1) %>%
            add_markers(
                x = ~longitude, y = ~latitude, size = ~V1,colors = "Blues", hoverinfo = "text", 
                text = ~paste(deaths_final_1$Country, ":", format(deaths_final_1$V1, big.mark=",", scientific=FALSE), "deaths")
            ) %>% 
            layout(
                title = 'COVID deaths in countries', 
                geo = g1,
                annotations = 
                    list(x = 1, y = -0.1, text = 'Source:<a href="https://ourworldindata.org/coronavirus-source-data">Our World Data</a>', 
                         showarrow = F, xref='paper', yref='paper', 
                         xanchor='right', yanchor='auto', xshift=0, yshift=0,
                         font=list(size=13))
            )
    }
        
    })
    
    output$daily_cases <- renderPlotly({
    if (input$data_type =='total_cases'){
        p5 <- plot_ly(cases[, c(1:2)], x = ~as.Date(date, format = "%d/%m/%Y"), y = ~World, type = 'bar',
                      marker = list(color = '#605ca8'))
        p5 <- p5 %>%  layout(title = "Daily Covid cases from 1/01/2020 to 6/11/2020",
                                xaxis = list(title = "Date"),
                                yaxis = list(title = "Number of Cases"))

    }
    else {
        p6 <- plot_ly(deaths[, c(1:2)], x = ~as.Date(date, format = "%d/%m/%Y"), y = ~World,type = 'bar')
        p6 <- p6 %>%  layout(title = "Daily Covid deaths from 1/01/2020 to 6/11/2020",
                                 xaxis = list(title = "Date"),
                                 yaxis = list(title = "Number of Deaths")) 
    }

    })
    
    output$asia <- renderPlotly({
        if (input$data_type =='total_cases'){
        cases_df_asia <- (cases_df %>% dplyr::filter(continent == "Asia")) %>% select( -c(Country, continent) )
        cases_df_asia  <- data.frame(sapply(cases_df_asia , function(x) as.numeric(as.character(x))))
        cases_asia_sum <- data.frame(colSums(cases_df_asia, na.rm = TRUE))
        colnames(cases_asia_sum) <- "total"
        cases_asia_sum$date <- cases$date
        
        p7 <-  plot_ly(cases_asia_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                       marker = list(color = '#605ca8')) 
        
        p7 <- p7 %>%  layout(title = "Daily Covid cases in Asia",
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Number of cases")) 
        }
       else{
           deaths_df_asia <- (deaths_df %>% dplyr::filter(continent == "Asia")) %>% select( -c(Country, continent) )
           deaths_df_asia  <- data.frame(sapply(deaths_df_asia , function(x) as.numeric(as.character(x))))
           deaths_asia_sum <- data.frame(colSums(deaths_df_asia, na.rm = TRUE))
           colnames(deaths_asia_sum) <- "total"
           deaths_asia_sum$date <- deaths$date
           
           p7 <-  plot_ly(deaths_asia_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                          marker = list(color = '#0073b7'))
           
           p7 <- p7 %>%  layout(title = "Daily Covid deaths in Asia",
                                xaxis = list(title = "Date"),
                                yaxis = list(title = "Number of deaths"))  
           
       } 
    })
    
    output$north_america <- renderPlotly({
        if (input$data_type =='total_cases'){
        cases_df_north <- (cases_df %>% dplyr::filter(continent == "North America")) %>% select( -c(Country, continent) )
        cases_df_north  <- data.frame(sapply(cases_df_north , function(x) as.numeric(as.character(x))))
        cases_north_sum <- data.frame(colSums(cases_df_north, na.rm = TRUE))
        colnames(cases_north_sum) <- "total"
        cases_north_sum$date <- cases$date
        
        p8 <-  plot_ly(cases_north_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                       marker = list(color = '#605ca8'))
        
        p8 <- p8 %>%  layout(title = "Daily Covid cases in North America",
                             xaxis = list(title = "Date"),
                             yaxis = list(title = "Number of cases")) 
        }
        else{
            deaths_df_north <- (deaths_df %>% dplyr::filter(continent == "North America")) %>% select( -c(Country, continent) )
            deaths_df_north  <- data.frame(sapply(deaths_df_north , function(x) as.numeric(as.character(x))))
            deaths_north_sum <- data.frame(colSums(deaths_df_north, na.rm = TRUE))
            colnames(deaths_north_sum) <- "total"
            deaths_north_sum$date <- deaths$date
            
            p8 <-  plot_ly(deaths_north_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                           marker = list(color = '#0073b7'))
            
            p8 <- p8 %>%  layout(title = "Daily Covid deaths in North America",
                                 xaxis = list(title = "Date"),
                                 yaxis = list(title = "Number of deaths"))  
            
        } 
    })
    
    output$south_america <- renderPlotly({
        if (input$data_type =='total_cases'){
        cases_df_south <- (cases_df %>% dplyr::filter(continent == "South America")) %>% select( -c(Country, continent) )
        cases_df_south  <- data.frame(sapply(cases_df_south , function(x) as.numeric(as.character(x))))
        cases_south_sum <- data.frame(colSums(cases_df_south, na.rm = TRUE))
        colnames(cases_south_sum) <- "total"
        cases_south_sum$date <- cases$date
        
        p9 <-  plot_ly(cases_south_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                       marker = list(color = '#605ca8'))
        
        p9 <- p9 %>%  layout(title = "Daily Covid cases in South America",
                             xaxis = list(title = "Date"),
                             yaxis = list(title = "Number of cases"))
    }
    else{
            deaths_df_south <- (deaths_df %>% dplyr::filter(continent == "South America")) %>% select( -c(Country, continent) )
            deaths_df_south  <- data.frame(sapply(deaths_df_south , function(x) as.numeric(as.character(x))))
            deaths_south_sum <- data.frame(colSums(deaths_df_south, na.rm = TRUE))
            colnames(deaths_south_sum) <- "total"
            deaths_south_sum$date <- deaths$date
            
            p9 <-  plot_ly(deaths_south_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                           marker = list(color = '#0073b7'))
            
            p9 <- p9 %>%  layout(title = "Daily Covid deaths in South America",
                                 xaxis = list(title = "Date"),
                                 yaxis = list(title = "Number of deaths"))  
            
        } 
    })
    
    output$europe <- renderPlotly({
        if (input$data_type =='total_cases'){
        cases_df_europe <- (cases_df %>% dplyr::filter(continent == "Europe")) %>% select( -c(Country, continent) )
        cases_df_europe  <- data.frame(sapply(cases_df_europe , function(x) as.numeric(as.character(x))))
        cases_europe_sum <- data.frame(colSums(cases_df_europe, na.rm = TRUE))
        colnames(cases_europe_sum) <- "total"
        cases_europe_sum$date <- cases$date
        
        p10 <-  plot_ly(cases_europe_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                       marker = list(color = '#605ca8'))
        
        p10 <- p10 %>%  layout(title = "Daily Covid cases in Europe",
                             xaxis = list(title = "Date"),
                             yaxis = list(title = "Number of cases")) 
    }
    else{
        deaths_df_europe <- (deaths_df %>% dplyr::filter(continent == "Europe")) %>% select( -c(Country, continent) )
        deaths_df_europe  <- data.frame(sapply(deaths_df_europe , function(x) as.numeric(as.character(x))))
        deaths_europe_sum <- data.frame(colSums(deaths_df_europe, na.rm = TRUE))
        colnames(deaths_europe_sum) <- "total"
        deaths_europe_sum$date <- deaths$date
        
        p10 <-  plot_ly(deaths_europe_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                       marker = list(color = '#0073b7'))
        
        p10 <- p10 %>%  layout(title = "Daily Covid deaths in Europe",
                             xaxis = list(title = "Date"),
                             yaxis = list(title = "Number of deaths"))  
    }
    })
    
    output$africa <- renderPlotly({
        if (input$data_type =='total_cases'){
        cases_df_africa <- (cases_df %>% dplyr::filter(continent == "Africa")) %>% select( -c(Country, continent) )
        cases_df_africa  <- data.frame(sapply(cases_df_africa , function(x) as.numeric(as.character(x))))
        cases_africa_sum <- data.frame(colSums(cases_df_africa, na.rm = TRUE))
        colnames(cases_africa_sum) <- "total"
        cases_africa_sum$date <- cases$date
        
        p11 <-  plot_ly(cases_africa_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                       marker = list(color = '#605ca8'))
        
        p11 <- p11 %>%  layout(title = "Daily Covid cases in Africa",
                             xaxis = list(title = "Date"),
                             yaxis = list(title = "Number of cases")) 
    }
    else{
        deaths_df_africa <- (deaths_df %>% dplyr::filter(continent == "Africa")) %>% select( -c(Country, continent) )
        deaths_df_africa  <- data.frame(sapply(deaths_df_africa , function(x) as.numeric(as.character(x))))
        deaths_africa_sum <- data.frame(colSums(deaths_df_africa, na.rm = TRUE))
        colnames(deaths_africa_sum) <- "total"
        deaths_africa_sum$date <- deaths$date
        
        p11 <-  plot_ly(deaths_africa_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                       marker = list(color = '#0073b7'))
        
        p11 <- p11 %>%  layout(title = "Daily Covid deaths in Africa",
                             xaxis = list(title = "Date"),
                             yaxis = list(title = "Number of deaths"))  
    }
    })
    
    output$oceania <- renderPlotly({
        if (input$data_type =='total_cases'){
        cases_df_oceania <- (cases_df %>% dplyr::filter(continent == "Oceania")) %>% select( -c(Country, continent) )
        cases_df_oceania  <- data.frame(sapply(cases_df_oceania , function(x) as.numeric(as.character(x))))
        cases_oceania_sum <- data.frame(colSums(cases_df_oceania, na.rm = TRUE))
        colnames(cases_oceania_sum) <- "total"
        cases_oceania_sum$date <- cases$date
        
        p12 <-  plot_ly(cases_oceania_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                        marker = list(color = '#605ca8'))
        
        p12 <- p12 %>%  layout(title = "Daily Covid cases in Oceania",
                               xaxis = list(title = "Date"),
                               yaxis = list(title = "Number of cases")) 
    }
    else{
        deaths_df_oceania <- (deaths_df %>% dplyr::filter(continent == "Oceania")) %>% select( -c(Country, continent) )
        deaths_df_oceania  <- data.frame(sapply(deaths_df_oceania , function(x) as.numeric(as.character(x))))
        deaths_oceania_sum <- data.frame(colSums(deaths_df_oceania, na.rm = TRUE))
        colnames(deaths_oceania_sum) <- "total"
        deaths_oceania_sum$date <- deaths$date
        
        p12 <-  plot_ly(deaths_oceania_sum, x = ~as.Date(date, format = "%d/%m/%Y"), y = ~total, type = 'bar',
                       marker = list(color = '#0073b7'))
        
        p12 <- p12 %>%  layout(title = "Daily Covid deaths in Oceania",
                             xaxis = list(title = "Date"),
                             yaxis = list(title = "Number of deaths"))  
    }
    })
}


