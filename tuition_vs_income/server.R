library(shiny)
library(leaflet)

setwd("C:/Users/Dominic/Documents/tuition_v_income/app/")
source("tuition_vs_income/global.R")

shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet(map_dat) %>%
      addPolygons(
        weight = 1,
        color = "white",
        fillColor = "gray",
        fillOpacity = 0.9,
        layerId = ~NAME
      )
  })
  
  output$income_vs_tuition <- renderPlot({
    allchanges %>% 
      filter(state == currentState()) %>% 
      group_by(yr) %>% 
      summarise(avg_median_hh_income_adj=mean(median_hh_income_adj),
                avg_tuition_adj=mean(tuition_adj)) %>% 
      ungroup %>% 
      mutate(pct_change_median_hh_income=
               (avg_median_hh_income_adj-first(avg_median_hh_income_adj))/first(avg_median_hh_income_adj),
             pct_change_tuition=
               (avg_tuition_adj-first(avg_tuition_adj))/first(avg_tuition_adj)
      ) %>% 
      select(-avg_median_hh_income_adj, -avg_tuition_adj) %>% 
      gather(metric, value, -yr) %>% 
      filter(yr == 2017) %>% mutate(yr = as.factor(yr)) %>% 
      mutate(value = value*100) %>% 
      mutate(metric = case_when(
        metric == "pct_change_median_hh_income" ~ "% Change Income",
        metric == "pct_change_tuition" ~ "% Change Tuition"
      )) %>% 
      ggplot(aes(x=metric, y=value, fill=metric)) + geom_bar(stat="identity") +
      scale_y_continuous(limits=c(-4, 695)) + theme_classic() +
      # scale_fill_manual(values = c("blue", "red")) +
      geom_text(aes(label=return_prty_pct(value)), nudge_y = 18) +
      labs(title="Change in Median Household Income vs \n Change in Flagship School Tuition \n 1980 to 2018") +
      guides(fill=FALSE)
  })
  
  output$summary_stats <- renderUI({
    dat <- allchanges %>% 
      filter(state == currentState())
    
    tuition_1980 <- filter(dat, yr==1980)$tuition_adj
    tuition_2017 <- filter(dat, yr==2017)$tuition
    
    median_hh_income_1980 <- filter(dat, yr==1980)$median_hh_income_adj
    median_hh_income_2017 <- filter(dat, yr==2017)$median_hh_income
    
    school_name <- first(dat$institution_name)
    state_name <- first(dat$state)
    
    str1 <- paste0("Tuition 1980 at <i>", school_name, "</i> (inflation adjusted): <b> $", round(tuition_1980,0), "</b>")
    str2 <- paste0("Tuition 2017 at <i>", school_name, "</i>: <b>$", tuition_2017, "</b>")
    str3 <- paste0("Median household income 1980 in <i>", state_name, "</i> (inflation adjusted): <b> $", round(median_hh_income_1980,0), "</b>")
    str4 <- paste0("Median household income 2017 in <i>", state_name, "</i>: <b> $", round(median_hh_income_2017,0), "</b>")
    
    HTML(paste(str1, str2, str3, str4, sep = '<br/>'))  
  })
  
  output$savings <- renderUI({
    
    dat <- allchanges %>% 
      filter(state == currentState())
    
    tuition_1980 <- filter(dat, yr==1980)$tuition_adj
    tuition_2017 <- filter(dat, yr==2017)$tuition
    
    median_hh_income_1980 <- filter(dat, yr==1980)$median_hh_income_adj
    median_hh_income_2017 <- filter(dat, yr==2017)$median_hh_income
    
    yr_savings_1980 <- median_hh_income_1980*(savingsChoices()[1]/100)*(savingsChoices()[2]/100)
    yr_savings_2017 <- median_hh_income_2017*(savingsChoices()[1]/100)*(savingsChoices()[2]/100)
    
    yrs_to_save_1980 <- (tuition_1980*4)/yr_savings_1980
    yrs_to_save_2017 <- (tuition_2017*4)/yr_savings_2017
    
    str1 <- paste0("If your income was ", savingsChoices()[1]/100, "x the median household and you allocated ",
                   savingsChoices()[2], "% each year for school, it would take you:")
    str2 <- paste0(round(yrs_to_save_1980,1), " years in 1980")
    str3 <- paste0(round(yrs_to_save_2017,1), " years in 2017")
    str4 <- "to save for four years of tuition + mandatory fees"
    
    HTML(paste(str1, "<b>", str2, str3, "</b>", str4, sep = '<br/>'))
  })
  
  currentState <- reactive({
    event <- input$map_shape_click
    if(is.null(event)){
      return("Michigan")
    }
    return(event$id)
  })
  
  observe({
    leafletProxy("map") %>% 
      clearShapes() %>% 
      addPolygons(
        data = map_dat,
        weight = 1,
        color = "white",
        fillColor = "gray",
        fillOpacity = 0.9,
        layerId = ~NAME
      ) %>% 
      addPolygons(
        data = filter(map_dat, NAME == currentState()),
        weight = 1,
        color = "white",
        fillColor = "navy",
        fillOpacity = 0.9,
        layerId = ~NAME
      )
  })
  
  savingsChoices <- reactive({
    c(input$pct_median, input$pct_save)
  })
  
})
