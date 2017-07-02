library(shiny)
# library(rdrop2)
library(lubridate)
library(tidyverse)
library(stringr)
library(wesanderson)
library(ggthemes)
library(pool)
theme_set(theme_bw(20))
source("functions.R")

# Connect to sqlite database
sqlite_path <- "../needs_app_db/home_stuff.db"
orders_table <- "orders"
purchases_table <- "purchases"


users <- read.csv("./users.csv") %>% unlist() %>% as.character()

shinyServer(function(input, output, session){
  action <- reactive({
    input$action
  })
  
  #########################
  ## render UI functions ##
  #########################
  
  
  output$chooseFormType <- renderUI({
    
    if(action() == "Custom"){
     
      ## Show a selected range of results
      
      uiOutput("selectCustomPeriod")
      

    }else{
      NULL
    }
  })
  
  
  output$selectCustomPeriod <- renderUI({
    d_purchases <- pull.data.from.db(sqlite_path, purchases_table)
    
    dates <- d_purchases %>% mutate(date = ymd_hms(timestamp)) %>% pull(date)
    
    max.date <- max(dates)
    min.date <- min(dates)
    dateRangeInput("customDateRange", "Select custom timerange",
                start = min.date,  end = max.date)
  })
  
  
  selected_data <- reactive({
    # browser()
    d_purchases <- pull.data.from.db(sqlite_path, purchases_table)
    
    now_month <- month(now())
    now_year <- year(now())
    
    if(input$action == "Last month"){
      selected_data <- d_purchases %>% filter(year == now_year & month == now_month) 
      validate(
        need(nrow(selected_data) != 0, "No data for this month yet :(")
      )
    }else if(input$action == "Total"){
      selected_data <- d_purchases
    }else if(input$action == "Custom"){
      
      selected_data <- d_purchases %>%
        mutate(date = as.Date(timestamp)) %>% 
        filter(date >= as.Date(input$customDateRange[1]) & date <= as.Date(input$customDateRange[2]))
        
        
      validate(
        need(nrow(selected_data) != 0, "No data in selected time range.")
        )
    }
    return(selected_data)
}  )
  
  
  
  # Show the responses already submitted
  output$OrdersData <-render.tables(sqlite_path, orders_table)
  
  
  output$PurchasesData <- render.tables(sqlite_path, purchases_table)
  
  
  # Allow user to download responses
  output$downloadPurchBtn <- downloadHandler(
    filename = function() { 
      sprintf("%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(pull.data.from.db(sqlite_path, purchases_table), file, row.names = FALSE)
    }
  )  
  
  # Allow user to download responses
  output$downloadOrdersBtn <- downloadHandler(
    filename = function() { 
      sprintf("%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(pull.data.from.db(sqlite_path, orders_table), file, row.names = FALSE)
    }
  )  
  
  output$table.orders <- renderUI({
      div(
        h2("Current orders"),
        downloadButton("downloadOrdersBtn", "Download Orders"), br(), br(),
        DT::dataTableOutput("OrdersData") 
      )
  })


  
  output$table.purchases <- renderUI({
    div(
      id = "adminPanel",
      h2("Previous purchases"),
      downloadButton("downloadPurchBtn", "Download purchases"), br(), br(),
      DT::dataTableOutput("PurchasesData")
      
    )
  })
  
  
  output$plot.time.purchases <- renderPlot({
    selected_data() %>% 
      arrange(timestamp) %>% 
      mutate(
        time_d =  as.numeric(difftime(ymd_hms(timestamp), ymd_hms(timestamp)[1], units= "day"))
      ) %>% 
      group_by(person) %>% 
      mutate(paid = cumsum(amount)) %>% 
      ggplot(aes(x = time_d, y = paid, colour = person)) +
      theme_solarized(20) +
      scale_color_manual(name = "Who", values = wes_palette("Cavalcanti")) +
      theme(legend.position = "top") +
      geom_point(size = 5) +
      geom_line() +
      xlim(0, 31) +
      ylim(0, NA) +
      xlab("Time (days)") +
      ylab("Money-money €")
  })  
  
  output$plot.total.purchases <- renderPlot({
    selected_data() %>% 
      group_by(person) %>% 
      summarise(
        total = sum(amount) 
      ) %>%  
      ggplot(aes(x = person, y = total, colour = person)) +
      theme_solarized(20) +
      scale_color_manual(values = wes_palette("Cavalcanti")) +
      geom_point(size = 5) +
      theme(legend.position = "none") +
      ylim(0, NA) +
      xlab("Who") +
      ylab("Money-money €")
    
  })
  
  output$table.total.paid <-renderDataTable({
    
    selected_data() %>% 
      group_by(person) %>% 
      summarise(
        total = sum(amount) 
      )
    
  })
  
  output$adjust.payments <-renderTable({
    
    total.per.person <- selected_data() %>% 
      group_by(person) %>% 
      summarise(
        total_per_person = sum(amount) 
      )
      
    total.month <- sum(total.per.person$total)
    total.month.per.person <- total.month / length(unique(total.per.person$person))
    out.table <- total.per.person %>% 
      mutate(
        total_paid_in_period = total.month,
        due_per_person = total.month.per.person,
             balance = total_to_paid_per_person - total)
    out.table
  })
  
  
  
}
)