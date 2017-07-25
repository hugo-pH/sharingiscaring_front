# server balance
library(shiny)
library(shinythemes)
library(lubridate)
library(tidyverse)
library(stringr)
library(wesanderson)
library(ggthemes)
library(pool)
library(shinyjs)
library(shinyURL)
library(cowplot)
theme_set( theme_fivethirtyeight(15))
source("functions.R")

paths <- read_csv("../needs_app_db/paths.csv", col_types = cols(
  file = col_character(),
  path = col_character()
))

sqlite_path <- paths %>% filter(file == "sqlite") %>% pull(path)
users_path <- paths %>% filter(file == "users") %>% pull(path)

orders_table <- "orders"
purchases_table <- "purchases"

users_df <- read_csv(users_path, col_types = cols(
  name = col_character(),
  password = col_character()
))


users <- users_df %>%  pull(password) %>% as.list()
names(users) <- users_df$name

shinyServer(function(input, output, session){
  
  
  
  
  shinyURL.server()
  
  USER <- reactiveValues(Logged = FALSE)
  
  observeEvent(input$.login, {
    # browser()
    if (isTRUE(users[[input$.username]]==input$.password)){
      USER$Logged <- TRUE
    } else {
      show("message")
      output$message = renderText("Invalid user name or password")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })
  
  output$app = renderUI(
    if (!isTRUE(USER$Logged)) {
      fluidRow(column(width=4, offset = 4,
                      wellPanel(id = "login",
                                textInput(".username", "Username:"),
                                passwordInput(".password", "Password:"),
                                div(actionButton(".login", "Log in"), style="text-align: center;")
                      ),
                      textOutput("message")
      ))
    } else{
      
      fluidPage(
        theme = shinytheme("sandstone"),
        shinyjs::useShinyjs(),
        title = "The street of the children's dike",
        div(id = "header",
            h1("Sharing is caring"),
            h4("")),
        fluidRow(
          column(12,
                 radioButtons("action", "Which period do you wanna check?", 
                              choices = c("Last month", "Total" , "Custom"), 
                              selected = "Last month"),
                 uiOutput("chooseFormType"), 
                 shinyjs::hidden(
                   div(
                     id = "thankyou_msg",
                     h3("Thanks, your response was submitted successfully!"),
                     actionLink("submit_another", "Submit another response")
                   )
                 )
          )
        ),
        fluidRow(
          tabsetPanel(
            tabPanel("Summary", DT::dataTableOutput("table.purchases")),
            # tabPanel("Purchases on time", plotOutput("plot.time.purchases")),
            tabPanel("Figures", plotOutput("figures", height = "1000px")),
            tabPanel("Balance",  DT::dataTableOutput("adjust.payments"))
            
          )
        )
        
      )
      
      
    }
  )
  
  
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
  # output$OrdersData <-render.tables(sqlite_path, orders_table)
  
  
  # output$PurchasesData <- render.tables(selected_data())
  
  
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
  
  output$table.purchases <- DT::renderDataTable(
    DT::datatable(selected_data() %>% select(timestamp, person, item, amount) %>% arrange(desc(timestamp)), options = list(pageLength = 25))
  )
  
  
  
  
  output$figures <- renderPlot({
    # browser()
    
    
    n_persons <- selected_data() %>% distinct(person) %>% summarise(n()) %>% pull()
    
    plot.time <- selected_data() %>% 
      arrange(timestamp) %>% 
      mutate(
        time_d =  as.numeric(difftime(ymd_hms(timestamp), ymd_hms(timestamp)[1], units= "day"))
      ) %>% 
      group_by(person) %>% 
      mutate(paid = cumsum(amount)) %>% 
      ungroup() %>% 
      filter(!is.na(person)) %>% 
      ggplot(aes(x = time_d, y = paid, colour = person)) +
      theme(legend.position = "top") +
      geom_point(size = 5) +
      geom_line() +
      ggtitle("Purchases over time") +
      xlim(0, NA) +
      ylim(0, NA) +
      xlab("Time (days)") +
      ylab("Money-money €")
    
    
    if(n_persons > 5){
     plot.time <- plot.time + scale_colour_brewer(name = "Who", palette = "Set1") 
    }else{
      plot.time <- plot.time + scale_color_manual(values = wes_palette("Darjeeling")) 
    }
    
    
    
    plot.total.bars <- selected_data() %>% 
      ungroup() %>% 
      filter(!is.na(person)) %>% 
      group_by(person) %>% 
      summarise(
        total = sum(amount) 
      ) %>% 
      ggplot(aes(x = person, y = total, fill = person)) +
      geom_bar(stat = "identity") +
      theme(legend.position = "none") +
      ylim(0, NA) +
      ggtitle("Total expenses per person") +
      scale_fill_manual(values = wes_palette("Darjeeling")) +
      xlab("Who") +
      ylab("Money-money €")
    
    if(n_persons > 5){
      plot.total.bars <- plot.total.bars + scale_colour_brewer(name = "Who", palette = "Set1") 
    }else{
      plot.total.bars <- plot.total.bars + scale_color_manual(values = wes_palette("Darjeeling")) 
    }
    
    plot.total.stack <- selected_data() %>% 
      filter(!is.na(person)) %>% 
      mutate(timestamp = ymd_hms(timestamp),
             month = month(timestamp, label = T)) %>% 
      group_by(person, month, year) %>% 
      summarise(
        total = sum(amount) 
      ) %>% 
      arrange(year, month) %>% 
      ggplot(aes(x = interaction(year, month), y = total)) +
      geom_bar(aes(fill = person), position = position_stack(reverse = TRUE), stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = wes_palette("Darjeeling")) +
      theme(legend.position = "none") +
      ylim(0, NA) +
      ggtitle("Total expenses per person by month") +
      xlab("Period") +
      ylab("Money-money €")
    
    if(n_persons > 5){
      plot.total.stack <- plot.total.stack + scale_colour_brewer(name = "Who", palette = "Set1") 
    }else{
      plot.total.stack <- plot.total.stack + scale_color_manual(values = wes_palette("Darjeeling")) 
    }
    
    
    plot_grid(plot.time, plot.total.bars, plot.total.stack, ncol = 1)
    
  })
  output$table.total.paid <-renderDataTable({
    
    selected_data() %>% 
      group_by(person) %>% 
      summarise(
        total = sum(amount) 
      )
    
  })
  
  output$adjust.payments <-DT::renderDataTable({
    # browser()
    total.per.person <- selected_data() %>% 
      group_by(person) %>% 
      summarise(
        total_per_person = sum(amount) 
      )
      
    total.month <- sum(total.per.person$total_per_person)
    total.month.per.person <- total.month / length(unique(total.per.person$person))
    out.table <- total.per.person %>% 
      mutate(
        total_paid_in_period = total.month,
        due_per_person = total.month.per.person,
             balance = due_per_person - total_per_person) 
    DT::datatable(out.table, 
                  options = list(paging = FALSE, searching = FALSE),
                  colnames = c("Who", "Total paid per person", "Total expenses", "Due per person", "Balance")) %>% 
      DT::formatRound(columns = 2:5, digits=2)
  })
  
  
  
}

)