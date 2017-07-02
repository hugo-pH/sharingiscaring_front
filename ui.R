library(shiny)
library(shinythemes)
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
            tabPanel("Summary", uiOutput("table.orders"), uiOutput("table.purchases")),
             tabPanel("Purchases on time", plotOutput("plot.time.purchases")),
            tabPanel("Total month", plotOutput("plot.total.purchases")),
            tabPanel("Balance", tableOutput("adjust.payments"))
            
                  )
  )

)
  