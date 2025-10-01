library(shiny)
library(bslib)
library(RSQLite)
library(tidyverse) 

)

# Define UI ----
ui <- page_navbar(
  
  title = "Practice App",
  bg = "#2D89C8",
  inverse = theme = bs_theme(bg = "black", fg = "white"),
  nav_panel(title = "Database View",
    h3("Practice Database"),
    p("See current data in database below."),
    card(
      card_header("Pratice Shiny app"),
      "Welcome to my pratice shiny app",
      tableOutput("preview"),
      card_footer("This is the end of the card")
    )
  ),
  nav_panel(title = "Manage Databse", 
            h3("Enter data below"),
              card(
                dateInput("date", label = h3("Enter Date of Birth:"), value = "2025-01-01"),

                fluidRow(column(3, verbatimTextOutput("valueDate"))),
                hr(),
                textInput("text", label = h3("Enter Full Name:"), value = ""),
 
                fluidRow(column(3, verbatimTextOutput("valueText"))),
              ),
            actionButton("action", label = "CLICK TO INSER INTO DATABSE"),
            fluidRow(column(2, verbatimTextOutput("valueAction")))
  )
)

# Define server logic ----
server <- function(input, output) {
  #Build the placeholder
  con <- dbConnect(drv = RSQLite::SQLite(),dbname = "mydata.sqlite")
  
  # Ensure the table exists
  if(!"users" %in% dbListTables(con)) {
    dbExecute(con, "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, dob TEXT)")
  }
  
  # Reactive trigger to refresh table
  refresh <- reactiveVal(0)
  
  # Show first few rows of users
  output$preview <- renderTable({
    refresh()  # depend on refresh
    dbGetQuery(con, "SELECT * FROM users")
  })
  
  observeEvent(input$action, {
    req(input$text)  # require name not empty
    dob_str <- as.character(input$date)  # convert Date to string
    dbExecute(con, "INSERT INTO users (name, dob) VALUES (?, ?)",
              params = list(input$text, dob_str))  # <-- use dob_str, not input$dob_str
    refresh(isolate(refresh()) + 1)  # trigger table refresh
  })
  
    
  output$valueDate <- renderPrint({ input$date })
  output$valueText <- renderPrint({ input$text })
  output$valueAction <- renderPrint({ input$action })
  
  # Close DB connection when app stops
  onStop(function() {
    dbDisconnect(con)
  })
}



# Run the app ----
shinyApp(ui = ui, server = server)