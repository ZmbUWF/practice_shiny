library(shiny)
library(bslib)
library(RSQL)
library(RSQLite)
library(tidyverse) 

# Define UI ----
ui <- page_navbar(
  title = "Practice App",
  bg = "#2D89C8",
  inverse = TRUE,
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
  nav_panel(title = "Two", p("Second page content.")),
)

# Define server logic ----
server <- function(input, output) {
  
  #Build the placeholder
  con <- dbConnect(drv = RSQLite::SQLite(),
                   dbname = "mydata.sqlite"
                   )
  
  #Load the population table
  dbWriteTable(conn = con, 
               name = "population",
               value = population,
               append  = TRUE)
  
  #Load the who table
  dbWriteTable(conn = con, 
               name = "who",
               value = who,
               append  = TRUE)
  
  # Show first few rows of "population"
  output$preview <- renderTable({
    dbGetQuery(con, "SELECT * FROM population LIMIT 10")
  })
    
    # Close DB connection when app stops
    onStop(function() {
      dbDisconnect(con)
    })
}



# Run the app ----
shinyApp(ui = ui, server = server)