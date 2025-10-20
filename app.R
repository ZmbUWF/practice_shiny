library(shiny)
library(bslib)
library(RMySQL)
library(shinyauthr)
library(shinyjs)
library(magrittr)

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("user1", "user2", "user3"),
  password = c("pass1", "pass2", "pass3"),
  permissions = c("standard", "standard", "standard"),
  name = c("User One", "User Two", "User Three")
)

ui <- page_fluid(
  shinyjs::useShinyjs(),
  
  # add login panel UI function
  shinyauthr::loginUI(id = "login"),
  
  div(
    id = "show-page-content",
    navset_bar(
      title = "Practice App",
      bg = "#2D89C8",
      
      nav_panel(
        title = "Database View",
        layout_columns(
          card(
            card_header("Practice Database"),
            card_body(
              p("See current data in database below."),
              tableOutput("preview")
            ),
            card_footer("This is the end of the card")
          )
        )
      ),
      
      nav_panel(
        title = "Manage Database",
        h3("Enter data below"),
        layout_columns(
          card(
            card_header("Enter Data"),
            card_body(
              dateInput("date", 
                        label = "Enter Date of Birth:", 
                        value = "2025-01-01"),
              
              textInput("textName", 
                        label = "Enter Full Name:", 
                        value = ""),
              
              textInput("textMajor", 
                        label = "Enter Major:", 
                        value = ""),
              
              textInput("textClass", 
                        label = "Enter Class:", 
                        value = ""),
              
              "To delete or update enter a ID and click appropriate button (Leave blank if inserting)",
              numericInput("numID", 
                           label = " (*Required for update/delete) Enter ID:", 
                           value = NA,
                           min = 1)
            )
          )
        ),
        actionButton("insert", 
                     label = "Insert Into Database",
                     class = "btn-success btn-sm"),
        actionButton("update", 
                     label = "Update Database",
                     class = "btn-warning btn-sm"),
        actionButton("delete", 
                     label = "Delete from Database",
                     class = "btn-danger btn-sm")
      ),
      
      # Add logout button to navbar
      nav_spacer(),
      nav_item(
        shinyauthr::logoutUI(id = "logout")
      )
    )
  ) %>% shinyjs::hidden()
)

# Define server logic ----
server <- function(input, output, session) {
  shinyjs::useShinyjs()
  
  # call login module supplying data frame, 
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # Show content when logged in
  shiny::observe({
    shiny::req(credentials()$user_auth)
    shinyjs::show(id = "show-page-content")         
  })
  
  # Hide content when logged out
  shiny::observe({
    if (!is.null(credentials()$user_auth) && !credentials()$user_auth) {
      shinyjs::hide(id = "show-page-content")
    }
  })
  
  # Build the MySQL database connection
  con <- dbConnect(
    MySQL(),
    host = "mysql-10ef0821-shiny-0e09.g.aivencloud.com",
    port = 19625,
    dbname = "defaultdb",
    user = "avnadmin",
    password = "AVNS_8IXRc4DJH6EGDS3QxyU",
    ssl = TRUE
  )
  
  # Ensure the updatedUsers table exists with added_by column
  if(!"updatedUsers" %in% dbListTables(con)) {
    dbExecute(con, "CREATE TABLE updatedUsers (
      id INT AUTO_INCREMENT PRIMARY KEY, 
      name VARCHAR(255), 
      dob DATE, 
      major VARCHAR(255), 
      class VARCHAR(255),
      added_by VARCHAR(255)
    )")
  }
  
  # Reactive trigger to refresh table
  refresh <- reactiveVal(0)
  
  # Show only rows added by the current user
  output$preview <- renderTable({
    req(credentials()$user_auth)
    refresh()  # depend on refresh
    current_user <- credentials()$info$user
    query <- sqlInterpolate(con, 
                            "SELECT id, name, dob, major, class FROM updatedUsers WHERE added_by = ?user",
                            user = current_user)
    dbGetQuery(con, query)
  })
  
  observeEvent(input$insert, {
    req(credentials()$user_auth)
    req(input$textName)  # require name not empty
    req(input$textMajor)  # require major not empty
    req(input$textClass)  # require class not empty
    dob_str <- as.character(input$date)  # convert Date to string
    current_user <- credentials()$info$user
    
    # Use sqlInterpolate for safe parameterized queries, including added_by
    query <- sqlInterpolate(con,
                            "INSERT INTO updatedUsers (name, dob, major, class, added_by) VALUES (?name, ?dob, ?major, ?class, ?user)",
                            name = input$textName,
                            dob = dob_str,
                            major = input$textMajor,
                            class = input$textClass,
                            user = current_user
    )
    dbExecute(con, query)
    refresh(isolate(refresh()) + 1)  # trigger table refresh
  })
  
  observeEvent(input$update, {
    req(credentials()$user_auth)
    req(!is.na(input$numID))  # require ID to be filled
    
    current_user <- credentials()$info$user
    dob_str <- as.character(input$date)
    
    # Build UPDATE query manually with proper escaping
    updates <- c()
    
    if(input$textName != "") {
      updates <- c(updates, paste0("name = ", dbQuoteString(con, input$textName)))
    }
    if(dob_str != "2025-01-01") {
      updates <- c(updates, paste0("dob = '", dob_str, "'"))
    }
    if(input$textMajor != "") {
      updates <- c(updates, paste0("major = ", dbQuoteString(con, input$textMajor)))
    }
    if(input$textClass != "") {
      updates <- c(updates, paste0("class = ", dbQuoteString(con, input$textClass)))
    }
    
    # Only update if there's something to update AND the record belongs to current user
    if(length(updates) > 0) {
      query <- paste0("UPDATE updatedUsers SET ", paste(updates, collapse = ", "), 
                      " WHERE id = ", input$numID, 
                      " AND added_by = ", dbQuoteString(con, current_user))
      rows_affected <- dbExecute(con, query)
      
      # Show message if no rows were updated (record doesn't exist or doesn't belong to user)
      if(rows_affected == 0) {
        showNotification("Record not found or you don't have permission to update it.", 
                         type = "error", duration = 3)
      } else {
        showNotification("Record updated successfully!", type = "message", duration = 2)
      }
      refresh(isolate(refresh()) + 1)
    }
  })
  
  observeEvent(input$delete, {
    req(credentials()$user_auth)
    req(!is.na(input$numID))  # require ID to be filled
    
    current_user <- credentials()$info$user
    
    # Delete only if the record belongs to the current user
    query <- sqlInterpolate(con, 
                            "DELETE FROM updatedUsers WHERE id = ?id AND added_by = ?user", 
                            id = input$numID,
                            user = current_user)
    rows_affected <- dbExecute(con, query)
    
    # Show message if no rows were deleted (record doesn't exist or doesn't belong to user)
    if(rows_affected == 0) {
      showNotification("Record not found or you don't have permission to delete it.", 
                       type = "error", duration = 3)
    } else {
      showNotification("Record deleted successfully!", type = "message", duration = 2)
    }
    refresh(isolate(refresh()) + 1)  # trigger table refresh
  })
  
  # Close DB connection when app stops
  onStop(function() {
    dbDisconnect(con)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
