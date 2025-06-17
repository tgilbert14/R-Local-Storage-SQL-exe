library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(uuid)
library(shinytoastr)
library(digest)
library(shinyjs)

# Get app path
app_path <- getwd()

# Convert UUID to hexadecimal
Convert2Hex <- function(vgs5_guid) {
  guid <- gsub("[{}-]", "", vgs5_guid)
  hex_guid <- tolower(paste0(
    substr(guid, 7, 8), substr(guid, 5, 6), substr(guid, 3, 4),
    substr(guid, 1, 2), substr(guid, 11, 12), substr(guid, 9, 10),
    substr(guid, 15, 16), substr(guid, 13, 14),
    gsub("-", "", substr(guid, 17, 36))
  ))
  return(paste0("X'", hex_guid, "'"))
}
# Updating theme
updateTheme <- function(theme_css) {
  removeUI(selector = "head link#theme-style", multiple = TRUE)
  insertUI(selector = "head",
           ui = tags$link(rel = "stylesheet", type = "text/css", href = theme_css, id = "theme-style"))
}

# Determine database location
if (app_path == "C:/Users/tsgil/OneDrive/Documents/VGS - R/App-Local_SQL_electron/local_SQL_storage_electron") {
  db_loc <- file.path(app_path, "storage", "SQL_storage.db")
} else {
  db_loc <- file.path(Sys.getenv("PROGRAMDATA"), "local_SQL_storage_electron", "SQL_storage.db")
}
mydb <- dbConnect(RSQLite::SQLite(), dbname = db_loc)

# Logging
log_path <- file.path(Sys.getenv("PROGRAMDATA"), "local_SQL_storage_electron", "app_R.log")
write(paste("Connected to SQLite database at:", db_loc), file = log_path, append = TRUE)

# <-- UI -->
ui <- fluidPage(

  useToastr(),
  useShinyjs(),
  
  titlePanel("Yo yo mah"),
  uiOutput("login_ui"),
  
  # inactivity log out
  tags$script(HTML("
  var idleTimer;
  var resetTimer = function() {
    clearTimeout(idleTimer);
    idleTimer = setTimeout(function() {
      Shiny.setInputValue('auto_logout', Math.random());
    }, 3600000);  // 60 minutes
  };
  $(document).on('mousemove keydown click', resetTimer);
  resetTimer();
")),
  
  # keyboard log in with enter
  tags$script(HTML("
  $(document).on('keypress', function(e) {
    var modalIsOpen = $('.modal:visible').length > 0;
    var keyPressed = e.which || e.keyCode;
    if (keyPressed == 13 && modalIsOpen) {
      $('#login_btn').click();
    }
  });
")),
  
  # Main app content, hidden until log in
  hidden(div(id = "main_ui",
             
             tags$div(
               style = "position: absolute; top: 10px; right: 10px; z-index: 9999;",
               actionButton("open_settings", label = NULL, icon = icon("cog"),
                            class = "btn btn-default", style = "border: none;"),
               actionButton("logout_btn", label = NULL, icon = icon("sign-out-alt"),
                            class = "btn btn-default", style = "border: none; margin-left: 10px;")
             ),
             
             sidebarLayout(
               sidebarPanel(
                 textInput("subject", "Subject", ""),
                 textInput("description", "Description", ""),
                 actionButton("go", "Go!"),
                 actionButton("delete", "Delete Selected"),
                 downloadButton("download_db", "Download Database")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Subjects ->",
                            br(),
                            actionButton("open_subject_modal", "Choose Subject", icon = icon("list")),
                            textOutput("selected_subject_label")
                   ),
                   tabPanel("Storage ->",
                            br()
                   ),
                   tabPanel("Samples ->",
                            br()
                   ),
                   tabPanel("Subject Table",
                            verbatimTextOutput("distText"),
                            dataTableOutput("dataTable")
                            ),
                   

                   
                   tabPanel("Sample")
                 )
               )
             )
  ))
)

# <-- Server -->
server <- function(input, output, session) {
  logged_in <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  
  # log in UI
  login_modal <- function(prefill_user = "") {
    modalDialog(
      title = "Login",
      textInput("login_user", "Profile", value = prefill_user),
      passwordInput("login_pass", "Password"),
      tags$div(style = "margin-top: 10px;",
               actionLink("create_account", "Create a new account"),
               span("  |  "),
               actionLink("reset_password", "Forgot password?")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("login_btn", "Login")
      ),
      easyClose = FALSE
    )
  }
  
  # log in cache - check on start up
  session_dir <- tools::R_user_dir("local_SQL_storage_electron", which = "data")
  if (!dir.exists(session_dir)) dir.create(session_dir, recursive = TRUE)
  session_file <- file.path(session_dir, "session.rds")
  if (file.exists(session_file)) {
    user <- readRDS(session_file)
    current_user(user)
    logged_in(TRUE)
    
    # Fetch stored theme from the database immediately
    sql <-  "SELECT value FROM settings WHERE username = ?user_name AND key = 'theme'"
    query <- sqlInterpolate(mydb, sql, user_name = user)
    prefs <- dbGetQuery(mydb, query)
    
    if (length(prefs$value) > 0) {
      updateTheme(theme_css = prefs$value[1])
    }

    shinyjs::runjs('setTimeout(function() { $("#main_ui").show(); }, 100);')
  } else {
    showModal(login_modal())
  }
    shinyjs::show("main_ui")
  } else {
    showModal(login_modal())
  }
  
  # log in button
  observeEvent(input$login_btn, {
    req(input$login_user, input$login_pass)
    # Disable log in button immediately to avoid double clicks
    shinyjs::disable("login_btn")
    
    hash <- digest(input$login_pass, algo = "sha256")
    result <- dbGetQuery(mydb, "SELECT * FROM users WHERE username = ? AND password_hash = ?", 
                         params = list(input$login_user, hash))
    
    if (nrow(result) == 1) {
      # save log in cache
      current_user(result$username)
      saveRDS(result$username, session_file)
      logged_in(TRUE)
      removeModal()
      shinyjs::show("main_ui")
      toastr_success(paste("Welcome,", result$display_name), title = "Login")
    } else {
      toastr_error("Invalid username or password", title = "Login")
      shinyjs::enable("login_btn")  # Re-enable so user can try again
    }
  })
  
  # if logged in
  output$login_ui <- renderUI({
    if (!logged_in()) {
      showModal(login_modal())
    }
    NULL
  })
  
  # logout button
  observeEvent(input$logout_btn, {
    logged_in(FALSE)
    shinyjs::hide("main_ui")
    current_user(NULL)
    if (file.exists(session_file)) unlink(session_file)
    showModal(login_modal())
  })
  # auto logout - timer/cache
  observeEvent(input$auto_logout, {
    if (logged_in()) {
      toastr_info("Logged out due to inactivity", title = "Session Timeout")
      logged_in(FALSE)
      shinyjs::hide("main_ui")
      current_user(NULL)
      if (file.exists(session_file)) unlink(session_file)
      showModal(login_modal())
    }
  })
  
  # create new account button
  observeEvent(input$create_account, {
    removeModal()
    showModal(modalDialog(
      title = "Create New Account",
      textInput("new_user", "Username"),
      passwordInput("new_pass1", "Password"),
      passwordInput("new_pass2", "Confirm Password"),
      textInput("display_name", "Display Name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("register_btn", "Sign Up")
      ),
      easyClose = FALSE
    ))
  })
  # confirm registering account
  observeEvent(input$register_btn, {
    req(input$new_user, input$new_pass1, input$new_pass2, input$display_name)
    
    if (input$new_pass1 != input$new_pass2) {
      toastr_error("Passwords do not match.", title = "Signup Error")
      return()
    }
    # Check if username already exists
    exists <- dbGetQuery(mydb, "SELECT username FROM users WHERE username = ?", params = list(input$new_user))
    if (nrow(exists) > 0) {
      toastr_error("Username already taken.", title = "Signup Error")
      return()
    }
    hash <- digest(input$new_pass1, algo = "sha256")
    dbExecute(mydb, "INSERT INTO users (username, password_hash, display_name) VALUES (?, ?, ?)",
              params = list(input$new_user, hash, input$display_name))
    
    toastr_success("Account created! Please log in.", title = "Success")
    removeModal()
    showModal(login_modal(prefill_user = input$new_user))
    
    # Trigger log in modal again
    showModal(modalDialog(
      title = "Login",
      textInput("login_user", "Username", value = input$new_user),
      passwordInput("login_pass", "Password"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("login_btn", "Login")
      ),
      easyClose = FALSE
    ))
  })
  
  # reset password
  observeEvent(input$reset_password, {
    removeModal()
    showModal(modalDialog(
      title = "Reset Password",
      textInput("reset_user", "Username"),
      passwordInput("new_pass1", "New Password"),
      passwordInput("new_pass2", "Confirm New Password"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Reset Password")
      ),
      easyClose = FALSE
    ))
  })
  observeEvent(input$confirm_reset, {
    if (input$new_pass1 != input$new_pass2) {
      toastr_error("Passwords do not match.", title = "Reset Error")
      return()
    }
    hash <- digest(input$new_pass1, algo = "sha256")
    updated <- dbExecute(mydb, "UPDATE users SET password_hash = ? WHERE username = ?", 
                         params = list(hash, input$reset_user))
    if (updated == 1) {
      removeModal()
      toastr_success("Password updated!", title = "Success")
      showModal(modalDialog(
        title = "Login",
        textInput("login_user", "Username"),
        passwordInput("login_pass", "Password"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("login_btn", "Login")
        ),
        easyClose = FALSE
      ))
    } else {
      toastr_error("Username not found.", title = "Reset Error")
    }
  })
  
  # settings button
  observeEvent(input$open_settings, {
    user <- current_user()
    prefs <- dbGetQuery(mydb, "SELECT key, value FROM settings WHERE username = ?", params = list(user))
    
    get_val <- function(key, default = NULL) {
      val <- prefs$value[prefs$key == key]
      if (length(val)) val else default
    }
    
    showModal(modalDialog(
      title = "Settings",
      checkboxInput("show_welcome", "Show welcome message on startup", value = get_val("show_welcome", "FALSE") == "TRUE"),
      selectInput("theme_choice", "Theme", choices = c("Light", "Dark", "System"), selected = get_val("theme", "System")),
      selectInput("coordinate_system", "Coordinate System", choices = c("Decimal Degrees", "DMS", "UTM"), selected = get_val("coordinate_system", "Decimal Degrees")),
      selectInput("language", "Language", choices = c("English", "Spanish", "French"), selected = get_val("language", "English")),
      checkboxInput("code_display", "Enable Code Display", value = get_val("code_display", "FALSE") == "TRUE"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_settings", "Save")
      ),
      easyClose = TRUE
    ))
  })
  
  # save settings
  observeEvent(input$save_settings, {
    user <- current_user()
    settings <- list(
      show_welcome = as.character(input$show_welcome),
      theme = input$theme_choice,
      coordinate_system = input$coordinate_system,
      language = input$language,
      code_display = as.character(input$code_display)
    )
    
    for (k in names(settings)) {
      dbExecute(mydb,
                "INSERT OR REPLACE INTO settings (username, key, value) VALUES (?, ?, ?)",
                params = list(user, k, settings[[k]])
      )
    }
    
    toastr_success("Settings saved!", title = "Preferences")
    removeModal()
  })
  
  # update for theme choice
  observeEvent(input$theme_choice, {
    theme_css <- switch(input$theme_choice,
                        "Light" = "light-theme.css",
                        "Dark" = "dark-theme.css",
                        "System" = ifelse(Sys.info()["sysname"] == "Windows", "light-theme.css", "dark-theme.css"))
    
    updateTheme(theme_css)
  })
  
  # get data
  data <- reactive({
    dbGetQuery(mydb, "SELECT quote(pk_subject), name_subject, description_subject FROM Subject")
  })
  
  # get Subjects in database
  get_subject_choices <- function() {
    subjects <- dbGetQuery(mydb, "SELECT quote(pk_subject), name_subject FROM subject
    ORDER BY name_subject COLLATE NOCASE")
    if (nrow(subjects) == 0) {
      return(NULL)
    }
    unique(subjects$name_subject)
  }
  
  # open subject modal
  observeEvent(input$open_subject_modal, {
    showModal(modalDialog(
      title = "Select or Add Subject",
      selectInput("subject_choice", "Existing Subjects", choices = get_subject_choices()),
      textInput("new_subject", "Or create a new subject"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submit_subject", "OK")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$submit_subject, {
    subj <- input$subject_choice
    new_subj <- input$new_subject
    
    if (nzchar(new_subj)) {
      existing <- dbGetQuery(mydb, "SELECT name_subject FROM subject WHERE name_subject = ?", params = list(new_subj))
      if (nrow(existing) == 0) {
        pk_guid <- Convert2Hex(UUIDgenerate())

        sql <-  "INSERT INTO subject (pk_subject, name_subject) VALUES (?pk_subject, ?value)"
        #stop(sql)
        
        
        query <- sqlInterpolate(mydb, sql, pk_subject = SQL(pk_guid), value = new_subj)
        result <- tryCatch(dbExecute(mydb, query), error = function(e) 0)
        if (result == 1) {
          toastr_success("Insert successful!", title = "Database")
          output$distText <- renderPrint({
            cat("Inserted Subject:", input$subject, "\nInserted Description:", input$description, "\n")
          })
        }

        subj <- pk_guid
      } else {
        subj <- dbGetQuery(mydb, "SELECT pk_subject FROM subject WHERE name_subject = ?", params = list(new_subj))$pk_subject[1]
      }
    }
    
    # output$selected_subject_label <- renderText({
    #   name <- dbGetQuery(mydb, "SELECT name_subject FROM subject WHERE pk_subject = ?", params = list(subj))$name_subject[1]
    #   paste("Selected Subject:", name)
    # })
    
    removeModal()
  })
  
  # initial table render
  output$dataTable <- renderDataTable({
    d <- data()
    d <- d[names(d) != "quote(pk_subject)"]
    datatable(d, selection = "single")
  })
  
  # go button
  observeEvent(input$go, {
    sub_data <- dbGetQuery(mydb, "SELECT name_subject FROM Subject")
    if (input$subject %in% sub_data$name_subject) {
      toastr_error(paste0("Subject '", input$subject, "' already exists"), title = "Database")
      output$distText <- renderPrint({ cat("Subject already exists\n") })
    } else {
      pk_guid <- Convert2Hex(UUIDgenerate())
      sql <- "INSERT INTO Subject (pk_subject, name_subject, description_subject) VALUES (?pk_subject, ?value, ?text)"
      query <- sqlInterpolate(mydb, sql, pk_subject = SQL(pk_guid), value = input$subject, text = input$description)
      result <- tryCatch(dbExecute(mydb, query), error = function(e) 0)
      
      if (result == 1) {
        toastr_success("Insert successful!", title = "Database")
        output$distText <- renderPrint({
          cat("Inserted Subject:", input$subject, "\nInserted Description:", input$description, "\n")
        })
      }
    }
    output$dataTable <- renderDataTable({
      data.t <- DBI::dbReadTable(mydb, "subject")
      data.view <- data.t[names(data.t) != "pk_subject"]
      datatable(data.view, selection = "single")
    })
  })
  
  # delete entry button
  observeEvent(input$delete, {
    db.data <- dbGetQuery(mydb, "SELECT quote(pk_subject), name_subject FROM Subject")
    row <- input$dataTable_rows_selected
    if (length(row) == 0) {
      toastr_error("No row selected", title = "Database")
      return()
    }
    pk <- db.data[row, "quote(pk_subject)"]
    del_query <- paste0("DELETE FROM subject WHERE pk_subject = ", pk)
    output$distText <- renderPrint({ cat("Deleting subject:", pk, "\n") })
    result <- tryCatch(dbExecute(mydb, del_query), error = function(e) 0)
    
    if (result == 1) {
      toastr_success("Deleted successfully!", title = "Database")
      output$dataTable <- renderDataTable({
        data.t <- DBI::dbReadTable(mydb, "subject")
        data.view <- data.t[names(data.t) != "pk_subject"]
        datatable(data.view, selection = "single")
      })
    }
  })
  
  # download button
  output$download_db <- downloadHandler(
    filename = function() {
      paste0("SQL_storage_", Sys.Date(), ".db")
    },
    content = function(file) {
      file.copy(db_loc, file, overwrite = TRUE)
    },
    contentType = "application/octet-stream"
  )
  
  # disconnect database on session end
  session$onSessionEnded(function() {
    dbDisconnect(mydb)
  })
  
}

# run app
shinyApp(ui = ui, server = server)