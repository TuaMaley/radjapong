library(shiny)
library(shinydashboard)
library(DBI)
library(RMySQL)
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(pool)
library(bit64)
library(aws.s3)
library(shinyjs)
library(shinyWidgets)
library(bslib)

# AWS S3 Configuration
Sys.setenv(
  AWS_ACCESS_KEY_ID = "AKIASVLKCNXMJIQP",  
  AWS_SECRET_ACCESS_KEY = "HIDDEN",  
  AWS_DEFAULT_REGION = "us-east-2"
)

# Database Connection Pool
db_pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "smu_campus_connect",
  host = "ryanadjapong.clogay4kuwnd.us-east-2.rds.amazonaws.com",
  port = 3306,
  user = "DrRyan",
  password = "NAnakwame1986"
)

# Ensure pool is closed when app stops
onStop(function() {
  poolClose(db_pool)
})

# Helper Function: Query Database.
query_database <- function(sql_query, params = NULL) {
  tryCatch({
    if (is.null(params)) {
      dbGetQuery(db_pool, sql_query)
    } else {
      dbGetQuery(db_pool, sql_query, params = params)
    }
  }, error = function(e) {
    stop(paste("Database query error:", e$message))
  })
}

# Upload Image to S3
upload_to_s3 <- function(file_path, bucket_name, object_key) {
  tryCatch({
    s3_url <- sprintf("https://%s.s3.%s.amazonaws.com/%s", bucket_name, Sys.getenv("AWS_DEFAULT_REGION"), object_key)
    result <- put_object(
      file = file_path,
      object = object_key,
      bucket = bucket_name,
      headers = list(`x-amz-acl` = "public-read")
    )
    if (!result) stop("Upload to S3 failed.")
    return(s3_url)
  }, error = function(e) {
    stop(paste("Error during S3 upload:", e$message))
  })
}

# Helper Function: Query ChatGPT
query_chatgpt <- function(user_query) {
  api_key <- "sk-proj--"  
  url <- "https://api.openai.com/v1/chat/completions"
  body <- list(
    model = "gpt-4o",
    messages = list(
      list(role = "system", content = "You are a helpful assistant that translates natural language questions into SQL queries for the given database schema.
Here is the database schema:

Table: Users
  Columns: user_id (INT, PK, AUTO_INCREMENT), smu_id (VARCHAR(20), UNIQUE, NOT NULL), name (VARCHAR(100), NOT NULL), race (VARCHAR(50)), age (INT), photo_path (VARCHAR(255)), gender (ENUM('Male','Female','Other')), password (VARCHAR(255), NOT NULL), academic_level (ENUM('Undergraduate','Graduate') NOT NULL), created_at (TIMESTAMP DEFAULT CURRENT_TIMESTAMP)

Table: User_Verification
  Columns: verification_id (INT, PK, AUTO_INCREMENT), user_id (INT, NOT NULL), verified (BOOLEAN DEFAULT FALSE), verification_date (TIMESTAMP), FOREIGN KEY (user_id) REFERENCES Users(user_id)

Table: Preferences
  Columns: preference_id (INT, PK, AUTO_INCREMENT), user_id (INT, NOT NULL), pref_gender (ENUM('Male','Female','Other')), pref_age_min (INT), pref_age_max (INT), pref_race (VARCHAR(50)), FOREIGN KEY (user_id) REFERENCES Users(user_id)

Table: Matches
  Columns: id (INT, PK, AUTO_INCREMENT), smu_id (VARCHAR(255) NOT NULL), matched_user_id (VARCHAR(255) NOT NULL), matched_at (TIMESTAMP DEFAULT CURRENT_TIMESTAMP), FOREIGN KEY (smu_id) REFERENCES Users(smu_id) ON DELETE CASCADE, FOREIGN KEY (matched_user_id) REFERENCES Users(smu_id) ON DELETE CASCADE

Table: Messages
  Columns: message_id (INT, PK, AUTO_INCREMENT), sender_id (INT, NOT NULL), receiver_id (INT, NOT NULL), message_content (TEXT NOT NULL), sent_at (TIMESTAMP DEFAULT CURRENT_TIMESTAMP), FOREIGN KEY (sender_id) REFERENCES Users(user_id), FOREIGN KEY (receiver_id) REFERENCES Users(user_id)

Table: Message_Permissions
  Columns: permission_id (INT, PK, AUTO_INCREMENT), user_id (INT, NOT NULL), liked_by_user_id (INT, NOT NULL), can_message (BOOLEAN DEFAULT FALSE), FOREIGN KEY (user_id) REFERENCES Users(user_id), FOREIGN KEY (liked_by_user_id) REFERENCES Users(user_id)

Table: Dating_Tips
  Columns: tip_id (INT, PK, AUTO_INCREMENT), category (VARCHAR(50)), content (TEXT NOT NULL), created_at (TIMESTAMP DEFAULT CURRENT_TIMESTAMP), updated_at (TIMESTAMP)

Table: Likes
  Columns: like_id (INT, PK, AUTO_INCREMENT), user_id (INT NOT NULL), liked_user_id (INT NOT NULL), like_timestamp (TIMESTAMP DEFAULT CURRENT_TIMESTAMP), FOREIGN KEY (user_id) REFERENCES Users(user_id) ON DELETE CASCADE, FOREIGN KEY (liked_user_id) REFERENCES Users(user_id) ON DELETE CASCADE

Return only SQL queries without explanations. Avoid any changes to the database. If you don't find answer in the database, answer from what you understand about the query."),
      list(role = "user", content = user_query)
    )
  )
  response <- POST(
    url,
    add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "json",
    timeout(60)
  )
  if (status_code(response) == 200) {
    content <- content(response, as = "parsed", simplifyVector = FALSE)
    content$choices[[1]]$message$content
  } else {
    error_message <- content(response, as = "text", encoding = "UTF-8")
    stop(paste("ChatGPT API Error:", error_message))
  }
}




ui <- fluidPage(
  theme = bs_theme(
    bg = "#0a2e4f",  # Dark black background
    fg = "#eeeeee",  # Light gray text
    primary = "#eeeeee",  # Primary color
    base_font = font_google("Quicksand"),
    heading_font = font_google("Pacifico")
  ),
  
  useShinyjs(),  # Enable Shinyjs
  
  # Dynamic Image JavaScript#
  tags$head(
    tags$script(HTML("
      let images = [
        'https://smucampusconnect.s3.us-east-2.amazonaws.com/users/connect.jpeg',
        'https://smucampusconnect.s3.us-east-2.amazonaws.com/users/smudate.jpeg'
      ];
      let index = 0;
      
      function changeImage() {
        const imgElement = document.getElementById('dynamic_image');
        index = (index + 1) % images.length;
        imgElement.src = images[index];
      }
      
      setInterval(changeImage, 4000); // Change image every 3 seconds
    "))
  ),
  
  # Layout
  tags$div(
    style = "display: flex; height: 100vh;",
    
    # Main Content Area
    tags$div(
      style = "flex: 1; padding: 20px; background-color: #0a2e4f; color: #eeeeee;",
      fluidRow(
        # Header Section with Logo and Title
        column(
          9,
          div(
            style = "display: flex; align-items: center;",
            # Add Logo
            img(
              src = "https://smucampusconnect.s3.us-east-2.amazonaws.com/users/logosmu.jpg",  # Replace with your logo URL
              height = "80px",
              alt = "SMU Logo",
              style = "margin-right: 10px;"
            ),
            # Add Title
            h2("Welcome to SMU Campus Connect", 
               style = "font-family: 'Brush Script MT'; font-size: 50px; color: #cc0000;")
          )
        ),
        column(
          3,
          div(
            style = "display: flex; justify-content: flex-end; align-items: center; gap: 10px;",  # Use gap for spacing
            # Help Button
            actionButton(
              "help_btn", 
              NULL, 
              width = "100px",  # Uniform width
              icon = icon("question-circle"), 
              style = "color: white; border: 0.5px solid #eeeeee; padding: 10px 20px; font-size: 16px; border-radius: 5px; background-color: #007bff;"
            ),
            # Logout Button
            actionButton(
              "logout_btn", 
              "Logout", 
              width = "100px",  # Uniform width
              style = "color: white; border: 0.5px solid #eeeeee; padding: 10px 20px; font-size: 16px; border-radius: 5px; background-color: #dc3545;"
            )
          )
        )
      ),
      hr(),
      
      
      # Tab Panel
      tabsetPanel(
        id = "tabs",
        
        # Welcome Tab
        tabPanel(
          "Welcome",
          div(
            style = "display: flex; flex-direction: row; justify-content: space-between; align-items: center; padding: 50px;",
            
            # Left Content
            div(
              style = "flex: 1; height: 400px; color: white;",
              
              # Main Text Content
              div(
                style = "text-align: center;",
                h2(
                  span("SMU", style = "color: #FF0000;"), 
                  span("CampusConnect", style = "color: white;"),
                  style = "font-family: 'Arial Black', sans-serif; font-size: 50px; margin: 0;"
                ),
                h1("Get Who Gets You", style = "font-family: 'Impact', sans-serif; font-size: 60px; color: white; margin: 10px 0;"),
                h3("Start", 
                   style = "font-family: 'Arial', sans-serif; font-size: 30px; color: white; display: inline;"),
                h3("free today", 
                   style = "font-family: 'Arial', sans-serif; font-size: 30px; color: #FF4500; display: inline; margin-left: 5px;"),
                br(),
                # Hyperlink for Sign Up and Get Connected
                tags$a(
                  "Sign up and get connected", 
                  href = "javascript:void(0);",  # JavaScript handler for navigation
                  onclick = "Shiny.setInputValue('navigate_to_signup', Math.random());", # Use a unique value each time
                  style = "font-family: 'Arial', sans-serif; font-size: 25px; color: #00FFFF; margin-top: 10px; text-decoration: underline; cursor: pointer;"
                )
              )
            ),
            
            # Right Content (Dynamic Image)
            div(
              style = "flex: 0; text-align: right; border: 4px solid #3c8dbc;",
              img(
                id = "dynamic_image",
                src = "https://smucampusconnect.s3.us-east-2.amazonaws.com/users/smudate.jpeg",
                alt = "Dynamic Image",
                height = "400px"
              )
            )
          ),
          
          # Spacer for separation
          div(
            style = "height: 70px;"  # Adds spacing above the Data Privacy Statement
          ),
          
          div(
            style = "font-size: 12px; margin-top: 20px; position: absolute; bottom: 0px; width: 100%;",
            p("Data Privacy Statement: This is the sole product of SMU. Your data will be used strictly for the purpose of signing up and matching with other users. We respect your privacy.")
          )
        ),
        
        
        # Login Tab
        tabPanel("Login", 
                 textInput("login_id", "SMU ID"),
                 uiOutput('PasswordInput2'),
                 actionButton("login_btn", "Login", 
                              style = "color: white; border: 0.2px solid #eeeeee; padding: 10px 20px; font-size: 16px; border-radius: 1px;"),
                 verbatimTextOutput("login_message")
        ),
        
        # Sign Up Tab
        tabPanel("Sign Up", 
                 div(
                   id = "error_message", 
                   htmlOutput("signup_error_message"), 
                   style = "color: red; font-weight: bold; font-size: 16px; margin-bottom: 20px;"
                 ),
                 textInput("signup_id", "SMU ID"),
                 textInput("signup_name", "Name"),
                 numericInput("signup_age", "Age", value = 18, min = 18, max = 80),
                 selectInput("signup_academic_level", "Academic Level", choices = c("", "Undergraduate", "Graduate"), selected = ""),
                 selectInput("signup_race", "Race", choices = c("Hispanic", "White", "Black", "Asian")),
                 textAreaInput("signup_bio_details", "Bio", placeholder = "Tell us something about yourself...", rows = 5, cols = 100),
                 uiOutput('fileInput'),
                 radioButtons("signup_gender", "Gender", choices = c("Male", "Female")),
                 uiOutput('PasswordInput'),
                 h4("Dating Preferences", style = "color: white;"),
                 radioButtons("pref_gender", "Preferred Gender", choices = c("Male", "Female")),
                 sliderInput("pref_age", "Preferred Age Range", min = 18, max = 80, value = c(25, 35)),
                 selectInput("pref_race", "Preferred Race", choices = c("Hispanic", "White", "Black", "Asian")),
                 actionButton("signup_btn", "Sign Up",
                              style = "color: white; border: 0.2px solid #eeeeee; padding: 10px 20px; font-size: 16px; border-radius: 1px;"),
                 verbatimTextOutput("signup_message")
        ),
        
        # Profile Tab
        tabPanel(
          "Profile", 
          div(
            style = "position: relative;",
            
            # Unsubscribe Button at the Top Right Corner
            actionButton(
              "unsubscribe_btn", 
              "Unsubscribe", 
              style = "position: absolute; top: 10px; right: 10px; color: white; border: 0.2px solid #eeeeee; padding: 10px 20px; font-size: 16px; border-radius: 1px; background-color: #dc3545;"
            ),
            
            # User Profile Information
            uiOutput("profile_display"),  # Display user's profile information
            br(),                         # Add space before the button
            
            # Find Matches Button
            actionButton(
              "find_matches_btn", 
              "Find Matches",  
              style = "color: white; border: 0.2px solid #eeeeee; padding: 10px 10px; font-size: 16px; border-radius: 1px; margin-top: 20px;"
            ), 
            br(), br(),                   # Add more space after the button
            
            # Section for Matches
            h4(
              "Your Matches", 
              style = "font-family: 'Luminari, fantasy'; font-size: 20px; padding-top: 5px;"
            ),  # Section header for matches
            
            # Matches Display Placeholder
            uiOutput("matches_display")  # Placeholder for dynamic matches display
          )
        ),
        
        
        # Matches Tab
        tabPanel("Matches", 
                 uiOutput("match_tab_display"),
                 uiOutput("match_message_status")
        ),
        
        # Preferences Tab
        tabPanel("Preferences", 
                 h3("Edit Your Preferences", style = "font-family: 'Luminari, fantasy'; font-size: 20px;"),
                 radioButtons("edit_pref_gender", "Preferred Gender", choices = c("Male", "Female"), selected = NULL),
                 sliderInput("edit_pref_age", "Preferred Age Range", min = 18, max = 80, value = c(25, 35)),
                 selectInput("edit_pref_race", "Preferred Race", choices = c("Hispanic", "White", "Black", "Asian"), selected = NULL),
                 selectInput("edit_pref_academic_level", "Preferred Academic Level", choices = c("Undergraduate", "Graduate"), selected = NULL),
                 actionButton("save_preferences_btn", "Save Preferences", 
                              style = "color: white; border: 0.2px solid #eeeeee; padding: 10px 20px; font-size: 16px; border-radius: 1px;"),
                 verbatimTextOutput("preferences_message")
        ),
        
        # Dating Buddy Tab
        tabPanel("Dating Buddy", 
                 textAreaInput("user_query", "How can I help you today?", ""),
                 actionButton("ask_btn", "Ask", 
                              style = "color: white; border: 0.2px solid #eeeeee; padding: 10px 20px; font-size: 16px; border-radius: 1px;"),
                 verbatimTextOutput("response_type"),
                 verbatimTextOutput("generated_sql"),
                 tableOutput("query_result"),
                 verbatimTextOutput("general_answer")
        ),
        
        # Dashboard Tab
        tabPanel("Dashboard", 
                 selectInput("analytics_type", "Select Analytics Type:", choices = c(
                   "User Demographics",
                   "Age Distribution",
                   "Preferences Analysis"
                 )),
                 actionButton("run_analytics", "Run Analytics", 
                              style = "color: white; border: 0.2px solid #eeeeee; padding: 10px 20px; font-size: 16px; border-radius: 1px;"),
                 plotOutput("analytics_plot"),
                 tableOutput("analytics_table")
        ),
        
        # Logout Tab
        tabPanel("Logout", 
                 h3("You have been logged out.", style = "color:  #eeeeee; text-align: center;"),
                 actionButton("login_again_btn", "Login Again", 
                              style = "color: white; border: 0.2px solid #eeeeee; padding: 10px 20px; font-size: 16px; border-radius: 1px;")
        )
      )
    )
  )
)


# Server logic
server <- function(input, output, session) {
  # Initialize user_data globally
  user_data <- reactiveValues(data = NULL, matched_users = list())
  
  output$fileInput <- renderUI({
    fileInput("signup_photo", "Upload Photo", accept = c("image/png", "image/jpeg"))
  })
  
  output$PasswordInput <- renderUI({
    passwordInput("signup_password", "Password")
  })
  
  output$PasswordInput2 <- renderUI({
    passwordInput("login_password", "Password")
  })
  
  # Sign-up functionality
  observeEvent(input$signup_btn, {
    tryCatch({
      # Check for missing fields
      missing_fields <- c(
        if (is.null(input$signup_id) || input$signup_id == "") "SMU ID",
        if (is.null(input$signup_name) || input$signup_name == "") "Name",
        if (is.null(input$signup_age)) "Age",
        if (is.null(input$signup_academic_level) || input$signup_academic_level == "") "Academic Level",
        if (is.null(input$signup_race) || input$signup_race == "") "Race",
        if (is.null(input$signup_photo)) "Photo",
        if (is.null(input$signup_gender) || input$signup_gender == "") "Gender",
        if (is.null(input$signup_password) || input$signup_password == "") "Password",
        if (is.null(input$pref_gender) || input$pref_gender == "") "Preferred Gender",
        if (is.null(input$pref_age) || length(input$pref_age) < 2) "Preferred Age Range",
        if (is.null(input$pref_race) || input$pref_race == "") "Preferred Race",
        if (is.null(input$signup_bio_details) || input$signup_bio_details == "") "Bio"
      )
      
      # If any fields are missing, display an error message
      if (length(missing_fields) > 0) {
        output$signup_error_message <- renderUI({
          paste("All fields must be completed. Missing fields:", paste(missing_fields, collapse = ", "))
        })
        return() # Stop further execution
      }
      
      # Extract preferred age range from sliderInput
      pref_age_min <- input$pref_age[1]
      pref_age_max <- input$pref_age[2]
      
      # Clear any previous error messages
      output$signup_error_message <- renderUI({ NULL })
      
      # Process form if complete
      temp_file_path <- tempfile(fileext = ".jpg")
      file.copy(input$signup_photo$datapath, temp_file_path)
      photo_url <- upload_to_s3(temp_file_path, "smucampusconnect", paste0("users/", input$signup_id, ".jpg"))
      
      # Insert user details into the Users table
      dbExecute(db_pool, paste0(
        "INSERT INTO Users (smu_id, name, age, academic_level, race, photo_path, gender, password, bio_details) VALUES ('",
        input$signup_id, "', '", input$signup_name, "', ", input$signup_age, ", '", input$signup_academic_level, "', '",
        input$signup_race, "', '", photo_url, "', '", input$signup_gender, "', '", input$signup_password, "', '",
        input$signup_bio_details, "')"
      ))
      
      
      new_user_id <- dbGetQuery(db_pool, "SELECT LAST_INSERT_ID() AS user_id")$user_id[1]
      
      # Insert preferences into Preferences table
      query <- paste0(
        "INSERT INTO Preferences (user_id, pref_gender, pref_age_min, pref_age_max, pref_race) VALUES ('",
        new_user_id, "', '", input$pref_gender, "', ", pref_age_min, ", ", pref_age_max, ", '",
        input$pref_race, "')"
      )
      print(query)  # Debugging: Print the query to the console
      dbExecute(db_pool, query)
      
      # Show success message
      #output$signup_message <- renderText("Sign-up successful! You can now log in.")
      
      #Karshu - Start
      updateTextInput(session, "signup_id", value = "")
      updateTextInput(session, "signup_name", value = "")
      updateNumericInput(session, "signup_age", value = 18, min = 18, max = 80)
      updateSelectInput(session, "signup_academic_level", selected = "")
      updateSelectInput(session, "signup_race", selected = "Hispanic")
      output$fileInput <- renderUI({
        fileInput("signup_photo", "Upload Photo", accept = c("image/png", "image/jpeg"))
      })
      updateRadioButtons(session, "signup_gender", selected = "Male")
      output$PasswordInput <- renderUI({
        passwordInput("signup_password", "Password")
      })
      updateRadioButtons(session, "pref_gender", selected = "Male")
      updateSelectInput(session, "pref_race", selected = "Hispanic")
      updateTextAreaInput(session, "signup_bio_details", value = "")
      updateSliderInput(session, "pref_age", min = 18, max = 80, value = c(25, 35))
      
      Sys.sleep(2)
      # updateTabItems(session, "tabs", selected = "login")
      updateTabsetPanel(session, "tabs", selected = "Login")
      
      #Karshu - End
      
    }, error = function(e) {
      # Handle any errors during the process
      output$signup_error_message <- renderUI({ paste("Error:", e$message) })
    })
  })
  
  # Handle navigation to the signup tab
  observeEvent(input$navigate_to_signup, {
    updateTabsetPanel(session, "tabs", selected = "Sign Up")
  })
  
  
  
  # Reset Login Inputs on Login Tab Click#
  observeEvent(input$tabs, {
    if (input$tabs == "login") {
      # Clear input fields
      updateTextInput(session, "login_id", value = "")
      updateTextInput(session, "login_password", value = "")
    }
  })
  
  # Login Functionality
  observeEvent(input$login_btn, {
    output$matches_display <- renderUI(NULL) #Karshu
    req(input$login_id, input$login_password)
    
    # Query to fetch user details
    user <- query_database(paste0(
      "SELECT * FROM Users WHERE smu_id = '", input$login_id, "' AND password = '", input$login_password, "'"
    ))
    
    # Check if user exists
    if (is.null(user) || nrow(user) == 0) {
      output$login_message <- renderText("Invalid SMU ID or Password.")
      return()
    }
    
    # If user exists, store the logged-in user's data
    user_data$data <- user[1, ]
    user_data$matched_users <- list()  # Reset matched_users
    
    # Render the user's profile on the Profile Page
    output$profile_display <- renderUI({
      user <- user_data$data
      div(
        p(paste("Name:", user$name)),
        p(paste("Age:", user$age)),
        p(paste("Race:", user$race)),
        p(paste("Academic Level:", user$academic_level)),
        p(paste("Gender:", user$gender)),
        p(paste("Bio:", user$bio_details)),
        tags$img(src = user$photo_path, height = "150px", alt = "Profile Picture")
      )
    })
    
    # Fetch matches with message status
    matches_query <- paste0(
      "SELECT u.*, COALESCE(m.status, 'not_sent') AS message_status ",
      "FROM Matches mt ",
      "JOIN Users u ON mt.matched_user_id = u.smu_id ",
      "LEFT JOIN (SELECT sender_id, receiver_id, MAX(sent_at) AS latest_message_time, status ",
      "           FROM Messages WHERE sender_id = ", user$user_id, " ",
      "           GROUP BY sender_id, receiver_id) m ",
      "ON mt.matched_user_id = m.receiver_id AND mt.smu_id = m.sender_id ",
      "WHERE mt.smu_id = '", user_data$data$smu_id, "'"
    )
    matches <- query_database(matches_query)
    
    # Safeguard against NULL or invalid matches
    if (!is.null(matches) && is.data.frame(matches) && nrow(matches) > 0) {
      user_data$matched_users <- lapply(seq_len(nrow(matches)), function(i) matches[i, ])
    } else {
      user_data$matched_users <- list()  # Ensure matched_users is an empty list
    }
    
    # Update Matches Tab display
    output$match_tab_display <- renderUI({
      if (length(user_data$matched_users) > 0) {
        div(
          lapply(user_data$matched_users, function(matched_user) {
            status <- matched_user$message_status  # Retrieve message status
            div(
              style = "margin-bottom: 20px; border: 1px solid #ccc; padding: 10px;",
              tags$img(src = matched_user$photo_path, height = "100px", alt = "Match Picture"),
              p(paste("Name:", matched_user$name)),
              p(paste("Age:", matched_user$age)),
              p(paste("Race:", matched_user$race)),
              p(paste("Gender:", matched_user$gender)),
              p(paste("Bio:", matched_user$bio_details)),
              textInput(
                inputId = paste0("message_", matched_user$smu_id),
                label = "Send a message:",
                value = "",
                placeholder = ifelse(status == "sent", "Message Sent", "")
              ),
              actionButton(
                inputId = paste0("send_", matched_user$smu_id),
                label = "Send",
                disabled = ifelse(status == "sent", TRUE, FALSE)
              ),
              uiOutput(paste0("status_", matched_user$smu_id))  # Display message status
            )
          })
        )
      } else {
        h4("No matches found.")
      }
    })
    
    # Navigate to Profile tab on successful login
    output$login_message <- renderText("Login successful!")
    updateTabsetPanel(session, "tabs", selected = "Profile")
  })
  
  
  
  # Matches Functionality
  observeEvent(input$find_matches_btn, {
    req(user_data$data)
    user <- user_data$data
    
    preferences <- query_database(
      paste0("SELECT * FROM Preferences WHERE user_id = '", user$user_id, "'")
    )
    
    if (nrow(preferences) == 0) {
      output$matches_display <- renderUI({
        h4("No preferences found. Please update your preferences.")
      })
      return()
    }
    
    # Query for matches
    query <- paste0(
      "SELECT * FROM Users WHERE gender = '", preferences$pref_gender, 
      "' AND age BETWEEN ", preferences$pref_age_min, " AND ", preferences$pref_age_max, 
      " AND race = '", preferences$pref_race, "' AND smu_id != '", user$smu_id, "'",
      " AND smu_id NOT IN (SELECT matched_user_id FROM Matches WHERE smu_id = '", user$smu_id, "')"
    )
    matches <- query_database(query)
    
    if (!is.null(matches) && nrow(matches) > 0) {
      output$matches_display <- renderUI({
        div(
          lapply(seq_len(nrow(matches)), function(i) {
            match <- matches[i, ]
            div(
              id = paste0("match_", match$smu_id),  # Unique ID for each match
              style = "margin-bottom: 20px; border: 1px solid #ccc; padding: 10px;",
              tags$img(src = match$photo_path, height = "100px", alt = "Match Picture"),
              p(paste("Name:", match$name)),
              p(paste("Age:", match$age)),
              p(paste("Race:", match$race)),
              p(paste("Academic level:", match$academic_level)), # Match's academic level
              p(paste("Gender:", match$gender)),
              p(paste("Bio:", match$bio_details)), # Match's bio
              actionButton(
                paste0("like_", match$smu_id), 
                "Like", 
                style = "background-color: green; color: white; font-weight: bold; padding: 10px; border-radius: 5px;"
              ),
              actionButton(
                paste0("dislike_", match$smu_id), 
                "Dislike", 
                style = "background-color: red; color: white; font-weight: bold; padding: 10px; border-radius: 5px;"
              )
            )
          })
        )
      })
      
      lapply(seq_len(nrow(matches)), function(i) {
        match <- matches[i, ]
        
        # Like Button
        observeEvent(input[[paste0("like_", match$smu_id)]], {
          # Add the liked profile to matched_users
          user_data$matched_users <- c(user_data$matched_users, list(match))
          
          # Insert match into the Matches table in MySQL
          tryCatch({
            query <- paste0(
              "INSERT INTO Matches (smu_id, matched_user_id) VALUES ('",
              user_data$data$smu_id, "', '", match$smu_id, "')"
            )
            dbExecute(db_pool, query)
          }, error = function(e) {
            print(paste("Error inserting match:", e$message))
          })
          
          # Update Matches Tab display
          output$match_tab_display <- renderUI({
            div(
              lapply(user_data$matched_users, function(matched_user) {
                div(
                  style = "margin-bottom: 20px; border: 1px solid #ccc; padding: 10px;",
                  tags$img(src = matched_user$photo_path, height = "100px", alt = "Match Picture"),
                  p(paste("Name:", matched_user$name)),
                  p(paste("Age:", matched_user$age)),
                  p(paste("Race:", matched_user$race)),
                  p(paste("Academic level:", matched_user$academic_level)), # Matched user's academic level
                  p(paste("Gender:", matched_user$gender)),
                  p(paste("Bio:", matched_user$bio_details)), # Matched user's bio
                  textInput(inputId = paste0("message_", matched_user$smu_id), label = "Send a message:", value = ""),
                  actionButton(
                    inputId = paste0("send_", matched_user$smu_id),
                    label = "Send",
                    style = "background-color: green; color: white; border: none; padding: 10px 20px; font-size: 14px; border-radius: 4px;"
                  )
                )
              })
            )
          })
          
          # Remove the liked profile from Profile Page
          removeUI(selector = paste0("#match_", match$smu_id))
        })
        
        # Dislike Button
        observeEvent(input[[paste0("dislike_", match$smu_id)]], {
          # Remove Disliked Profile from Profile Page
          removeUI(selector = paste0("#match_", match$smu_id))
        })
      })
    } else {
      output$matches_display <- renderUI({
        h4("No matches found.")
      })
    }
  })
  
  
  # Messaging Functionality
  observe({
    user_matches <- user_data$matched_users
    req(user_matches)  # Ensure matches exist
    
    # Create observers for each matched user
    lapply(user_matches, function(matched_user) {
      btn_id <- paste0("send_", matched_user$smu_id)
      text_box_id <- paste0("message_", matched_user$smu_id)
      status_id <- paste0("status_", matched_user$smu_id)
      
      # Create observer for the send button
      observeEvent(input[[btn_id]], {
        message <- input[[text_box_id]]
        
        if (!is.null(message) && message != "") {
          tryCatch({
            # Escape the message content manually
            safe_message <- dbQuoteString(db_pool, message)
            
            # Insert message into the Messages table
            query <- paste0(
              "INSERT INTO Messages (sender_id, receiver_id, message_content, status, sent_at) VALUES (",
              user_data$data$user_id, ", ", matched_user$user_id, ", ", safe_message, ", 'sent', NOW())"
            )
            dbExecute(db_pool, query)
            print(paste("Message sent to", matched_user$smu_id, ":", message))
            
            # Deactivate text box and show "Message Sent" status
            updateTextInput(session, inputId = text_box_id, value = "", placeholder = "Message Sent")
            shinyjs::disable(text_box_id)  # Disable the text box
            shinyjs::disable(btn_id)       # Disable the send button
            output[[status_id]] <- renderUI({
              div(style = "color: green; font-weight: bold;", "Message Sent")
            })
          }, error = function(e) {
            print(paste("Error sending message:", e$message))
          })
        }
      })
    })
  })
  
  # UI with Styled Button
  output$matches_display <- renderUI({
    div(
      lapply(seq_len(nrow(user_data$matched_users)), function(i) {
        matched_user <- user_data$matched_users[[i]]
        div(
          style = "margin-bottom: 20px; border: 1px solid #ccc; padding: 10px;",
          tags$img(src = matched_user$photo_path, height = "100px", alt = "Match Picture"),
          p(paste("Name:", matched_user$name)),
          p(paste("Age:", matched_user$age)),
          p(paste("Race:", matched_user$race)),
          textInput(
            inputId = paste0("message_", matched_user$smu_id),
            label = "Send a message:",
            value = ""
          ),
          actionButton(
            inputId = paste0("send_", matched_user$smu_id),
            label = "Send",
            style = "background-color: green; color: white; border: none; padding: 10px 20px; font-size: 14px; border-radius: 4px;"
          )
        )
      })
    )
  })
  
  
  #Preferences Functionality
  # Fetch Preferences on Tab Navigation
  observeEvent(input$tabs, {
    if (input$tabs == "preferences") {
      req(user_data$data)  # Ensure the user is logged in
      
      # Fetch the user's current preferences
      preferences_query <- paste0("SELECT * FROM Preferences WHERE user_id = ", user_data$data$user_id)
      preferences <- query_database(preferences_query)
      
      if (!is.null(preferences) && nrow(preferences) > 0) {
        # Update the UI inputs with the fetched preferences
        updateRadioButtons(session, "edit_pref_gender", selected = preferences$pref_gender)
        updateSliderInput(session, "edit_pref_age", value = c(preferences$pref_age_min, preferences$pref_age_max))
        updateSelectInput(session, "edit_pref_race", selected = preferences$pref_race)
        updateSelectInput(session, "edit_pref_academic_level", selected = preferences$pref_academic_level)  # Academic Level
      }
      
      # Clear the message when navigating to Preferences tab
      output$preferences_message <- renderText("")
    }
  })
  
  # Save Preferences Button
  observeEvent(input$save_preferences_btn, {
    req(user_data$data)  # Ensure the user is logged in
    
    # Debugging: Check all inputs
    print(paste("Gender:", input$edit_pref_gender))
    print(paste("Age Range:", input$edit_pref_age))
    print(paste("Race:", input$edit_pref_race))
    print(paste("Academic Level:", input$edit_pref_academic_level))
    
    # Check for missing fields
    missing_fields <- c(
      if (is.null(input$edit_pref_gender) || input$edit_pref_gender == "") "Preferred Gender",
      if (is.null(input$edit_pref_age) || length(input$edit_pref_age) < 2) "Preferred Age Range",
      if (is.null(input$edit_pref_race) || input$edit_pref_race == "") "Preferred Race",
      if (is.null(input$edit_pref_academic_level) || input$edit_pref_academic_level == "") "Preferred Academic Level"
    )
    
    if (length(missing_fields) > 0) {
      output$preferences_message <- renderText({
        paste("All fields are required! Missing fields:", paste(missing_fields, collapse = ", "))
      })
      return()
    }
    
    tryCatch({
      # Update preferences in the database
      update_query <- paste0(
        "UPDATE Preferences SET pref_gender = '", input$edit_pref_gender, "', ",
        "pref_age_min = ", input$edit_pref_age[1], ", ",
        "pref_age_max = ", input$edit_pref_age[2], ", ",
        "pref_race = '", input$edit_pref_race, "', ",
        "pref_academic_level = '", input$edit_pref_academic_level, "' ",
        "WHERE user_id = ", user_data$data$user_id
      )
      dbExecute(db_pool, update_query)
      
      # Display success message
      output$preferences_message <- renderText("Preferences updated successfully!")
    }, error = function(e) {
      # Display error message
      output$preferences_message <- renderText(paste("Error updating preferences:", e$message))
    })
  })
  
  
  # Help Button Functionality
  observeEvent(input$help_btn, {
    # Query to fetch dating tips from the database
    tips_query <- "SELECT category, content FROM Dating_Tips"
    tips <- query_database(tips_query)
    
    # Check if tips are available
    if (!is.null(tips) && nrow(tips) > 0) {
      # Group tips by category
      categories <- unique(tips$category)
      
      # Dynamically create tabs for each category
      tabs <- lapply(categories, function(cat) {
        tabPanel(
          title = cat,
          div(
            style = "max-height: 400px; overflow-y: auto; padding: 10px;",
            lapply(which(tips$category == cat), function(i) {
              tip <- tips[i, ]
              div(
                style = "margin-bottom: 20px;",
                p(tip$content, style = "font-family: 'Arial'; font-size: 16px; color: #eeeeee;")
              )
            })
          )
        )
      })
      
      # Show modal with tabbed content
      showModal(modalDialog(
        title = "Dating Tips",
        tabsetPanel(
          id = "dating_tips_tabs",
          do.call(tabsetPanel, tabs)
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else {
      # Display a no tips available message if the table is empty
      showModal(modalDialog(
        title = "Dating Tips",
        div(
          style = "padding: 10px;",
          p("No dating tips available at the moment. Please check back later!", 
            style = "font-family: 'Arial'; font-size: 16px; color: #eeeeee;")
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })
  
  # Unsubscribe Button Functionality
  observeEvent(input$unsubscribe_btn, {
    # Show confirmation modal
    showModal(modalDialog(
      title = "Unsubscribe",
      div(
        p("Are you sure you want to unsubscribe? This will delete all your data from the platform."),
        style = "font-family: 'Arial'; font-size: 16px; color: #eeeeee;"
      ),
      footer = tagList(
        actionButton("confirm_unsubscribe", "Yes", 
                     style = "color: white; background-color: #dc3545; padding: 10px 20px; font-size: 16px; border-radius: 5px;"),
        actionButton("cancel_unsubscribe", "No", 
                     style = "color: white; background-color: #28a745; padding: 10px 20px; font-size: 16px; border-radius: 5px;")
      ),
      easyClose = TRUE
    ))
  })
  
  # Confirm Unsubscribe Functionality
  observeEvent(input$confirm_unsubscribe, {
    req(user_data$data)
    smu_id <- user_data$data$smu_id
    
    # Delete user data from all tables
    tryCatch({
      dbExecute(db_pool, paste0("DELETE FROM Preferences WHERE user_id = (SELECT user_id FROM Users WHERE smu_id = '", smu_id, "')"))
      dbExecute(db_pool, paste0("DELETE FROM Matches WHERE smu_id = '", smu_id, "' OR matched_user_id = '", smu_id, "'"))
      dbExecute(db_pool, paste0("DELETE FROM Messages WHERE sender_id = (SELECT user_id FROM Users WHERE smu_id = '", smu_id, "') OR receiver_id = (SELECT user_id FROM Users WHERE smu_id = '", smu_id, "')"))
      dbExecute(db_pool, paste0("DELETE FROM Users WHERE smu_id = '", smu_id, "'"))
      
      # Clear user session data
      user_data$data <- NULL
      user_data$matched_users <- list()
      
      # Reset UI components
      output$profile_display <- renderUI(NULL)
      output$matches_display <- renderUI(NULL)
      output$match_tab_display <- renderUI(NULL)
      
      # Clear input fields
      updateTextInput(session, "login_id", value = "")
      updateTextInput(session, "login_password", value = "")
      
      # Show success message
      showModal(modalDialog(
        title = "Unsubscribed",
        p("Your account has been successfully deleted. Thank you for being a part of SMU Campus Connect."),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
      
      # Redirect to Login tab
      updateTabItems(session, inputId = "tabs", selected = "Login")
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        p("An error occurred while processing your request. Please try again later."),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
    })
  })
  
  # Cancel Unsubscribe Functionality
  observeEvent(input$cancel_unsubscribe, {
    removeModal()  # Close the modal dialog
  })
  
  
  # Logout Functionality
  observeEvent(input$logout_btn, {
    print("Logging out...")
    
    # Clear user session data
    user_data$data <- NULL
    user_data$matched_users <- list()
    
    # Reset all UI components
    output$profile_display <- renderUI(NULL)
    output$matches_display <- renderUI(NULL)
    output$match_tab_display <- renderUI(NULL)
    
    # Clear input fields
    updateTextInput(session, "login_id", value = "")
    updateTextInput(session, "login_password", value = "")  # Clear password
    
    # Optional: Show logout message with a "Login Again" button
    showModal(modalDialog(
      title = "You have been logged out",
      div(
        p("Please log in again to continue using SMU Campus Connect."),
        actionButton("login_again_btn", "Login Again", class = "btn btn-primary btn-block")
      ),
      easyClose = FALSE,
      footer = NULL
    ))
  })
  
  # Redirect to Login Tab on "Login Again" button click
  observeEvent(input$login_again_btn, {
    removeModal()  # Close the modal dialog
    updateTabsetPanel(session, "tabs", selected = "Login")  # Redirect to Login tab
  })
  
  
  
  # ChatGPT Functionality
  response_type <- reactiveVal(NULL)
  generated_sql <- reactiveVal(NULL)
  query_result <- reactiveVal(NULL)
  general_answer <- reactiveVal(NULL)
  user_question <- reactiveVal(NULL)  # Store the user question
  
  observeEvent(input$ask_btn, {
    response_type(NULL)
    generated_sql(NULL)
    query_result(NULL)
    general_answer(NULL)
    user_question(NULL)  # Clear previous question
    
    # Clear all outputs initially
    output$response_type <- renderText({ NULL })
    output$generated_sql <- renderText({ NULL })
    output$query_result <- renderTable({ NULL })
    output$general_answer <- renderText({ NULL })
    output$user_question <- renderText({ NULL })
    
    # Retrieve the user's query
    user_query <- input$user_query
    user_question(user_query)  # Store the question
    
    # Clear the input field after asking a question
    updateTextAreaInput(session, "user_query", value = "")
    
    # Generate the prompt for ChatGPT
    gpt_prompt <- paste(
      "The user asked:", user_query,
      "\nDetermine if this question is general or database-related.",
      "\nIf database-related, generate a valid SQL query based on the schema provided.",
      "\nIf it is a general question, provide a direct and concise answer as ChatGPT normally would."
    )
    
    # Interact with ChatGPT API
    gpt_response <- tryCatch({
      query_chatgpt(gpt_prompt)
    }, error = function(e) {
      paste("ChatGPT Error:", e$message)
    })
    
    print("GPT Response:")
    print(gpt_response)  # Debugging output
    
    # Determine if the response is SQL or general
    if (grepl("```sql", gpt_response, ignore.case = TRUE)) {
      # Extract SQL query from ChatGPT response
      generated_sql_query <- tryCatch({
        gsub("(?s).*```sql\\s*(.*?)\\s*```.*", "\\1", gpt_response, perl = TRUE)
      }, error = function(e) {
        ""
      })
      
      response_type("Database Query")
      
      if (nchar(generated_sql_query) > 0 && grepl("select|insert|update|delete", generated_sql_query, ignore.case = TRUE)) {
        tryCatch({
          db_result <- query_database(generated_sql_query)
          
          # Ensure numeric columns are integers
          if (!is.null(db_result) && ncol(db_result) > 0) {
            for (col in colnames(db_result)) {
              if (is.numeric(db_result[[col]])) {
                db_result[[col]] <- as.integer(db_result[[col]])
              }
            }
          }
          
          generated_sql(generated_sql_query)
          query_result(db_result)
        }, error = function(e) {
          query_result(data.frame(Error = paste("Database error:", e$message)))
        })
      } else {
        generated_sql("Invalid SQL query generated. Please refine your question.")
        query_result(data.frame(Error = "ChatGPT did not produce a valid SQL query."))
      }
    } else {
      # Handle as a general question
      response_type("General Question")
      general_answer(gpt_response)
    }
    
    # Update outputs
    output$user_question <- renderText({
      paste("Question Asked:", user_question())
    })
    
    output$response_type <- renderText({
      paste("Response Type:", response_type())
    })
    
    output$generated_sql <- renderText({
      if (!is.null(generated_sql())) paste("Generated SQL Query:\n", generated_sql()) else NULL
    })
    
    output$query_result <- renderTable({
      query_result()
    }, sanitize.text.function = function(x) as.character(x))  # Ensure no decimals in text rendering
    
    output$general_answer <- renderText({
      if (!is.null(general_answer())) {
        # Format numeric answers as integers
        answer <- general_answer()
        if (!is.na(as.numeric(answer))) {
          as.character(as.integer(as.numeric(answer)))
        } else {
          paste("Answer:\n", answer)
        }
      } else {
        NULL
      }
    })
  })
  
  
  # Analytics Functionality
  observeEvent(input$run_analytics, {
    analytics_type <- input$analytics_type
    if (analytics_type == "User Demographics") {
      demographics <- query_database("SELECT gender, COUNT(*) AS count FROM Users GROUP BY gender")
      output$analytics_plot <- renderPlot({
        ggplot(demographics, aes(x = gender, y = count, fill = gender)) +
          geom_bar(stat = "identity") + theme_minimal() +
          labs(title = "User Demographics", x = "Gender", y = "Count")
      })
      output$analytics_table <- renderTable({ demographics })
    }
    if (analytics_type == "Age Distribution") {
      age_distribution <- query_database("SELECT age, COUNT(*) AS count FROM Users GROUP BY age")
      output$analytics_plot <- renderPlot({
        ggplot(age_distribution, aes(x = age, y = count)) +
          geom_line() + theme_minimal() +
          labs(title = "Age Distribution", x = "Age", y = "Count")
      })
      output$analytics_table <- renderTable({ age_distribution })
    }
    if (analytics_type == "Preferences Analysis") {
      preferences <- query_database("SELECT pref_gender, COUNT(*) AS count FROM Preferences GROUP BY pref_gender")
      output$analytics_plot <- renderPlot({
        ggplot(preferences, aes(x = pref_gender, y = count, fill = pref_gender)) +
          geom_bar(stat = "identity") + theme_minimal() +
          labs(title = "Preferences Analysis", x = "Preferred Gender", y = "Count")
      })
      output$analytics_table <- renderTable({ preferences })
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
