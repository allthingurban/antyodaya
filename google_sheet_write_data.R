
# Alternative function for OAuth authentication (interactive)
write_df_to_gsheet_oauth <- function(df, sheet_url, sheet_name = NULL) {
  
  # Load required libraries
  if (!require(googlesheets4)) {
    stop("googlesheets4 package is required. Install with: install.packages('googlesheets4')")
  }
  
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a data frame")
  }
  
  if (!is.character(sheet_url) || length(sheet_url) != 1) {
    stop("Input 'sheet_url' must be a single character string")
  }
  
  # Interactive OAuth authentication
  tryCatch({
    gs4_auth()
    cat("OAuth authentication successful!\n")
    
  }, error = function(e) {
    stop(paste("OAuth authentication failed:", e$message))
  })
  
  # Extract sheet ID from URL if needed
  sheet_id <- tryCatch({
    if (grepl("docs.google.com/spreadsheets", sheet_url)) {
      gsub(".*spreadsheets/d/([a-zA-Z0-9-_]+).*", "\\1", sheet_url)
    } else {
      sheet_url
    }
  }, error = function(e) {
    stop("Invalid Google Sheets URL or ID")
  })
  
  # Write data frame to Google Sheet
  tryCatch({
    if (is.null(sheet_name)) {
      sheet_write(df, ss = sheet_id)
      cat("Data successfully written to Google Sheet (first sheet)\n")
    } else {
      sheet_write(df, ss = sheet_id, sheet = sheet_name)
      cat(paste("Data successfully written to sheet:", sheet_name, "\n"))
    }
    
    return(sheet_id)
    
  }, error = function(e) {
    if (grepl("sheet", e$message, ignore.case = TRUE) && !is.null(sheet_name)) {
      tryCatch({
        sheet_add(ss = sheet_id, sheet = sheet_name)
        sheet_write(df, ss = sheet_id, sheet = sheet_name)
        cat(paste("Created new sheet and wrote data to:", sheet_name, "\n"))
        return(sheet_id)
      }, error = function(e2) {
        stop(paste("Failed to create or write to sheet:", e2$message))
      })
    } else {
      stop(paste("Error writing to Google Sheet:", e$message))
    }
  })
}

# Example usage:

# Method 1: Using service account credentials (recommended for automated scripts)
# df <- data.frame(
#   Name = c("Alice", "Bob", "Charlie"),
#   Age = c(25, 30, 35),
#   City = c("New York", "London", "Tokyo")
# )
# 
# sheet_url <- "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID/edit"
# credentials_path <- "path/to/your/service_account_credentials.json"
# 
# write_df_to_gsheet(df, sheet_url, credentials_path, "MyData")

# Method 2: Using OAuth (interactive authentication)
# write_df_to_gsheet_oauth(df, sheet_url, "MyData")