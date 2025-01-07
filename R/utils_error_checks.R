#' Error checking user input fields
#'
#' @param input_value,session,input_id,limit
#'
#' @noRd
#'
validate_text_input <- function(input_value, session, input_id, limit) {
  tryCatch({
    # Check if the input is numeric
    if (!grepl("^\\d+$", input_value)) {
      stop("Input must be a positive integer.")
    }
    # Check if the number of sites exceeds 1,000,000
    if (as.numeric(input_value) > limit) {
      stop(paste0("Input must not exceed ", limit, "."))
    }

    # If input is valid, return the numeric value
    return(input_value)

  }, error = function(e) {
    # Show error notification
    showNotification(e$message, type = "error")

    # Reset the input field to empty
    updateTextInput(session, input_id, value = "")

    # Return NULL if there's an error
    return(NULL)
  })
}
