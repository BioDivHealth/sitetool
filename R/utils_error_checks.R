#' Error checking user input fields
#'
#' @param input_value,session,input_id,limit
#'
#' @noRd
#'
validate_text_input <- function(input_value, session=NULL, input_id=NULL, limit=1000, inapp=T) {
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
    if(inapp){
      showNotification(e$message, type = "error")

      # Reset the input field to empty
      updateTextInput(session, input_id, value = "")

      # Return NULL if there's an error
    }

    return(NULL)
  })
}

#' Error checking user input fields
#'
#' @param area,crs
#'
#' @noRd
#'
check_validity <- function(area, crs=4326){
  if ("sf" %in% class(area)){
    bbox_sf = area
  } else if(length(area) == 4){
    bbox_sf = sf::st_as_sfc(sf::st_bbox(c(xmin=area[[1]],
                                          ymin=area[[2]],
                                          xmax=area[[3]],
                                          ymax=area[[4]]),
                                        crs=sf::st_crs(crs)))
  } else{
    message("Area must be shape object or bounding box.")
    return(NULL)
  }

  if(!sf::st_is_valid(bbox_sf)){
    message('Area must be valid shape object.')
    return(NULL)
  }
  return(bbox_sf)
}

check_bbox <- function(bbox) {

  if(is.null(names(bbox))){
    if (any(bbox[[1]] < -180 | bbox[[3]] < -180 | bbox[[1]] > 180 | bbox[[3]] > 180)) {
      return(FALSE)
    }
    if (any(bbox[[2]] < -90 | bbox[[4]] < -90 | bbox[[2]] > 90 | bbox[[4]] > 90)) {
      return(FALSE)
    }
  }
  else{
    if (any(bbox[c("xmin", "xmax")] < -180 | bbox[c("xmin", "xmax")] > 180)) {
      return(FALSE)
    }
    if (any(bbox[c("ymin", "ymax")] < -90 | bbox[c("ymin", "ymax")] > 90)) {
      return(FALSE)
    }
  }
  TRUE
}

fix_geometry <- function(geometry) {
  if (!sf::st_is_valid(geometry)) {
    geometry <- sf::st_make_valid(geometry)

    # If st_make_valid() results in an empty geometry, return NA
    if (sf::st_is_empty(geometry) || !sf::st_is_valid(geometry)) {
      return(sf::st_sfc())  # Return an empty geometry
    }
  }
  return(geometry)
}
