
# Sort
obj_sort <- function(direction =  c("ascending", "descending"),
                     timestamp = "last_edited_time"){

  if(is.null(direction)) return(NULL)
  # Validation
  direction <- match.arg(direction)
  if(timestamp != "last_edited_time")
      warning("Current 'timestamp' sort is limited to 'last_edited_time'")

  # Stamp and return
  parse_result(direction, timestamp)
}

#Filter
obj_filter <- function(value = NULL,
                       property = "object"){

  if(is.null(value)) return(NULL)
  # Validation
  value <- match.arg(value, c("page","database"))
  if(property != "object") warning("Current 'property' filter is limited to 'object'")

  # Stamp and return
  parse_result(value, property)
}
