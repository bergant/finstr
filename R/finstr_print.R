
#' Print a statements object
#' @param x a statements object
#' @param ... further arguments passed to or from other methods.
#' @seealso xbrl_get_statements
#' @keywords internal
#' @export
print.statements <- function(x, ...) {
  cat("Financial statements repository\n")
  p1 <- 
    do.call(
      rbind,
      lapply(x, function(x) {
        data.frame(
          From = min(x$endDate),
          To = max(x$endDate),
          Rows = nrow(x), Columns = ncol(x)) 
      }))
  
  print(p1, ...)
}


#' Print a statement object
#' 
#' @param x a statement object
#' @param ... further arguments passed to or from other methods.
#' @export
print.statement <- function(x, ...) {
  # prints statement data in transposed form

  if(!"statement" %in% class(x))
    stop("Not a statement object")
  if( !all(c("endDate", "startDate", "contextId") %in% names(x)) )
    return(NextMethod(object = x, ...))
  if( nrow(x)<1 )
    return(NextMethod(object = x, ...))

  
  
  decimals <- min(x[["decimals"]])
  if( is.null(decimals) ) decimals <- 0
  if( is.na(decimals) ) decimals <- 0

  cat( "Financial statement: ")
  cat( nrow(x), "observations from", min(x$endDate),"to",max(x$endDate), "\n")
  cat("Numbers in ", paste(rep("0", -decimals), collapse = ""), "\n")

  concepts <- names(x)[5:ncol(x)]
  values <- as.data.frame( t(x[, concepts] * 10^decimals))
  names(values) <- x[,3]
  if(ncol(values)>1) {
    values <- values[,ncol(values):1]
  }
  
  # names
  elements <- get_elements(x)
  r_names <- row.names(values)
  el_pos <- which(elements$elementId %in% r_names )
  #fill_dots <- paste0(rep(". ", max(elements$level)), collapse = "")
  fill_dots <- paste0(rep("  ", max(elements$level)), collapse = "")
  parent_pos <- match(elements$parentId, elements$elementId)
  r_names <- 
    paste0( 
      substring(fill_dots,1, (elements[["level"]][el_pos]-1)*2-2),
      ifelse( 
        is.na(elements[["balance"]][parent_pos]), 
        "",
        ifelse(
          elements[["balance"]][el_pos] == elements[["balance"]][parent_pos],
          "+ ", "- "
        )
      ),
      r_names,
      ifelse(elements[["level"]][el_pos] < elements[["level"]][el_pos+1],
             " = ", "")
    )

  row.names(values) <- substring(r_names, 1, 50)

  x <- values
  NextMethod(object = x, right = FALSE, ...)
}



#' Print elements object
#' @param x elements object
#' @param descriptions prints labels or element IDs
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.elements <- function(x, descriptions = FALSE, ...) {
  if(!all(c("level", "elementId", "labelString") %in% names(x))) {
    return(NextMethod(object = x))
  }
  sel_col <- ifelse(descriptions, "labelString", "elementId")
  for(i in seq_len(nrow(x))) {
    str_out <- x[[sel_col]][i]
    str_out <- paste0(paste0(rep("..", x[["level"]][i]-1), collapse=""), str_out)
    str_out <- 
      ifelse(nchar(str_out) > 70, 
             paste0(substring(str_out, 1, 70), "..."),
             str_out)
    cat(str_out, "\n")
  }
}

#' Print check object
#' @param x check object
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.check <- function(x, ...) {
  errors <- x[x$error != 0, ]
  num_errors <- nrow(errors)
  num_el_errors <- length(unique(errors[,"elementId"]))
  cat("Number of errors: ", num_errors, "\n")
  cat("Number of elements in errors: ", num_el_errors, "\n")
  if(num_errors > 0) {
    lapply( split(errors, errors$elementId), function(el) {
      cat("\nElement:", el$elementId[1], " = ", el$expression[1], "\n" )
      print.data.frame(el[,c("date", "original", "calculated", "error")])
    })
  }
}
