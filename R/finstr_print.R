#' Print a parsed XBRL document
#' @param x XBRL data: a list of data frames as returned from XBRL::xbrlDoAll or xbrl_parse_min
#' @param ... further arguments passed to or from other methods.
#' @seealso xbrl_parse_min, xbrl_get_statements
#' @keywords internal
#' @export
print.xbrl_vars <- function(x, ...) {
  size <- round( object.size (x) / 2^20, 2)
  cat("XBRL parsed data (xbrl_vars)\n")
  cat("  Source: ", attr(x, "source"), "\n")
  cat("  Size: ", size, " Mb\n")
  cat(capture.output(str(x, max.level=1)), "\n", sep = "\n")
}



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
  
  decimals <- min(x[["decimals"]])
  if( is.null(decimals) ) decimals <- 0
  if( is.na(decimals) ) decimals <- 0

  cat( "Financial statement: ")
  cat( nrow(x), "observations from", min(x$endDate),"to",max(x$endDate), "\n")
  cat("Numbers in ", paste(rep("0", -decimals), collapse = ""), "\n")
  
  

  concepts <- names(x)[5:ncol(x)]
  values <- as.data.frame( t(x[, concepts] * 10^decimals))
  names(values) <- x[,3]
  values <- values[,ncol(values):1]
  row.names(values) <- substring(row.names(values), 1, 50)
  x <- values
  NextMethod(object = x, right = FALSE, ...)
}



#' Print elements object
#' @param x elements object
#' @param descriptions prints labels or element IDs
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
