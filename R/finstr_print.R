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
  x <- values
  
  NextMethod(object = x, right = FALSE, ...)
}

print.xbrl_hierarchy <- function(x, ...) {
  # prints XBRL hierarchy object
  
  if(!"xbrl_hierarchy" %in% class(x))
    stop(substitute(x), " is not a XBRL hierarchy object")
  df1 <- as.data.frame( 
    lapply(x[grep("^level", names(x))], function(y) {
      vec1 <-  y != lag(y)   
      vec1[is.na(vec1)] <- TRUE
      ifelse(vec1, y, "")
      vec2 <- ifelse(vec1, y, "")
      vec2[is.na(vec2)] <- ""
      vec2    
    }),
    stringsAsFactors = FALSE
  )
  
  for(j in seq_len(nrow(df1))) {
    for(i in seq_len(ncol(df1))) {
      #cat(paste(rep("  ", i), df1[j, i]))
      #cat(df1[j, i])
      if(df1[j, i] != "") {
        cat(rep("..", i-1), df1[j, i], sep = "")
      }
      if(df1[j, i] != "")
        cat("\n")
    }
  }
}

#' Print relations object
#' @param x relations object
#' @param ... further arguments passed to or from other methods.
#' @seealso relations
#' @keywords internal
#' @export
print.xbrl_relations <- function(x, ...) {
  #cat("XBRL relations\n")
  h1 <- xbrl_get_hierarchy(x)
  print(h1, ...)
}