
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
#' @param descriptions if TRUE labels are printed instead of element IDs
#' @param html print in html format (requires htmlTable)
#' @param big.mark big mark to format numbers
#' @param dateFormat format string for format dates
#' @param ... further arguments passed to or from other methods.
#' @export
print.statement <- function (x, descriptions = FALSE, html = FALSE, big.mark = "", dateFormat = NULL, ...) {

  # prints statement data in transposed form
  
  if(!"statement" %in% class(x))
    stop("Not a statement object")
  if( !all(c("endDate", "startDate", "contextId") %in% names(x)) )
    return(NextMethod(object = x, ...))
  if( nrow(x)<1 )
    return(NextMethod(object = x, ...))
  
  if( any(names(x)[1:finstr_ncol()] !=  finstr_cols()))
    return(NextMethod(object = x, ...))
  
  if(html) {
    return(print_htmlTable(x, big.mark = big.mark, dateFormat = dateFormat, ...))
  }
  
  cat( "Financial statement: ")
  cat( nrow(x), "observations from", min(x$endDate),"to",max(x$endDate), "\n")

  xt <- reshape_table(x)
  
  # description
  if(descriptions) {
    s_names <- xt[["labelString"]]  
  } else {
    s_names <- xt[["elementId"]]
  }
  s_names <- ifelse(nchar(s_names) > 58, paste0(substr(s_names, 1, 40), "..."), s_names)
  fill_blanks <- paste0(rep("  ", 20), collapse = "")
  s_names <- paste0(
    substring(fill_blanks, 1, xt$level*2-2), 
    ifelse(xt$level == 1, "  ", ifelse(xt$is_negative, "- ", "+ ")), 
    s_names,
    ifelse(c(xt$level[-1], 0) > xt$level, " = ", "")
    )
  s_names <- substring(s_names, 3)
    
  df1 <- cbind( Element = s_names, xt[,7:ncol(xt), drop = FALSE] )
  df1 <- df1[, c(1, ncol(df1):2)]
  print.data.frame(df1, right = FALSE, row.names = FALSE, ...)
  
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

print_htmlTable <- function(x, big.mark = "", dateFormat = "%h %Y", ...) {
  if( !requireNamespace("htmlTable", quietly = TRUE)) {
    stop("htmlTable needed for this function to work. Please install it.",
         call. = FALSE)
  }
  x <- reshape_table(x)
  tab <- x[,-c(1, 2, 3, 4, 5, 6 )]
  labelString <- x$labelString
  #labelString <- ifelse(nchar(labelString) > 68, paste0(substr(labelString, 1, 65), "..."), labelString)
  labelString <- 
    paste0(substring(paste0(rep("&nbsp;", 20), collapse = ""), 1, x$level*18 - 18), 
           labelString)
  
  tab[,1:ncol(tab)] <- format(tab, big.mark = big.mark)
  
  
  to_strong <- function(x, b = TRUE) {
    ifelse(b, paste0("<strong>", x, "</strong>"), x )
  }
  
  row.names(tab) <- to_strong(labelString, !x$terminal)
  for(col in colnames(tab)) {
    tab[[col]] <- to_strong(tab[[col]], !x$terminal) 
  }
  if(!is.null(dateFormat))
    names(tab) <- format( as.POSIXct(names(tab)), dateFormat)
  tab <- tab[,ncol(tab):1]
  htmlTable::htmlTable(tab, align = paste(rep("r", ncol(tab) ), collapse = ""))
}

