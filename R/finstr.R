
#' @name xbrl_data_aapl2014
#' @title XBRL data from SEC archive
#' @description parsed XBRL files from 
#'    http://edgar.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml
#'    using XBRL::xbrlDoAll
#'    and truncating to only necessary data
#' @usage data(xbrl_data_aapl2014) 
#' @docType data
NULL

#' @name xbrl_data_aapl2013
#' @title XBRL data from SEC archive
#' @description parsed XBRL files from 
#'    http://edgar.sec.gov/Archives/edgar/data/320193/000119312513416534/aapl-20130928.xml
#'    using XBRL::xbrlDoAll
#'    and truncating to only necessary data
#' @usage data(xbrl_data_aapl2013) 
#' @docType data
NULL

#' Function to create package data included in the package
#' 
#' @details xbrlDoAll creates > 5Mb list - we need only 2Mb
#' @keywords internal
xbrl_create_data <-function() {

  file1 <- "http://edgar.sec.gov/Archives/edgar/data/320193/000119312513416534/aapl-20130928.xml"
  file2 <- "http://edgar.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml" 

  system.time(xbrl_data_aapl2013 <- XBRL::xbrlDoAll(file1) )
  system.time(xbrl_data_aapl2014 <- XBRL::xbrlDoAll(file2) )
  
  
  xbrl_data_aapl2013$unit <- NULL
  xbrl_data_aapl2013$footnote <- NULL
  xbrl_data_aapl2013$definition <- NULL
  xbrl_data_aapl2013$presentation <- NULL
  xbrl_data_aapl2013$element <- 
    xbrl_data_aapl2013$element %>%
    dplyr::semi_join(xbrl_data_aapl2013$fact, by="elementId" )
  
  xbrl_data_aapl2014$unit <- NULL
  xbrl_data_aapl2014$footnote <- NULL
  xbrl_data_aapl2014$definition <- NULL
  xbrl_data_aapl2014$presentation <- NULL
  xbrl_data_aapl2014$element <- 
    xbrl_data_aapl2014$element %>%
    dplyr::semi_join(xbrl_data_aapl2014$fact, by="elementId" )
  
  # devtools::use_data(xbrl_data_aapl2013, overwrite = T)
  # devtools::use_data(xbrl_data_aapl2014, overwrite = T)
  
}

#' Parse XBRL
#' 
#' @description Parse XBRL files
#' @details xbrl_parse_min uses primitives from 
#'  \link[XBRL]{XBRL-package} to parse only necessary information needed to 
#'  convert XBRL to statement object with \code{\link{xbrl_get_statements}} 
#'  function. 
#'  It is slightly faster than \code{\link{xbrlDoAll}} but it does not parse 
#'  all the data. 
#'  It parses only roles, elements, contexts, facts and calculation base link. 
#' @param xbrl_file file path or url to XBRL file
#' @param parse_labels if element labels should be parsed (default to FALSE)
#' @return xbrl_data object
#' @seealso \code{\link{xbrl_get_statements}}
#' @keywords internal
#' @export
xbrl_parse_min <- function(xbrl_file, parse_labels = FALSE) {
  
  # check if xbrl_file exist (url or file)
  if(grepl("^http", xbrl_file)) {
    if(!RCurl::url.exists(xbrl_file)) 
      stop("Url ", xbrl_file, " does not exist.")
  } else {
    if(!file.exists(xbrl_file)) 
      stop("File ", xbrl_file, " does not exist.")
  }
  
  old_opt <- options(stringsAsFactors = FALSE)
  xbrl_vars <- list()

  # process instance: contexts and facts
  doc <- XBRL::xbrlParse(xbrl_file)
  xbrl_vars[["context"]] <- XBRL::xbrlProcessContexts(doc)
  xbrl_vars[["fact"]] <- XBRL::xbrlProcessFacts(doc)
  xbrl_file_xsd <- paste0(dirname(xbrl_file), "/", XBRL::xbrlGetSchemaName(doc))
  XBRL::xbrlFree(doc)
  
  # process schema file (roles)
  docS <- XBRL::xbrlParse(xbrl_file_xsd)
  xbrl_vars[["role"]] <- XBRL::xbrlProcessRoles(docS)
  xbrl_vars[["element"]] <- XBRL::xbrlProcessElements(docS)
  XBRL::xbrlFree(docS)

  # labels
  if(parse_labels) {
    xbrl_file_lab <- gsub("\\.xml$", "_lab.xml", xbrl_file)
    docL <- XBRL::xbrlParse(xbrl_file_lab)
    xbrl_vars[["label"]] <-XBRL::xbrlProcessLabels(docL)
    XBRL::xbrlFree(docL)
  }

  # process calculation link base
  xbrl_file_cal <- gsub("\\.xml$", "_cal.xml", xbrl_file)
  docC <- XBRL::xbrlParse(xbrl_file_cal)
  xbrl_vars[["calculation"]] <- XBRL::xbrlProcessArcs(docC, arcType = "calculation")
  XBRL::xbrlFree(docC)
  class(xbrl_vars) <- c("xbrl_vars", "list")
  attr(xbrl_vars, "source") <- basename(xbrl_file)

  options(stringsAsFactors = unname(unlist(old_opt)))

  invisible(xbrl_vars)
}



#' Get a vector of statement IDs
#' @param xbrl_vars XBRL data
#' @importFrom magrittr "%>%"
#' @keywords internal
#' @export
xbrl_get_statement_ids <- function(xbrl_vars) {
  # finds roleIds for statements
  # example:
  #   xbrl_get_statements(xbrl_vars)
  
  if( !all( c("role", "calculation") %in% names(xbrl_vars)))
    stop(substitute(xbrl_vars), " does not include role and calculation data.")
  
  xbrl_vars$role %>%
    dplyr::filter_(~type == "Statement") %>%
    dplyr::semi_join(xbrl_vars$calculation, by = "roleId") %>%
    dplyr::select_(~roleId) %>%
    simplify2array() %>%
    unname()
}

xbrl_get_data <- function(elements, xbrl_vars, complete_only = TRUE) {
  # gets data in normal format (variables are columns and contexts are rows)
  
  if( !("data.frame" %in% class(elements)) )
    elements <- data.frame(elementId = elements, stringsAsFactors = FALSE)
  
  res <-
    elements %>%
    dplyr::inner_join(xbrl_vars$fact, by = "elementId") %>% 
    dplyr::mutate_(fact = ~as.numeric(fact), decimals = ~as.numeric(decimals)) %>%
    dplyr::inner_join(xbrl_vars$context, by = "contextId") %>%
    dplyr::select_(~contextId ,  ~startDate ,  ~endDate ,  ~elementId ,  ~fact ,  ~decimals) %>%
    tidyr::spread_("elementId", "fact") %>%
    dplyr::arrange_(~endDate)
  
  vec1 <- elements$elementId[! elements$elementId %in% names(res)]
  df1 <- setNames( data.frame(rbind(rep(0, length(vec1)))), vec1)
  res <- cbind(res, df1)
  
  value_cols <- which (
    ! names(res) %in% c("contextId", "startDate", "endDate", "decimals") )
  
  res <- res[, c(names(res)[1:4], elements$elementId)]
  
  # Handling strange NAs - if some columns are total NA:
  empty_cols <- sapply(res[elements$elementId], function(x) length(na.omit(x))==0 )
  res[, names(empty_cols)[ empty_cols]] <- 0
  
  # keep only complete rows
  if(complete_only)
    res <- res[complete.cases( res[ value_cols ] ), ]

  if(any(duplicated(res$endDate))) {
    warning("Rows with duplicated endDate")
    rownames(res) <- res$contextId
  } else {
    rownames(res) <- res$endDate
  }
  
  class(res) <- c("statement", "data.frame")
  return(res)
}



xbrl_get_elements <- function(xbrl_vars, relations) {
  
  # get elements & parent relations & labels
  elements <-
    data.frame( 
      elementId = with(relations, unique(c(fromElementId, toElementId))),
      stringsAsFactors = FALSE
      )  %>%
    dplyr::left_join(xbrl_vars$element, by = c("elementId")) %>%
    dplyr::left_join(relations, by = c("elementId" = "toElementId")) %>%
    dplyr::left_join(xbrl_vars$label, by = c("elementId")) %>%
    dplyr::filter_(~labelRole == "http://www.xbrl.org/2003/role/label") %>% 
    dplyr::transmute_(~elementId, parentId = ~fromElementId, ~order, ~balance, ~labelString)
  
  elements <- get_elements_h(elements)
  
  class(elements) <- c("elements", "data.frame")
  return(elements)
}


#' Get elements hierarchy 
#' 
#' @details Add level and hierarchycal id columns
#' @param elements data.frame with elementId, parentId and order columns 
#' @export
#' @keywords internal
get_elements_h <- function(elements) {
  # reorder and classify elements by hierarchy
  # adds level, hierarchical id and terminal column  
  level <- 1
  df1 <- elements %>%
    dplyr::filter_(~is.na(parentId)) %>%
    dplyr::mutate(id = "")
  
  while({
    level_str <- 
      unname(unlist(lapply(split(df1$id, df1$id), function(x) {
        sprintf("%s%02d", x, 1:length(x))
      })))
    
    elements[elements$elementId %in% df1$elementId, "level"] <- level
    elements[ 
      order(match(elements$elementId, df1$elementId))[1:length(level_str)], 
      "id"] <- level_str
    
    df1 <- elements %>%
      dplyr::filter_(~parentId %in% df1$elementId) %>%
      dplyr::arrange_(~order) %>%
      dplyr::select_(~elementId, ~parentId) %>%
      dplyr::left_join(elements, by=c("parentId"="elementId"))
    nrow(df1) > 0})
  {
    level <- level + 1
  }

  elements <- 
    elements %>%  
    dplyr::arrange_(~id) %>% 
    dplyr::mutate_( terminal = ~ !elementId %in% parentId )
}

#' Get relations from XBRL calculation link base
#' @param xbrl_vars XBRL data (list of dataframes)
#' @param role_id id of role (usually of type statement)
#' @param lbase link base (default is calculation)
#' @keywords internal
#' @export
xbrl_get_relations <- function(xbrl_vars, role_id, lbase = "calculation") {
  
  res <- 
    xbrl_vars[[lbase]] %>%
    dplyr::filter_(~roleId == role_id) %>%
    dplyr::select_(~fromElementId, ~toElementId, ~order) %>% 
    dplyr::mutate(order = as.numeric(order)) %>%
    unique() 

  class(res) <- c("xbrl_relations", "data.frame")
  return(res)
}

#' Get a financial statements object from XBRL
#' 
#' @param xbrl_vars a XBRL parsed object
#' @param rm_prefix a prefix to remove from element names
#' @return A statements object
#' @examples
#' \dontrun{
#' 
#' # parse XBRL to XBRL data and get statements:
#' xbrl_url <- "http://edgar.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml"
#' xbrl_data <- xbrl_parse_min(xbrl_url)
#' st1 <- xbrl_get_statements(xbrl_data)
#' 
#' # get statements directly from a url:
#' xbrl_url <- "http://edgar.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml"
#' st1 <- xbrl_get_statements(xbrl_url)
#' }
#' 
#' @seealso \link{finstr}
#' @export
xbrl_get_statements <- function(xbrl_vars, rm_prefix = "us-gaap_") {

  # xbrl is parsed xbrl
  if( !all( c("role", "calculation", "fact", "context", "element") %in% names(xbrl_vars))) {
    stop("Input does not include all data needed from XBRL.")
  }
  
  # get all statement types from XBRL
  role_ids <- xbrl_get_statement_ids(xbrl_vars)
  
  #get calculation link base relations
  relations <- lapply(role_ids, function(role_id){
    xbrl_get_relations(xbrl_vars, role_id)
  })
  names(relations) <- basename(role_ids)
  elements_list <- lapply(relations, function(x){
    xbrl_get_elements(xbrl_vars, x)
  })
  names(elements_list) <- basename(role_ids)
  
  taxonomy_prefix <- sprintf("^%s", rm_prefix)

  # store xbrl data in statement data structure
  statements <- 
    setNames(
      lapply(
        role_ids,
        function(role_id) {
          stat_name <- basename(role_id)
          links <- relations[[stat_name]]
          elements <- elements_list[[stat_name]]
          #label <- xbrl_get_labels(xbrl_vars, elements)
          res <- xbrl_get_data(elements, xbrl_vars)
          # delete taxonomy prefix
          names(res) <- gsub(taxonomy_prefix, "", names(res))          
          links$fromElementId <- gsub(taxonomy_prefix, "", links$fromElementId)          
          links$toElementId <- gsub(taxonomy_prefix, "", links$toElementId)          
          elements$elementId <- gsub(taxonomy_prefix, "", elements$elementId)          
          elements$parentId <- gsub(taxonomy_prefix, "", elements$parentId)          
          # set attributes
          attr(res, "role_id") <- stat_name
          attr(res, "relations") <- links
          attr(res, "elements") <- elements
          res
        } 
      ),
      basename(role_ids)
    )
  class(statements) <- c("statements", "list")
  return(statements)
}




#' Get statement elements and the calculation hierarchy
#' 
#' @param x A statement object
#' @param parent_id used as filter if defined
#' @param all if FALSE only terminal elements from the hierarchy will be returned
#' @seealso \code{\link{calculate}}
#' @export
get_elements <- function(x, parent_id = NULL, all = TRUE) {
  # returns all terminating elements 
  # if parent_id provided, only descendands from this elements are returned

  if( !"statement" %in% class(x)  ) {
    stop("Not a statement class")
  }

  elements <- attr(x, "elements")
  
  if(!missing(parent_id)) {
    id_parent <- elements[["id"]][elements[["elementId"]] == parent_id]
    elements <- elements %>%
      dplyr::filter_(~substring(id, 1, nchar(id_parent)) == id_parent) %>%
      as.elements()

  }
  if(!all) {
    elements <- elements %>%
      dplyr::filter_(~terminal) %>%
      dplyr::mutate(level = 1) %>%
      as.elements()
  }

  return(elements)

}

as.elements <- function(x) {
  if( !all(c("elementId", "parentId") %in% names(x))) {
    stop("Can't convert to elements")
  }
  class(x) <- c("elements", "data.frame")
  return(x)
}

#' Check statement
#' @description Checks statement calculation consistency
#' @param statement statement object
#' @return check object with calculated and original values
#' @export
check_statement <- function(statement) {
  
  els <- get_elements(statement)
  if(! "statement" %in% class(statement)) {
    stop("Not a statement object")
  }
  if(is.null(els)) {
    stop("No calculation hierarchy found")
  }
  
  err1 <- do.call(
    rbind,
    lapply(els$elementId, function(x) {
      
      xb <- els$balance[els$element == x]
      xc <- na.omit(els$elementId[els$parentId == x])
      xcb <- els$balance[els$element %in% xc]
      xcs <- ifelse( xb == xcb, 1, -1)
      xcv <- rowSums(  crossprod (t(statement[, xc]),  xcs) )
      xv <- statement[ , x]
      if(length(xc) == 0) {
        return(NULL)
      }
      err <- data.frame(
        date = statement$endDate, 
        elementId = x, 
        expression = paste(ifelse(xcs == 1, "+", "-"), xc, collapse = " "),
        original = xv,
        calculated = xcv,
        error = xv - xcv, 
        stringsAsFactors = FALSE)
      row.names(err) <- 1:nrow(err)
      return(err)
    })
  )
  class(err1) <- c("check", "data.frame")
  return(err1)
}


#' Merge statement elements object
#' 
#' @param x elements object
#' @param y elements object
#' @param ... ignored
#' @keywords internal
#' @export
merge.elements <- function(x, y, ...) {
  z <- NULL
  col_names <- names(x)[!names(x) %in% c("level", "id", "terminal")]
  z <- rbind(x[,col_names], y[,col_names])
  z <- z[!duplicated(z[,c("elementId", "parentId")]), ]  
  z <- get_elements_h(z)
  z <- as.elements(z)
  return(z)
}

#' Merge two financial statements
#' 
#' @description Merge two statements from different time periods.
#' @details
#' Since statements are basically data.frames the functions are similar. Except:
#' \itemize{
#' \item new statement elements are allways union of input elements,
#'   if taxonomy changes new and old elements are visible in all periods 
#' \item missing values are set to 0 not to NA as merge would normally do 
#' \item hierarchy is merged from both statements hierarchies
#' \item rows are treated as duplicated based on endDate and the row of x is 
#' allways considered as duplicate (so y should allways be the later statement)
#' }
#' 
#' @param x statement object
#' @param y statement object
#' @param ... further arguments passed to or from other methods
#' @return statement object
#' @export
merge.statement <- function(x, y, ...) {

  if( !"statement" %in% class(x) || !"statement" %in% class(y) ) {
    stop("Not statement objects")
  }
  
  # merge elements
  el_x <- get_elements(x)
  el_y <- get_elements(y)
  el_z <- merge(el_x, el_y)
  
  # merge facts and contexts
  z <- merge.data.frame(x, y, all = TRUE, ...)
  # replace NAs in values by zeros
  z[,5:ncol(z)][is.na(z[,5:ncol(z)])] <- 0
  # remove duplicated rows (based on periods)
  z <- z[!duplicated(z[c("startDate", "endDate")], fromLast = TRUE), ]
  # order rows by endDate
  z <- z[order(z$endDate), ]
  # order columns based on original taxonomy
  z <- z[,c(names(z)[1:4], el_z[["elementId"]])]
  row.names(z) <- z$contextId
  # add attributes
  class(z) <- class(x)
  attr(z, "elements") <- el_z
  attr(z, "role_id") <- attr(x, "role_id")
  return(z)  
}

#' Merge two lists of statements
#' 
#' @details Merges all statements in x with all statements in y
#' @param x statements object
#' @param y statements object
#' @param ... further arguments passed to or from other methods
#' @return statements object
#' @seealso \link{merge.statement} for merging two statements
#' @export
merge.statements <- function(x, y, ...) {

  if( !"statements" %in% class(x) || !"statements" %in% class(y) ) {
    stop("Not statements objects")
  }
  
  z <-
    lapply(names(x), function(statement){
      merge(x[[statement]], y[[statement]], ...)
    })
  names(z) <- names(y)
  class(z) <- "statements"
  return(z)
}

#' Calculate formulas 
#' 
#' @param x a statement object
#' @param ... list of formulas
#' @param digits if specified the result will be rounded according to number of digits
#' @param decimals if specified the result will be multiplied by 10^decimals
#' @return data frame with date and specified columns
#' @examples
#' \dontrun{
#' 
#' balance_sheet %>% calculate(
#'   
#'   current_ratio = AssetsCurrent / LiabilitiesCurrent,
#'   
#'   quick_ratio =  
#'     ( CashAndCashEquivalentsAtCarryingValue + 
#'         AvailableForSaleSecuritiesCurrent +
#'         AccountsReceivableNetCurrent
#'       ) / LiabilitiesCurrent
#' )
#' }
#' @seealso \code{\link{calculation}}
#' @export
calculate <- function(x, ..., digits = NULL, decimals = NULL) {
  # calculate
  res <- dplyr::transmute_(x, date = ~endDate, .dots = lazyeval::lazy_dots(...))
  # remove hidden columns (leading dots)
  res <- res[grep("^[^\\.]", names(res))]
  # rounding results
  if(!missing(digits)) {
    res[,2:ncol(res)] <- round(res[,2:ncol(res)], digits) 
  }
  if(missing(decimals)) {
    decimals <- min(x[["decimals"]], na.rm = TRUE)
  }
  if(!is.na(decimals) && missing(digits)) {
    res[,2:ncol(res)] <- res[,2:ncol(res)] * 10 ^ decimals 
  }
  return(res)
}

#' Define calculation
#' @param ... formulas
#' @param x not used
#' @seealso \code{\link{do_calculation}} and  \code{\link{calculate}}
#' @examples
#' \dontrun{
#' 
#' profit_margins <- calculation(
#'  Gross_Margin = (SalesRevenueNet - CostOfGoodsAndServicesSold) / SalesRevenueNet,
#'  Operating_Margin = OperatingIncomeLoss / SalesRevenueNet,
#'  Net_Margin = NetIncomeLoss / SalesRevenueNet
#' )
#' 
#' income_statement %>% do_calculation(profit_margins)
#' }
#' @export
calculation <- function(..., x = NULL) {
  lazyeval::lazy(calculate(x, ...)) 
}

#' Run a calculation
#' @description Calculations can be defined with \code{\link{calculation}} 
#'  function.
#'  With \code{\link{do_calculation}} the calculations get calculated on a 
#'  specified statement.
#'  Resulting data frame (or data frames if there are several calculations) has 
#'  a date column and the columns defined with calculations. 
#' @param x statement object
#' @param ... one or more calculations
#' @return a data frame or a list of data frames
#' @seealso \code{\link{calculation}}
#' @export
do_calculation <- function(x, ...) {
  calculations <- list(...)

  # accept a list as an argument
  if(length(calculations) == 1 && "list" %in% class(calculations[[1]])) {
    calculations <- calculations[[1]]
  }

  res <- 
    lapply(calculations, function(calc) {
      lazyeval::lazy_eval(calc, list(x = x))
    })

  # simplify to calculation
  if(length(res) == 1 && "list" %in% class(calculations)) {
    res <- res[[1]]
  }
  return(res)
}


#' Statement lagged differences
#' 
#' @param x a statement to be differenced
#' @param lag an integer indicating which lag to use
#' @param ... further arguments passed to or from other methods
#' @return a statement object equal to successive  differences (x and lagged x)
#' @export
diff.statement <- function(x, lag = 1L, ...) {
  y <- x[-((nrow(x)-lag+1):nrow(x)),]
  y$endDate <- x$endDate[-(1:lag)]
  y[,5:ncol(y)] <- x[-(1:lag),5:ncol(x)] - y[,5:ncol(y)]
  return(y)
}
