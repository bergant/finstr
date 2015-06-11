
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

  xbrl_data_aapl2013 <- XBRL::xbrlDoAll(file1)
  xbrl_data_aapl2014 <- XBRL::xbrlDoAll(file2)
  
  
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


finstr_cols <- function(x = NULL, inverse = FALSE) {
  cols <- c("contextId", "startDate", "endDate", "decimals")
  if(!missing(x)) {
    if(inverse){
      cols <- names(x)[!names(x) %in% cols]
    } else {
      cols <- names(x)[names(x) %in% cols]
    }
  }
  return(cols)
}

finstr_ncol <- function() length (finstr_cols())
finstr_values <- function(x) (x[(finstr_ncol()+1):ncol(x)])

  
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

#' Get a statement from data (data for specified elements)
#' @param elements elements object
#' @param xbrl_vars XBRL data
#' @param complete_only just the rows without NA
#' @keywords internal
#' @export
xbrl_get_data <- function(elements, xbrl_vars, complete_only = TRUE) {
  # gets data in normal format (with variables as columns and 
  # time periods as rows)
  
  if( !("data.frame" %in% class(elements)) )
    elements <- data.frame(elementId = elements, stringsAsFactors = FALSE)
  
  res <-
    elements %>%
    dplyr::inner_join(xbrl_vars$fact, by = "elementId")

  min_dec <- min(as.numeric(res$decimals), na.rm = TRUE)

  res <-
    res %>%
    dplyr::mutate_(fact = ~as.numeric(fact), decimals = ~min_dec )%>%
    dplyr::inner_join(xbrl_vars$context, by = "contextId") %>%
    dplyr::select_(~contextId ,  ~startDate ,  ~endDate ,  ~elementId ,  ~fact ,  ~decimals) %>%
    tidyr::spread_("elementId", "fact") %>%
    dplyr::arrange_(~endDate)
  
  
  vec1 <- elements$elementId[! elements$elementId %in% names(res)]
  df1 <- setNames( data.frame(rbind(rep(0, length(vec1)))), vec1)
  res <- cbind(res, df1)
  
  value_cols <- finstr_cols(res, inverse = TRUE)
  
  #res <- res[, c(names(res)[1:4], elements$elementId)]
  res <- res[, c(finstr_cols(res), elements$elementId)]

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
    to_update <- elements[elements$elementId %in% df1$elementId, "elementId"]
    elements[ 
      #order(match(elements$elementId, to_update))[1:length(level_str)], 
      order(match(elements$elementId, df1$elementId))[1:length(level_str)], 
      "id"] <- level_str
    
    df1 <- elements %>%
      dplyr::filter_(~parentId %in% df1$elementId) %>%
      dplyr::arrange_(~order) %>%
      dplyr::select_(~elementId, ~parentId) %>%
      dplyr::left_join(elements, by=c("parentId"="elementId")) %>%
      dplyr::arrange_(~id)
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

  if( is.null(x)  ) {
    stop("No statement")
  }
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


#' Get descendands
#' @description Gets all descendand terminal elements from calculation tree
#' @param x a statement object
#' @param element_id element (or vector of elements) from statement hierarchy
#' @export
#' @keywords internal
get_descendants <- function(x, element_id, all = FALSE) {
  
  unique(do.call(c, lapply(
    element_id, function(e) {
      get_elements(x, parent_id = e, all)[["elementId"]]
    }
  )))
  
}

#' Get parent
#' @description Gets a parent element id
#' @param x a statement object
#' @param element_id element id
#' @export
#' @keywords internal
get_parent <- function(x, element_id) {
  elements <- get_elements(x)
  elements[elements$elementId %in% element_id,"parentId"]
}


#' Get ascendant
#' @description Gets common ascendant from elements
#' @param x a statement object
#' @param element_id elements
#' @export
#' @keywords internal
get_ascendant <- function(x, element_id) {
  elements <- get_elements(x)

  if( !all(element_id %in% elements[,"elementId"] )) 
    return(NULL)
  
  sel_elements <- elements[elements$elementId %in% element_id,]
  asc1 <- sapply(elements$id, function(x) { all(x == substring(sel_elements$id, 1, nchar(x)))})
  return(elements[asc1,"elementId"])
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
#' @param element_id element from hierarchy where to perform calculation (if specified)
#' @return check object with calculated and original values
#' @export
check_statement <- function(statement, element_id = NULL) {
  
  if(! "statement" %in% class(statement)) {
    stop("Not a statement object")
  }

  els <- get_elements(statement)
  if(is.null(els)) {
    stop("No calculation hierarchy found")
  }
  if(missing(element_id)) {
    element_id <- els$elementId
  }
  err1 <- do.call(
    rbind,
    lapply(element_id, function(x) {
      
      xb <- els$balance[els$elementId == x]
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
  
  if(!any(names(x)[-(1:4)] %in% names(y)[-(1:4)])  ) {
    #if same period and different statments
    col_pos <- which(names(y) %in% c("contextId", "startDate", "decimals"))
    z <- 
      x %>%
      dplyr::left_join(y[,-col_pos], by = "endDate")
    
  } else {
    # if same statement type and different periods
    
    z <- merge.data.frame(x, y, all = TRUE, ...)
    # replace NAs in values by zeros
    z[,5:ncol(z)][is.na(z[,5:ncol(z)])] <- 0
    # remove duplicated rows (based on periods)
    z <- z[!duplicated(z[c("endDate")], fromLast = TRUE), ]
    # order rows by endDate
    z <- z[order(z$endDate), ]
    # order columns based on original taxonomy
    z <- z[,c(names(z)[1:4], el_z[["elementId"]])]
  }
  
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
#' @param calculations optional: calculations generated by calculation function
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
calculate <- function(x, ..., digits = NULL, decimals = NULL, calculations = NULL) {
  # calculate
  res <- dplyr::transmute_(x, date = ~endDate, .dots = c(lazyeval::lazy_dots(... ),calculations))
  # remove hidden columns (leading dots)
  res <- res[grep("^[^\\.]", names(res))]
  # rounding results
  if(!missing(digits)) {
    res[,2:ncol(res)] <- round(res[,2:ncol(res)], digits) 
  }
  if(missing(decimals)) {
    if(!is.null(x[["decimals"]])) {
      decimals <- min(x[["decimals"]], na.rm = TRUE)
    }
    if(!is.null(x[["decimals.x"]])) {
      decimals <- min(x[["decimals.x"]], na.rm = TRUE)
    }
  }
  if(!is.null(decimals) && !is.na(decimals) && 
       max(abs(res[1:5,2:ncol(res)]), na.rm = TRUE)>10^(-decimals)) {
    res[,2:ncol(res)] <- res[,2:ncol(res)] * 10 ^ decimals 
  }
  return(res)
}

#' Define calculation
#' @param ... formulas
#' @seealso \code{\link{calculate}}
#' @examples
#' \dontrun{
#' 
#' profit_margins <- calculation(
#'  Gross_Margin = (SalesRevenueNet - CostOfGoodsAndServicesSold) / SalesRevenueNet,
#'  Operating_Margin = OperatingIncomeLoss / SalesRevenueNet,
#'  Net_Margin = NetIncomeLoss / SalesRevenueNet
#' )
#' 
#' income_statement %>% calculate(calculations = profit_margins)
#' }
#' @export
calculation <- function(...) {
  lazyeval::lazy_dots(...) 
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

#'Without operator
#'@param a element id from element hierarchy
#'@param b element id from element hierarchy
#'@description Used inside fold function to select elements in a without elements in b
#'@return Lazy object with function to select statement elements in a witout elements in b
#'@export
#'@keywords internal
`%without%` <- function (a, b) {
  # declare x and y just to let the check now there is no problem...
  # x will be a statement object when the expression evaluates
  # y will be all used elements
  x <- NULL
  y <- NULL

  lazyeval::lazy(
    setdiff( setdiff( get_descendants(x, a), get_descendants(x, b)), y)
  )
}

#' Proportional values
#' 
#' Every value in the financial statement is divided by topmost parent's value
#' @param x a statement object
#' @param digits if specified number of digits to round the result
#' @export
proportional <- function(x, digits = NULL) {
  y <- x
  for(col_name in get_elements(x)[["elementId"]]) {
    parent_id <- get_ascendant(x, col_name)[[1]]
    x[[col_name]] <- y[[col_name]] / y[[parent_id]]
    if(!missing(digits)) {
      x[[col_name]] <- round(x[[col_name]], digits )
    }
  }
  x[["decimals"]] <- 0
  return(x)
}

#'Other elements
#'@param ... element IDs from element hierarchy
#'@description Used inside expose function to select all other elements
#'@return Lazy object with function to select elements from statement x
#'@export
#'@keywords internal
other <- function (...) {
  # declare x and y just to let the check now there is no problem...
  # x will be a statement object when the expression evaluates
  # y will be all used elements
  x <- NULL
  y <- NULL
  
  lazyeval::lazy(
    setdiff(get_descendants(x, c(...) ), y)
  ) 
}



#' Reshape to "long" format
#' @description Reshapes statement object to a data frame with one value column
#'  and dimension columns (endDate, elementId and parentId). 
#'  @param x a statement object
#'  @param levels if defined only elements from specified levels will be included
#'  @export
reshape_long <- function(x, levels = NULL) {
  elements <- get_elements(x)
  if(missing(levels)) levels <- unique(elements[["level"]])

  x %>%
    tidyr::gather_("elementId", "value", elements[["elementId"]], convert = FALSE) %>%
    dplyr::mutate_("elementId" = ~as.character(elementId)) %>%
    dplyr::inner_join(elements, by = "elementId") %>%
    dplyr::filter_(~!is.na(parentId) & level %in% levels ) %>%
    dplyr::select_(date = ~endDate, element = ~elementId, parent = ~parentId, ~value, 
            label = ~labelString, ~decimals, element_id = ~id) %>%
    dplyr::left_join(elements, by = c("parent" = "elementId")) %>%
    dplyr::select_(~date, ~element, ~parent, ~value, ~label, 
            parent_label = ~labelString, ~decimals, 
            ~element_id,  parent_id = ~id)

}

#' Reshape statement data to table format
#' 
#' Transposes data to "print-out" format
#' @param x a statement object
#' @param decimals return values with decimals
#' @export
#' @keywords internal
reshape_table <- function(x, decimals = TRUE, simple = FALSE) {

  e <- get_elements(x)
  values <- finstr_values(x) 

  if(decimals) {
    decimals_no <- min(x[["decimals"]])
    if(is.na(decimals_no)) decimals_no <- 0
    values <- values * 10 ^ decimals_no
  }
  
  parent_pos <- match(e[["parentId"]], e[["elementId"]])
  is_negative <- e[,"balance"] != e[parent_pos, "balance"]
  is_negative[is.na(is_negative)] <- FALSE
  
  ret <- cbind(e[,c("elementId", "level", "id", "terminal", "labelString")], is_negative, t(values))
  names(ret)[7:ncol(ret)] <- x$endDate
  row.names(ret) <- 1:nrow(ret)
  if(simple) {
    ret <- ret[, -c(1, 2, 3, 4, 6)]
  }
  return(ret)
}


expose_prepare <- function(x, e_list) {
  used_elements <- c()
  ret_list <- list()
  
  for(exp_name in names(e_list) ) { 
    exp_els <- e_list[[exp_name]]
    
    # Elements can be defined as a function (lazy object)
    # ... or as descendands of specified elements
    if("lazy" %in% class(exp_els)) {
      els <- lazyeval::lazy_eval( exp_els, 
                                  data = list(x = x, y = used_elements))
    } else {
      els <- unname(get_descendants(x, exp_els))
    }
    if( any(els %in% used_elements) ) {
      warning("The duplicate elements will be removed from ", 
              exp_name, ": ",
              els[ els %in% used_elements],
              call. = FALSE)
      els <- setdiff( els, used_elements)
    }
    # track used elements
    used_elements <- c(used_elements, els)
    # result
    ret_list[[exp_name]] <- els    
  }
  
  # pick all leftovers
  elements <- get_elements(x, all = FALSE)[["elementId"]]
  the_rest <- setdiff(elements, used_elements)
  if(length(the_rest) > 0) {
    the_rest <- split(the_rest, sapply(the_rest, function(s) get_ascendant(x, s)[1]))
    for(tr in names(the_rest)) {
      ret_list[[paste0("Other",tr,"_")]] <- the_rest[[tr]]
    }
  }
  
  return(ret_list)
}


#' Expose financial sheet values
#' 
#' Simplifies statement to 2-level hierarchy.
#' Elements are defined by list of element vectors.
#' 
#' @param x a statement object
#' @param ... expressions to expose values
#' @param e_list expressions to expose values
#' @examples
#' 
#' \dontrun{
#' expose(balance_sheet,
#'                      
#'   # Assets
#'   `Current Assets` = "AssetsCurrent",
#'   `Noncurrent Assets` = other("Assets"),
#'   # Liabilites and equity
#'   `Current Liabilities` = "LiabilitiesCurrent",
#'   `Noncurrent Liabilities` = other(c("Liabilities", "CommitmentsAndContingencies")),
#'   `Stockholders Equity` = "StockholdersEquity"
#' )
#' }
#' @export
expose <- function(x, ..., e_list = NULL) {
  
  # concatenate dots and list arguments
  e_list <- c(e_list, list(...))
  # prepare list of elements
  if(length(e_list) > 0)
    e_list <- expose_prepare(x, e_list)
  
  descriptions <- names(e_list)
  names(e_list) <- make.names(names(e_list))
  
  # initial y = top level elements from x
  x_els <- get_elements(x)
  y_els <- x_els[ x_els$level == 1, ]
  y_els$terminal <- TRUE
  y <- x[, c(names(x)[1:4], y_els$elementId)]
  attr(y, "elements") <- y_els

  for(exp_name in names(e_list) ) { 
    els <- e_list[[exp_name]]
    description <- descriptions[which(exp_name == names(e_list))]
    # add a row in elements object
    parent_id <- get_ascendant(x, els)[1]
    if(is.na(parent_id) || length(parent_id) == 0)
      stop("A group of elements defined too broadly")
    balance <- x_els[ x_els$elementId == parent_id, "balance" ]
    labelString <- description
    
    y_els <-
      dplyr::bind_rows(y_els, dplyr::data_frame(
          elementId = exp_name,
          parentId = parent_id,
          order = 1,
          balance = balance,
          labelString = labelString))
    
    # calculate value
    xb <- x_els$balance[x_els$elementId == parent_id]
    xc <- na.omit(x_els[x_els$elementId %in% els, "elementId"])
    xcb <- x_els[x_els$elementId %in% xc, "balance"]
    xcs <- ifelse(xb == xcb, 1, -1)
    xcv <- rowSums(crossprod(t(x[, xc]), xcs) )
    y[[exp_name]] <- xcv
    
    # rearrange hierarchy
    y_els <- get_elements_h(y_els[1:5])
    y_els <- as.elements(y_els)
    y <- y[,c(names(x)[1:4], y_els$elementId)]
    attr(y, "elements") <- y_els
  }
  
  return(y)
}

