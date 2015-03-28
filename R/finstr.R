
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
    semi_join(xbrl_data_aapl2013$fact, by="elementId" )
  
  xbrl_data_aapl2014$unit <- NULL
  xbrl_data_aapl2014$footnote <- NULL
  xbrl_data_aapl2014$definition <- NULL
  xbrl_data_aapl2014$presentation <- NULL
  xbrl_data_aapl2014$element <- 
    xbrl_data_aapl2014$element %>%
    semi_join(xbrl_data_aapl2014$fact, by="elementId" )
  
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
  xbrl_file_xsd <- paste0(dirname(xbrl_file), "/", xbrlGetSchemaName(doc))
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
    dplyr::filter(type == "Statement") %>%
    dplyr::semi_join(xbrl_vars$calculation, by = "roleId") %>%
    dplyr::select(roleId) %>%
    simplify2array() %>%
    unname()
}

xbrl_get_data <- function(elements, xbrl_vars, complete_only = TRUE) {
  # gets data in normal format (variables are columns and contexts are rows)
  
  if( !("data.frame" %in% class(elements)) )
    elements <- data.frame(elementId = elements, stringsAsFactors = FALSE)
  col_vec <- 
    c("contextId", "startDate", "endDate", "elementId", "fact", "decimals")  
  res <-
    elements %>%
    dplyr::inner_join(xbrl_vars$fact, by = "elementId") %>% 
    dplyr::mutate(fact = as.numeric(fact), decimals = as.numeric(decimals)) %>%
    dplyr::inner_join(xbrl_vars$context, by = "contextId") %>%
    dplyr::select(one_of(col_vec)) %>%
    tidyr::spread(elementId, fact) %>%
    dplyr::arrange(endDate)
  
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

xbrl_get_labels <- function(xbrl_vars, elements) {

  if( is.null(xbrl_vars$label[1])) {
    return(NULL)
  }
  if( !("data.frame" %in% class(elements)) )
    elements <- data.frame(elementId = elements, stringsAsFactors = FALSE)
  
  xbrl_vars$label %>%
    dplyr::semi_join(elements, by = "elementId") %>%
    dplyr::filter(labelRole == "http://www.xbrl.org/2003/role/label") %>%
    dplyr::select(elementId, labelString)
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
    dplyr::filter(labelRole == "http://www.xbrl.org/2003/role/label") %>% 
    dplyr::transmute(elementId, parentId = fromElementId, order, balance, labelString)
  
  elements <- get_elements_hierarchy(elements)
  
  class(elements) <- c("elements", "data.frame")
  return(elements)
}


#' Get elements hierarchy 
#' 
#' @details Add level and hierarchycal id columns
#' @param elements data.frame with elementId, parentId and order columns 
#' @export
#' @keywords internal
get_elements_hierarchy <- function(elements) {
  # reorder and classify elements by hierarchy
  # adds level, hierarchical id and terminal column  
  level <- 1
  df1 <- elements %>%
    dplyr::filter(is.na(parentId)) %>%
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
      dplyr::filter(parentId %in% df1$elementId) %>%
      dplyr::arrange(order) %>%
      dplyr::select(elementId, parentId) %>%
      dplyr::left_join(elements, by=c("parentId"="elementId"))
    nrow(df1) > 0})
  {
    level <- level + 1
  }

  elements <- 
    elements %>%  
    dplyr::arrange(id) %>% 
    dplyr::mutate( terminal = !elementId %in% parentId )
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
    dplyr::filter(roleId == role_id) %>%
    dplyr::select(fromElementId, toElementId, order) %>% 
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




#' Get the terminating nodes of the calculation hierarchy
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
    strop("Not a statement class")
  }
  
  elements <- attr(x, "elements")
  
  if(!missing(parent_id)) {
    id_parent <- elements$id[elements$elementId == parent_id]
    elements <- elements %>%
      dplyr::filter(substring(id, 1, nchar(id_parent)) == id_parent) %>%
      as.elements()

  }
  if(!all) {
    elements <- elements %>%
      dplyr::filter(terminal) %>%
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
  
  # merge elements and create new hierarchy id-s
  el_x <- get_elements(x)
  el_y <- get_elements(y)
  el_z <- NULL
  if(!is.null(el_x) & !is.null(el_y))
  col_names <- names(el_x)[!names(el_x) %in% c("level", "id", "terminal")]
  el_z <- rbind(el_x[,col_names], el_y[,col_names])
  el_z <- el_z[!duplicated(el_z[,c("elementId", "parentId")]), ]  
  el_z <- get_elements_hierarchy(el_z)
  class(el_z) <- c("elements", "data.frame")
  
  
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
#' @export
calculate <- function(x, ...) {

  #dplyr::transmute_(x, endDate = ~endDate, .dots = lazyeval::lazy_dots(...))
  dplyr::transmute_(x, endDate = ~endDate, .dots = lazyeval::lazy_dots(...)) %>%
    dplyr::select(everything(), -matches("^\\."))
  
}

