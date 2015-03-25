
#' @name xbrl_data_aapl2013
#' @title XBRL data from SEC archive
#' @description parsed XBRL files from 
#'    http://edgar.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml
#' @docType data
NULL

#' @name xbrl_data_aapl2014
#' @title XBRL data from SEC archive
#' @description parsed XBRL files from 
#'    http://edgar.sec.gov/Archives/edgar/data/320193/000119312513416534/aapl-20130928.xml
#' @docType data
NULL


#' Parse XBRL
#' 
#' @param xbrl_file file path or url to XBRL file
#' @return xbrl_data object
#' @description Parse XBRL files
#' @details xbrl_parse_min is using primitives from 
#'  \link[XBRL]{XBRL-package} to parse only necessary information needed to 
#'  convert XBRL to statement object with \code{\link{xbrl_get_statements}} 
#'  function. 
#'  It is slightly faster than \code{\link{xbrlDoAll}} but it does not parse 
#'  all the data. 
#'  It parses only roles, contexts, facts and calculation base link. 
#' @seealso \code{\link{xbrl_get_statements}}
#' @export
xbrl_parse_min <- function(xbrl_file) {
  # 
  # slightly faster than xbrlDoAll (for all data)

  
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
  XBRL::xbrlFree(doc)
  
  # process schema file (roles, labels)
  xbrl_file_xsd <- gsub("\\.xml$", ".xsd", xbrl_file)
  docS <- XBRL::xbrlParse(xbrl_file_xsd)
  xbrl_vars[["role"]] <- XBRL::xbrlProcessRoles(docS)
#  xbrl_vars[["label"]] <-XBRL::xbrlProcessLabels(docS)
  XBRL::xbrlFree(docS)

#   xbrl_file_lab <- gsub("\\.xml$", "_lab.xml", xbrl_file)
#   docL <- XBRL::xbrlParse(xbrl_file_lab)
#   xbrl_vars[["label"]] <-XBRL::xbrlProcessLabels(docL)
#   XBRL::xbrlFree(docL)

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



#' @importFrom magrittr "%>%"
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
    tidyr::spread(elementId, fact)
  
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

  
  rownames(res) <- res$contextId
  
  class(res) <- c("statement", "data.frame")
  return(res)
}


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
#' @param xbrl A file path, url or XBRL parsed object
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
xbrl_get_statements <- function(xbrl) {
  
  if(is.character(xbrl)) {
    # parse XBRL file
    opt1 <- unname(unlist(options("stringsAsFactors")))
    options(stringsAsFactors = FALSE)
    xbrl_vars <- xbrl_parse_min(xbrl)
    options(stringsAsFactors = opt1)
  } else {
    # xbrl is parsed xbrl
    if( !all( c("role", "calculation", "fact", "context") %in% names(xbrl)))
      stop("Input does not include necessery data from XBRL.")
    xbrl_vars <- xbrl
  }
  
  # get all statement types from XBRL
  role_ids <- xbrl_get_statement_ids(xbrl_vars)
  
  #get calculation link base relations
  relations <- lapply(role_ids, function(role_id){
    xbrl_get_relations(xbrl_vars, role_id)
  })
  names(relations) <- basename(role_ids)

  taxonomy_prefix <- "^us-gaap_"

  # store xbrl data in statement data structure
  statements <- 
    setNames(
      lapply(
        role_ids,
        function(role_id) {
          stat_name <- basename(role_id)
          links <- relations[[stat_name]]
          elements <- get_elements(links)
          res <- xbrl_get_data(elements, xbrl_vars)
          # delete taxonomy prefix
          names(res) <- gsub(taxonomy_prefix, "", names(res))          
          links$fromElementId <- gsub(taxonomy_prefix, "", links$fromElementId)          
          links$toElementId <- gsub(taxonomy_prefix, "", links$toElementId)          
          # set attributes
          attr(res, "role_id") <- stat_name
          attr(res, "relations") <- links
          res
        } 
      ),
      basename(role_ids)
    )
  class(statements) <- c("statements", "list")
  return(statements)
}

# relations <- r_z
# View(r_z)
# View(res_df)
xbrl_get_hierarchy <- function(relations) {
  
  lbase <- relations
  
  # start with top element of the presentation tree
  res_df <- 
    lbase %>%
    dplyr::anti_join(lbase, by = c("fromElementId" = "toElementId")) %>%
    dplyr::select(elementId = fromElementId) %>%
    unique()
  
  # breadth-first search
  while({
    df1 <- res_df %>%
      na.omit() %>%
      dplyr::left_join( lbase, by = c("elementId" = "fromElementId")) %>%
      dplyr::arrange(elementId, order) %>%
      dplyr::select(elementId, child = toElementId)  %>% 
      unique();
    nrow(df1) > 0
  }) 
  {
    # add each new level to data frame
    res_df <- res_df %>% dplyr::left_join(df1, by = "elementId")
    names(res_df) <-  c(sprintf("level%d", 1:(ncol(res_df)-1)), "elementId")
  }
  
  # add last level as special column (the hierarchy may not be uniformly deep)
  res_df["elementId"] <- 
    apply( t(res_df), 2, function(x){tail( x[!is.na(x)], 1)})
  
  # trim unused rows and columns
  res_df <- res_df[sapply( res_df, function(x) length(unique(x)) ) > 1] 
  res_df["elOrder"] <- 1:nrow(res_df) 
  class(res_df) <- c("xbrl_hierarchy", "data.frame")
  return(res_df)
}



#' Get the terminating nodes of the calculation hierarchy
#' 
#' @param x A statement object
#' @param parent_id Ascendant element - used as filter if defined
#' @param as_data_frame If TRUE, the return value is in a data frame format (defaults to FALSE)
#' @seealso \code{\link{get_relations}} and \code{\link{expose}}
#' @export
get_elements <- function(x, parent_id = NULL, as_data_frame = FALSE) {
  # returns all terminating elements 
  # if parent_id provided, only descendands from this elements are returned

  if( "statement" %in% class(x)  )
    links <- attr(x, "relations")
  else
    links <- x

  if( !"xbrl_relations" %in% class(links) )
    stop("Function get_elements needs a statement or xbrl_relation class.")
  
  hierarchy <- xbrl_get_hierarchy(links)
  
  if(!missing(parent_id)) {
    vec1 <- unique(unlist(
      lapply(hierarchy[grep("^level", names(hierarchy))], function(x) {
        hierarchy$elementId[x %in% parent_id]
      })
    ))
  } else {
    vec1 <- hierarchy$elementId
  }
  
  if(as_data_frame) 
    return(data.frame(elementId = vec1, stringsAsFactors = FALSE))
  else
    return(vec1)
}

#' Get calculation hierarchy of the statement elements
#' 
#' @param x a statement object
#' @export
#' @seealso \code{\link{get_elements}}, \code{\link{xbrl_get_statements}}
get_relations <- function(x) {
  return(attr(x, "relations"))
}


#' Collapses columns to specified groups
#' 
#' @param x a statement object
#' @param element_groups A list of groups with column names
#' @keywords internal
#' @export
fold <- function(x, element_groups) {
  if(!"statement" %in% class(x))
    stop("Not a financial statement data object")
  old_names <- names(x)
  old_relations <- get_relations(x)
  for(g in seq_len(length(element_groups))) {
    x[[names(element_groups)[g]]] <-  rowSums( x[ , element_groups[[g]] ])
    old_names[old_names == element_groups[[g]][1]] <- names(element_groups)[g]
  }
  old_names <- old_names[ !old_names %in%  unlist(element_groups) ]
  x <- x[,old_names]
  x  
}

file.path(R.home("doc"), "KEYWORDS")

#' Calculate higher order element values
#' 
#' @param x a statement object
#' @param ... elements of the statement
#' @return a statement object
#' @description Generate statement with agregated values. 
#' @details expose uses calculation link base hierarchy to find 
#' which concept to include in the aggregate. The aggregation function is
#' always \emph{sum}. See examples for more details.
#' @examples
#' \dontrun{
#' 
#' # total assets
#' st1$StatementOfFinancialPositionClassified %>%
#'   expose(
#'     "Assets" 
#'   )
#'
#' # current and non-current assets (using other)  
#' st1$StatementOfFinancialPositionClassified %>%
#'   expose(
#'     CurrentAssets = "AssetsCurrent", 
#'     NoncurrentAssets = other("Assets"),
#'     "LiabilitiesAndStockholdersEquity"
#'   )
#'   
#' # calculate non-current assets (using %without% operator)   
#' st1$StatementOfFinancialPositionClassified %>%
#'   expose(
#'     NoncurrentAssets = "Assets" %without% "AssetsCurrent",
#'     CurrentAssets = "AssetsCurrent", 
#'     "LiabilitiesAndStockholdersEquity"
#'   )
#' }
#' 
#' @seealso \link{finstr}
#' @export
expose <- function(x, ...) {
  elements <- list(...)
  # prepare for "other" and "without" directive
  used_cols <- c()
  for(i in seq_along(elements)) {
    cols <- get_elements(x, elements[[i]])
    if(elements[[i]][1] == "")
      cols <- get_elements(x)
    el_type <- attr(elements[[i]], "type")
    if( !is.null(el_type) && el_type == "other") {
      cols <- cols[ !cols %in% used_cols]
      elements[[i]] <- cols
    } 
    if( !is.null(el_type) && el_type == "without") {
      e1 <- get_elements(x, elements[[i]][[1]])
      e2 <- get_elements(x, elements[[i]][[2]])
      cols <- e1[!e1 %in% e2]
      elements[[i]] <- cols
    }
    used_cols <- union(used_cols, cols)
  }
  
  res <- 
    data.frame(
      do.call(
        cbind,
        lapply(elements, function(element) {
          cols <- get_elements(x, element)
          cbind(rowSums(x[cols]))
        })
      )
    )
  names(res) <- elements
  names(res)[names(elements)!=""] <- names(elements)[names(elements)!=""]
  res <- cbind(x[,1:4], res )
  class(res) <- c("statement", "data.frame")
  return(res)
}

#' Used in expose to sum the concepts not already used 
#' 
#' @param element element from concept hierarchy 
#' @keywords internal
#' @export
other <- function(element = NULL) {
  if(missing(element)){
    element <- ""
  }
  attr(element, "type") <- "other"
  return(element)
}

#' Used in expose to sum the concepts under e1 without e2 
#' 
#' @param e1 element group
#' @param e2 element group not to be included
#' @keywords internal
#' @export
'%without%' <- function(e1, e2) {
  x <- list(e1, e2)
  attr(x, "type") <- "without"
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
  
  # merge calculation hierarchies
  r_x <- get_relations(x)
  r_y <- get_relations(y)
  r_z <- NULL
  if(!is.null(r_x) && !is.null(r_x)) {
    r_z <- 
      unique(rbind(r_x[,1:2], r_y[,1:2])) %>%
      left_join(r_x, by = c("fromElementId", "toElementId")) %>%
      left_join(r_y, by = c("fromElementId", "toElementId")) %>%
      mutate( order = ifelse(is.na(order.x), order.y, order.x)) %>%
      select( -order.x, -order.y)
    class(r_z) <- class(r_x)
  }
  
  # merge facts and contexts
  z <- merge.data.frame(x, y, all = TRUE)
  z[is.na(z)] <- 0
  z <- z[!duplicated(z[c("startDate", "endDate")], fromLast = TRUE), ]
  z <- z[,c(names(z)[1:4], get_elements(z))]
  class(z) <- class(x)
  attr(z, "relations") <- r_z
  attr(z, "role_id") <- attr(x, "role_id")
  row.names(z) <- z$contextId
  return(z)  
}