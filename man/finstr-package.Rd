\name{finstr-package}
\alias{finstr}
\docType{package}
\title{finstr: financial statements in R}
\description{
  finstr provides data structures and functions for financial statement data
  manipulation
}
\section{Financial Statements}{

Financial statements data contain many variables (concepts, elements). 
Several concepts are aggregations of other concepts. 
Specific aggregations are needed for each purpose of 
financial statement analysis.

Package finstr provides the necessary data structures and functions to 
manipulate financial statements data in R.
}

\section{Getting Data to Statement Structure}{

\itemize{
\item
  Convert XBRL (XML files and schemas) to a list of data frames 
  (facts, contexts, roles and hierarchies) with \link[XBRL]{xbrlDoAll} 
  function from \link[XBRL]{XBRL-package}. 
\item
  Function \code{\link{xbrl_get_statements}} converts the XBRL data list
  to \emph{statements} object. It is a list of 
  several \emph{statement} objects (ballance sheets, income and cash 
  flow statements) which are data frames with elements as columns and periods
  as rows.
\item   
  Information about calculation hierarchy of concepts and their descriptions 
  is stored as an attribute to the statement object
  and can be retrieved by \code{\link{get_elements}}.
}

}
\section{Using Financial Statements Data}{
\itemize{
\item
  Function \code{\link{merge.statement}} can merge statements from different
  periods to a single statement object. The elements and hierarchies are merged to
  provide comfortable analysis of data over several periods.
\item To calculate values with formulas   
  use \code{\link{calculate}} function or use 
  statement as a data frame with R base functions and packages 
  like \link[dplyr]{dplyr} for manipulating data 
  frames.
}
For real data examples see vignettes: \code{browseVignettes("finstr")}

}

\author{
Darko Bergant

Maintainer: Darko Bergant <darko.bergant@gmail.com>
}
\references{

http://en.wikipedia.org/wiki/Financial_statement

http://www.xbrl.org
}

\keyword{ package finstr XBRL financial statement analysis}

