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
financial statement analysis .

Package finstr provides the necessary data structures and functions to 
manipulate financial statements data in R.
}

\section{Getting Data to Statement Structure}{

\itemize{
\item
  Convert XBRL (XML files and schemas) to a list of data frames 
  (facts, contexts, roles and hierarchies) with \code{\link{xbrl_parse_min}} 
  function. It is using XBRL parsing primitives from \link[XBRL]{XBRL-package} 
  to parse only minimum data needed to construct financial statements.
\item
  Function \code{\link{xbrl_get_statements}} converts the XBRL data list
  to \emph{statements} object. It is a list of 
  several \emph{statement} objects (ballance sheets, income and cash 
  flow statements) which are data frames with elements as columns and periods
  as rows.
\item Only terminal (lowest level) concepts and values are kept in statement
  object's columns. 
  Information about hierarchical definition of higher order concepts is stored 
  as an attribute to the statement object
  and can be retrieved by \code{\link{get_relations}}.
\item Statement objects are printed in transposed format. In most cases this 
  improves visibility of the statement content since there are usually more
  columns than rows. To see the statement in its data frame form use 
  print.data.frame.
}
}
\section{Using Financial Statements Data}{
\itemize{
\item To calculate values of higher order concepts and concept 
  combinations (e.g. \emph{non-current assets}) use \code{\link{expose}} function.
  The result of expose function is a statement object with transformed concepts 
  (data frame columns).
\item
  Function \code{\link{merge.statement}} can merge statements from different
  periods to a single statement. The elements and hierarchies are merged to
  provide comfortable analysis of data over several periods.
\item To calculate values from existing columns in statements use 
  R base functions or packages like \link[dplyr]{dplyr} for manipulating data 
  frames.
}
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

