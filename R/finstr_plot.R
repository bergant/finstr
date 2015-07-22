




`%ggplot_replace%` <- function(e1, e2) 
{
  e1[names(e2)] <- e2
  e1
}

#' Plot double stacked bar 
#' 
#' @param x a statement object with
#' @param by_date if FALSE facets will be grouped by first element level
#' @param is_diff if TRUE data is rearanged to support negative values
#' @param dif_labels vector with two new labels in case of diff mode 
#' @param ... other attributes
#' @export
plot_double_stacked_bar <- function(
  x, by_date = TRUE, is_diff = FALSE, dif_labels = NULL, ... ) {
      
  if( !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # x <- bs1
  gg_data <- 
    x %>%
    reshape_long(levels = 2) %>%
    dplyr::arrange_(~parent_id, ~desc(element_id))
  
  gg_data$label <- factor(gg_data$label, levels = unique(gg_data$label))
  gg_data$parent_label <- factor(gg_data$parent_label, levels = unique(gg_data$parent_label))
  

  
  if(is_diff) {
    
    if(missing(dif_labels)) {
      dif_labels <- c("Consumption", "Supply")
    }
    
    gg_data <- gg_data %>%
      dplyr::mutate_(
        side = ~ ifelse(side == names(x)[[1]],
                        ifelse(Value >=0, dif_labels[1], dif_labels[2]),
                        ifelse(Value >=0, dif_labels[2], dif_labels[1])
        ),
        Value = ~ abs(Value)
      )
  }
  # wrap labels
  levels(gg_data$label) <-
    sapply(strwrap(levels(gg_data$label), width=15, simplify=FALSE), 
           paste, collapse="\n")
  levels(gg_data$parent_label) <-
    sapply(strwrap(levels(gg_data$parent_label), width=15, simplify=FALSE), 
           paste, collapse="\n")
  
  # decimals
  gg_data$value <- gg_data$value * 10 ^ gg_data$decimals
  
  # colors
  data_dims <- colSums(table(unique(gg_data[,c("element", "parent")])))
  
  bs_colors <- c(
    hcl(h = 240, c = 30, l = seq(30, 80, len = data_dims[1])),
    hcl(h = 120, c = 30, l = seq(30, 80, len = data_dims[2]))
  )

  g1 <-      
    ggplot2::ggplot(data = gg_data) +
    ggplot2::geom_bar(
      ggplot2::aes_string(
        y = "value", 
        x = ifelse(by_date, "parent_label","date"), 
        fill = "label"), 
      stat="identity", position='stack') +
    #geom_text(aes(label = pos, y = pos, x = side), size = 3) +
    ggplot2::scale_fill_manual(values = bs_colors) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL, reverse = TRUE)) +
    ggplot2::theme_minimal() %ggplot_replace%
    ggplot2::theme( 
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
  
  if(by_date) {
    g1 + ggplot2::facet_grid( ~ date )
  } else {
    g1 + ggplot2::facet_grid( ~ parent_label)
  }
      
}
    
#' Plot waterfall
#' 
#' @description Plots income statement as waterfall bar chart
#' @details Requires ggplot2. 
#' @param x a statement object (usually income statement)
#' @param date end date of selected period
#' @param ... other attributes
#' @export
plot_waterfall <- function(x, date = NULL, ...) {
  
  if( !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if( !requireNamespace("scales", quietly = TRUE)) {
    stop("Package scales needed for this function to work. Please install it.",
         call. = FALSE)
  }
  library(ggplot2)

  row_num <- 1
  if(!missing(date)) {
    row_num <- which(x$endDate == date) 
  } else {
    if( nrow(x) > 1 ) {
      row_num <- nrow(x)
    } 
  }
  x <- x[row_num, ]
  
  decimals <- x$decimals
  
  elements <- 
    get_elements(x, all = FALSE)
  elements$sign <- 
    ifelse(elements$balance == "credit", 1, -1)
  elements$value <- 
    unlist( x[1, elements$elementId]) * elements$sign * 10 ^ x$decimals
  elements$position <- 
    cumsum(c(0, elements$value[-nrow(elements)]))
  elements$time <- 1:nrow(elements)
  
  elements$labelString <-
    sapply(strwrap(elements$labelString, width=15, simplify=FALSE), 
           paste, collapse="\n")
  
  
  # Adapted from http://vita.had.co.nz/papers/ggplot2-wires.pdf (waterfall example):
  ggplot2::ggplot(elements) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey", size = 0.5) +
    ggplot2::geom_rect(ggplot2::aes_string(xmin = "time - 0.45", xmax = "time + 0.45", 
                  ymin = "position", ymax = "position + value", fill = "factor(sign)")) +
    ggplot2::geom_text(ggplot2::aes_string(
      x = "time", 
      y = "pmin(position, position + value) - 50", label = "scales::comma(value)"),
      hjust = 0.5, vjust = 1, size = 3) +
    ggplot2::scale_x_continuous( breaks = elements$time, labels = elements$labelString) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_fill_manual(values = c("-1" = "#9ecae1", "1" = "#3182bd"), guide = "none") +    
    ggplot2::theme_bw() %ggplot_replace% 
    ggplot2::theme( axis.title.x = ggplot2::element_blank(),
                    axis.title.y = ggplot2::element_blank(),
                    axis.ticks.x = ggplot2::element_blank(),
                    axis.ticks.y = ggplot2::element_blank(),
                    panel.grid.major.x = ggplot2::element_blank(),
                    panel.grid.minor.x = ggplot2::element_blank(),
                    panel.grid.minor.y = ggplot2::element_blank(),
                    panel.border = ggplot2::element_blank()
    )
}

#' autoplot statement
#' @param x statement object
#' @param ... other params
#' @export
#' @keywords internal
autoplot.statement <- function(x, ...) {
  if( sum(is.na(get_elements(x)[["parentId"]] )) ) {
    plot_double_stacked_bar(x, ...)
  } else {
    plot_waterfall(x, ...)
  }
}


