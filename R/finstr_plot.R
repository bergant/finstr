
#' Plot double stacked bar 
#' 
#' @param x a list with two data frames (sides)
#' @param by_date if TRUE, facets will be grouped by date
#' @param is_diff if TRUE data is rearanged to support negative values
#' @param dif_labels vector with two new labels in case of diff mode 
#' @export
plot_double_stacked_bar <- 
  function(x, by_date = FALSE, is_diff = FALSE, dif_labels = NULL ) {
  
  if( !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  gg_data <-
    do.call(
      rbind,
      lapply(names(x), function(side) {
        
        df_side <- 
          cbind(x[[side]], side) %>%
          tidyr::gather_("Element", "Value", names(x[[side]])[-1])
        df_side <- df_side[nrow(df_side):1,]
      })
    )
  
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
  
  
  bs_colors <- 
    c(
      hcl(h = 240, c = 30, l = seq(80, 30, len = ncol(x[[1]]) - 1)),
      hcl(h = 120, c = 30, l = seq(80, 30, len = ncol(x[[2]]) - 1))
    )
  
  
  if(by_date) {
    aes1 <- ggplot2::aes_string(y = "Value", x = "side", fill = "Element")
  } else {
    aes1 <- ggplot2::aes_string(y = "Value", x = "date", fill = "Element")
  }
  
  g1 <- 
    ggplot2::ggplot(data = gg_data) +
    ggplot2::geom_bar(aes1, stat="identity", position='stack') +
    ggplot2::theme_bw() + 
    ggplot2::scale_fill_manual(values = bs_colors) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
    ggplot2::xlab(label = NULL) + 
    ggplot2::ylab(label = NULL)
  
  if(by_date) {
    g1 + ggplot2::facet_grid( ~ date)
  } else {
    g1 + ggplot2::facet_grid( ~ side)
  }
  
}
