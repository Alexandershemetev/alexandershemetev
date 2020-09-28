#' alex_na_plot
#' @title Prepares a Graph Displaying Missing Values in Panel Data
#'
#' @description
#' Displays a heatmap of missing value frequency across the panel; 
#' Inspired by the ExPanDaR package approach as a basis
#' @param df Data frame containing the data.
#' @param ts_id A string containing the name of the variable indicating the time dimension. Needs to be coercible into an ordered factor.
#' @param colorx - color scheme in terms of terrain.colors function from grDevices package (will be loaded and/or installed automatically inside this function) 
#' The basic value = 10 (10 means: red color for many missings; yellow color for moderate missing; green color for no or almost no missings)
#' @param alphax - alpha of the color transparency function in terms of terrain.colors function from grDevices package; by default it is 1. 
#' @param no_factors A logical value indicating whether you want to limit the plot to
#'   logical and numerical variables. Defaults to \code{FALSE}.
#' @param binary If set to \code{TRUE}, the plot uses a binary scale only high-lightening
#'   whether values are missing or not. Defaults to \code{FALSE}.
#'
#' @return A \code{ggplot2} plot. ggplot2, data.table and grDevices packages will be loaded, installed and loaded to your system automatically inside this function - 
#' whatever your system needs. 
#' If a package is installed - it will be called; if not - it will be installed and called automatically (user selection for the mirror to download the package may be needed). 
#' @details
#'   This was inspired by a
#'   \href{http://www.rensenieuwenhuis.nl/r-sessions-30-visualizing-missing-values/}{blog post of Rense Nieuwenhuis}. And by ExPanDaR package :)
#' 
#'   Thanks! You are the best!
#'
#' This function has a useful add-on to adjust the design of the chart and select colors; by default it is intuitive reg-yellow-green scheme. 
#' Datasets with no missing values may be replicated yelow by default - nothing to compare with. You may change the color scheme and design any time.  
#' @examples
#' library(plm); data("Produc", package="plm"); Produc$test = ifelse(Produc$unemp > 5, Produc$unemp, NA) #Adding NAs occasionally to the dataset
#' alex_na_plot(Produc, ts_id = "year")
#' @export


alex_na_plot <- function(df, ts_id, no_factors = FALSE, binary = FALSE, colorx = 10, alphax = 1) {
  
  install_and_load = function(name, char = T)
{
  if (!require(name, character.only = char)) 
  {
    install.packages(name)
  }
  require(name, character.only = char)
}

 #install_github("dgrtwo/broom")
# library(broom)
sapply(
  c("data.table", "ggplot2", "grDevices"),
  install_and_load
)
rm(install_and_load)

  
  
  
  # Make devtools:check() and CRAN happy
  value <- NULL
  if(! is.data.frame(df)) stop("df needs to be a dataframe")
  if (! ts_id %in% names(df)) stop("'ts_id' needs to be present in data frame 'df'")
  if (any(is.na(df[,ts_id]))) stop("'ts_id' must not contain missing values")
  if (! is.logical(no_factors)) stop("'no_factors' needs to be a logical value")
  if (! is.logical(binary)) stop("'binary' needs to be a logical value")
  
  df <- as.data.frame(df)
  df[,ts_id] <- as.ordered(df[,ts_id])
  
  if (no_factors)
    df <- cbind(df[ts_id], df[sapply(df, is.logical) | sapply(df, is.numeric)])
  else
    df <- cbind(df[ts_id], df[, !(ts_id == names(df))])
  
  nas <- matrix(ncol=ncol(df) - 1, nrow=nlevels(df[,ts_id]))
  for (i in 2:ncol(df))
  {
    if (!binary) nas[,i - 1] <- tapply(df[,i], df[,ts_id], function(x) sum(is.na(x)) / length(x))
    else  nas[,i - 1] <- tapply(df[,i], df[,ts_id], function(x) sum(is.na(x)) > 0)
  }
  mv <- data.frame(levels(df[, ts_id]), nas)
  names(mv) <- c(ts_id, names(df)[2:ncol(df)])
  if (!anyNA(suppressWarnings(as.numeric(as.character(mv[,ts_id])))))
    mv[,ts_id] <- as.numeric(as.character(mv[,ts_id]))
  mv <- tidyr::gather_(mv, key_col = "variable", value_col = "value",
                       gather_cols = names(df)[2:ncol(df)])
  mv$variable <- factor(mv$variable, levels = names(df)[2:ncol(df)])
  
  p <- ggplot2::ggplot(mv, ggplot2::aes_string("variable", ts_id)) +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90),
                   axis.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 8),
                   legend.title = ggplot2::element_text(size = 8)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour="white") # grid color of lines in chart
  
  if (!binary)  {
    p <- p +
      ggplot2::labs(fill = "% missing\n") +
      ggplot2::scale_fill_continuous(labels=scales::percent)
  } else {
    p <- p +
      ggplot2::labs(fill = "Has missing values?") +
      ggplot2::scale_fill_discrete(labels=c("No", "Yes"))
  }
  #  p +    scale_fill_gradientn(colours = terrain.colors(10)) # Green for no NAs, Yellow for 50% Nas, Red for 90% NAs
  colorx = colorx
  alphax = alphax
  p +    scale_fill_gradientn(colours = terrain.colors(colorx, alpha = alphax)) #+ theme( plot.background = element_rect(fill = "white"),    panel.background = element_rect(fill = "white"),    axis.line.x = element_line(color = "green")) # first and second colors - for color of area around chart; third color - line between names and value (one line)
  
  
}