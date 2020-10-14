# alex_na_plot
if (require(ggplot2)){
library(plm); data("Produc", package="plm"); Produc$test = ifelse(Produc$unemp > 5, Produc$unemp, NA) #Adding NAs occasionally to the dataset
ts_id = "year"
df <- Produc
df <- as.data.frame(df)
  df[,ts_id] <- as.ordered(df[,ts_id])

    df <- cbind(df[ts_id], df[, !(ts_id == names(df))])
  
  nas <- matrix(ncol=ncol(df) - 1, nrow=nlevels(df[,ts_id]))
  for (i in 2:ncol(df))
  {
   nas[,i - 1] <- tapply(df[,i], df[,ts_id], function(x) sum(is.na(x)) / length(x))

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

    p <- p +
      ggplot2::labs(fill = "% missing\n") +
      ggplot2::scale_fill_continuous(labels=scales::percent)

  test_that("geom_tile_label layer is a geom_tile", {

  expect_is(p$layers[[1]]$geom, "GeomTile")
})

}