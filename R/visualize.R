#' make a functon to plot board
#'
#' @param x  a board
#'
#' @return to pot a graph
#' @export
#' @import tidyr
#' @import ggplot2
#' @examples plot.board(matrix)
#' plot(board(generate_board_mat()))
plot.board <- function(x){
  if (is_valid(x))
  {
    x_df<- tidyr::gather(data.frame(row = 1:nrow(x), x),
                         key = "column", value = "value", -row)
    x_df$column <- as.numeric(substr(x_df$column, 2, nchar(x_df$column)))
    x_df$value <- factor(x_df$value, levels = c(0,1,2))
    ggplot(data =x_df, aes(x=column , y=max(row)-row, fill = value)) + geom_tile(aes(fill = value))+
      scale_fill_manual(values = c("0" = "black", "1" = "white", "2" = "lightblue3")) +
      theme(legend.position = "none")+
      theme_void()+
      labs(x ="Row", y="Column", title = paste("Size", nrow(x)))
  }
}
