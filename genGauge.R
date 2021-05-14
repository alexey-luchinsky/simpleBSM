library(plotly)


gen_gauge <- function(value, text = "Temperature", color = "gray", vMin = 20, vMax = 100) {
  fig <- plot_ly(
    type = "indicator",    mode = "gauge+number",
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = value,
    title = list(text = text),
    gauge = list( axis = list(range = list(vMin, vMax)),   bar = list(color = color))
  ) 
  fig
}