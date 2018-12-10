waffleFun <- function(values, names, waffleTitle, numRows){
  val_names <- sprintf("%s (%s)", names, percent(round(values/sum(values), 2)))
  names(values) <- val_names
  waffle(values, title = waffleTitle, rows = numRows, colors = c('gold', 'hotpink3', 'mediumturquoise', 'purple', 'midnightblue', 'mediumvioletred', 'plum', 'red', 'steelblue4', 'tan4')) + theme(plot.background = element_rect(fill = "snow1"))
}