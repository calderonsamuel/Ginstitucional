mi_pivot <- function(.df, .str){
  .df %>% 
    select(contains(.str)) %>% 
    pivot_longer(cols = everything()) %>% 
    filter(value != "")
}

my_plot <- function(df, columna, separate = FALSE, orientation = "x"){
  data <- select(df, x = contains(columna))
  if (separate == TRUE) {
    data <- 
      separate_rows(data, x, sep = ", ") %>% 
      mutate(x = str_trunc(x, 60)) %>% 
      filter(x != "NA")
  }
  p <- ggplot(data, aes(fill = x))
  
  if (orientation == "x"){
    p + geom_bar(aes(x = fct_infreq(x)))
  }  else if (orientation == "y"){
    p + geom_bar(aes(y = fct_infreq(x)))
  }
  
}

my_wrap <- function(..., width = 35){
  scale_y_discrete(..., labels = function(x) str_wrap(x, width = width))
}
