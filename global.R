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
      mutate(x = str_trunc(x, 60)) 
  }
  data <- data %>% 
    filter(x != "NA") %>% 
    group_by(x) %>% 
    tally(sort = TRUE, name = "recuento") %>% 
    mutate(x = as_factor(x))
  
  p <- ggplot(data)
  
  if (orientation == "x"){
    p + 
      geom_col(aes(x = x, y = recuento, fill = x)) +
      geom_label(aes(x = x, y = recuento, label = recuento))
  }  else if (orientation == "y"){
    p + 
      geom_col(aes(y = x, x = recuento, fill = x)) +
      geom_label(aes(y = x, x = recuento, label = recuento))
  }
  
}

my_wrap <- function(..., width = 35){
  scale_y_discrete(..., labels = function(x) str_wrap(x, width = width))
}
