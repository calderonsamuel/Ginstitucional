mi_pivot <- function(.df, .str){
  .df %>% 
    select(contains(.str)) %>% 
    pivot_longer(cols = everything()) %>% 
    filter(value != "")
}

my_pipe <- function(df, columna, separate = FALSE){
  
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
  data
}

my_graph <- function(df, orientation = "x"){
  ggplot(data, aes(x = x, recuento)) +
    geom_col(aes(fill = x)) +
    geom_label(aes(label = recuento))
}

my_plot <- function(df, orientation = "x", paleta){
  
  p <- ggplot(df)
  
  if (orientation == "x"){
    p <- p + 
      geom_col(aes(x = x, y = recuento, fill = x)) +
      geom_label(aes(x = x, y = recuento, label = recuento))
  }  else if (orientation == "y"){
    p <- p + 
      geom_col(aes(y = x, x = recuento, fill = x)) +
      geom_label(aes(y = x, x = recuento, label = recuento))
  }
  p +
    my_wrap() + 
    scale_fill_brewer(palette = paleta) +
    theme(legend.position = "none")
}

my_wrap <- function(..., width = 35){
  scale_y_discrete(..., labels = function(x) str_wrap(x, width = width))
}
