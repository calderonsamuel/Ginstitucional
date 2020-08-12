mi_pivot <- function(.df, .str){
  .df %>% 
    select(contains(.str)) %>% 
    pivot_longer(cols = everything()) %>% 
    filter(value != "")
}

my_pipe <- function(df, columna, separate = FALSE, usar_porc = FALSE){
  
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
    mutate(x = as_factor(x),
           porcentaje = recuento/sum(recuento))
  
  if(usar_porc){
    data <- select(data, x, valor = porcentaje)
  } else {
    data <- select(data, x, valor = recuento)
  }
  data
}

my_plot <- function(df, paleta){
  
  p <- ggplot(df, aes(y = x, x = valor)) +
      geom_col(aes(fill = x)) +
      geom_label(aes(label = valor))
  p +
    my_wrap() + 
    scale_fill_brewer(palette = paleta) +
    theme(legend.position = "none", axis.title.y = element_blank())
    
}

my_wrap <- function(..., width = 35){
  scale_y_discrete(..., labels = function(x) str_wrap(x, width = width))
}
