mi_pivot <- function(.df, .str){
  .df %>% 
    select(contains(.str)) %>% 
    pivot_longer(cols = everything()) %>% 
    filter(value != "")
}

my_pipe <- function(df, separate = FALSE, usar_porc = FALSE){
  data <- df
  if (separate == TRUE) {
    data <- 
      separate_rows(data, x, sep = ", ") %>% 
      filter(x != "," & x != " ") 
  }
  data <- data %>% 
    filter(x != "NA" & x != "") %>% 
    group_by(x) %>% 
    tally(sort = TRUE, name = "recuento") %>% 
    arrange(recuento) %>% 
    mutate(x = str_trunc(x, 80),
           x = as_factor(x),
           porcentaje = recuento/nrow(df))
  
  if(usar_porc){
    data <- select(data, x, valor = porcentaje)
  } else {
    data <- select(data, x, valor = recuento)
  }
  data
}

my_plot <- function(df, paleta = "Set1", usar_porc = FALSE){
  filas <- nrow(df)
  
  p <- ggplot(df, aes(y = x, x = valor)) +
      geom_col(aes(fill = x), color = "gray50")
  if(usar_porc){
    p <- p + 
      geom_label(aes(label = scales::percent(valor, 0.1))) +
      scale_x_continuous(labels = scales::label_percent())
  } else {
    p <- p + geom_label(aes(label = valor))
  }
  p +
    my_wrap() + 
    scale_fill_manual(values = as.vector(pals::polychrome(filas)))
    
}

my_wrap <- function(..., width = 35){
  scale_y_discrete(..., labels = function(x) str_wrap(x, width = width))
}

my_theme <- theme(legend.position = "none", 
                  axis.title.y = element_blank(),
                  panel.background = element_blank(), 
                  panel.border = element_rect(fill = NA, colour = "black"),
                  plot.title = element_text(size = 16))


