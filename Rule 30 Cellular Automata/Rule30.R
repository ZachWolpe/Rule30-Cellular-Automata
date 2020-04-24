

library(ggplot2)
library(tidyverse)




#     for last cell 

Rule30 <- function(half_len, iterations) {
  
  # Functional Rules 
  half_len <- 2*half_len
  a = c('111', '110', '101', '100', '011', '010', '001', '000')
  b = c(0, 0, 0, 1, 1, 1, 1, 0)
  
  instructions <- c()
  for (i in 1:length(a)) {instructions[[a[i]]] <- b[i]}
  
  # Init data
  z <- c(rep(0, half_len), 1, rep(0, half_len))
  data <- data.frame(x=1:length(z), y=iterations, z=z)
  
  # computation 
  n <- nrow(data)
  x2 <- 1:n
  y_num <- iterations-1
  final <- data
  
  for (c in 1:iterations) {
    
    z2 <- c()
    for (i in 1:n){
      c2 = data$z[i]
      if (i==1) {
        c1 = 0
        c3 = data$z[i+1]
      }
      else if (i==n) {
        c1 = data$z[i-1]
        c3 = 0
      }
      else {
        c1 = data$z[i-1]
        c3 = data$z[i+1]
      }
      
      # compute 
      code = paste(toString(c1), toString(c2), toString(c3), sep='')
      # add to next row
      z2 <- c(z2, instructions[code])
    }
    y2 <- rep(y_num,n)
    y_num = y_num - 1
    data <- data.frame(x=x2, y=y2, z=z2)
    final <- rbind(final, data.frame(x=x2, y=y2, z=z2))
  }

  # remove buffer
  #final <- final[final$x == (half_len+1):(max(final$x)-half_len),]
  return(final)
}

# vis
visualize <- function(data, c1='white', c2='#779ecb') {
  data %>% 
    ggplot(aes(x=x, y=y, fill = factor(z))) +
    geom_raster()  +
    scale_fill_manual(breaks = levels(factor(data$z)),
                      values = c(c1, c2)) +
    theme(legend.position = 'none',
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          aspect.ratio = 0.9) +
    scale_x_continuous(expand = c(0, 0), limits = c(range(data$x)[1]+half_len, range(data$x)[2]-half_len)) +
    scale_y_continuous(expand = c(0, 0), limits = c(round(max(data$y)*0.2), max(data$y)))
}



half_len = 100
final = Rule30(half_len, 500)

visualize(final, c1='navy', c2='pink')









