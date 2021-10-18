library(ggplot2)
library(plotly)

get_sample_size <- function(target = 0.9, nfailure){
  
  x = 1
  
  while(1-pbeta(target, x, nfailure) < 0.95){
    x = x+1

  }
  return(x)
}

get_sample_size(nfailure = 0)

1- pbeta(.9, 0 ,0)

df <- data.frame(nfailures = 1:25, nsuccess = 1)


for ( i in df$nfailures){
  df$nsuccess[i] <- get_sample_size(.9, i)
}



label_data <- data.frame( x = c(4, 20),
                          y = c(250, 50),
                          text = c("p < 0.05 (gut)","p > 0.05 (schlecht)"))
plot <- ggplot(df, aes(x = nfailures, y = nsuccess)) + 
  geom_line(lwd = 2, col = "dark grey") +
  scale_y_continuous(breaks = seq(0, 300, 50))+
  scale_x_continuous(breaks = seq(0, 26, 2))+
  ylab("N LRV > Zielwert") +
  xlab('N LRV < Zielwert') +
  geom_label(inherit.aes = F, data = label_data,  mapping = aes(x = x, 
                           y = y, 
                           label= text,
                           fill = text), 
             label.size = .1,
             show.legend = F )+ 
  scale_fill_manual(values = rev(c("red3", "steelblue")))+
  ggtitle("Beta-Verteilungsparameter für P(p > 0.9) > 0.95") 

ggsave(filename = "beta_samples.png", 
       plot = plot,
       device = "png",
       width = 15, 
       height = 10,
       units = "cm",
       dpi = "screen",
       bg = "white")


# Beispielabbildungen

l <- list()
nsuc <- c(1, 2, 3,4,  5, 10, 15,20, 30)
for ( i in nsuc){
  l[[as.character(i)]] <- dbeta(x = seq(0, 1, length.out = 100), i, 1) 

}

df <- l %>% dplyr::bind_rows() %>% tidyr::gather(nsuccess, value)
df$increments <- rep(seq(0, 1, length.out = 100), length(nsuc))
df$nsuccess <- as.numeric(df$nsuccess)
summary(df)

df$group <- ifelse(df$increments <= 0.9, "P(rate < 0.9)", "P(rate > 0.9)")

multiples <- ggplot(df, aes(x = increments, 
               y = value, 
               group = group,
               fill = group)) + 
  geom_line() + 
  geom_ribbon(data=subset(df,
                          increments < 1),
              aes(x= increments, 
                  ymax = value),
              ymin = 0 ,alpha=0.3) +
  scale_fill_manual(name='', 
                    values=c("P(rate > 0.9)" = "green4",
                             "P(rate < 0.9)" = "red3"))+
  
  facet_wrap(.~as.factor(nsuccess), 
             scales = "free_y")+

  xlab("geschätzte Rate") + 
  ylab("Wahrscheinlichkeitsdichte")  +
  theme(legend.position = "bottom")

  

ggsave(filename = "beta_illustration.png", 
       plot = multiples,
       device = "png",
       width = 25, 
       height = 15,
       units = "cm",
       dpi = "screen",
       bg = "white")

