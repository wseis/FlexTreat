library(tidyverse)

# creating fake population distribution
influent <- rnorm(10000, 7, 0.5)
effluent <- rnorm(10000, 1.5, 0.5)

# Calculating log-removal values
lrv <- log10(10^influent/10^effluent)

# data.frame for population
df_pop <- data.frame(Influent = influent,
                 Effluent = effluent,
                 LogRemoval = lrv,
                 sampling = "grab sampling")

# variances
var(influent) + var(effluent)
var(lrv)

# standard deviations
sqrt(var(influent) + var(effluent))
sd(lrv)

# create fake samples from population distribution
composites_in <- replicate(n = 10000, 
                           expr = log10(mean(10^sample(influent, 20))))

composites_out <- replicate(n = 10000, 
                            expr = log10(mean(10^sample(effluent, 20))))

lrv_composites <- log10(10^composites_in/10^composites_out)


# data.frame for composites
df_comp <- data.frame(Influent = composites_in,
                     Effluent = composites_out,
                     LogRemoval = lrv_composites,
                     sampling = "composite of 20 grab samples")


# combined data frames
df <- bind_rows(df_pop, df_comp)

# plotting the data
df.tidy <- df %>% gather(quantity, value, -sampling)


df.tidy$quantity <- factor(x = df.tidy$quantity,
                           levels = c("Influent", 
                                      "Effluent",
                                      "LogRemoval"),
                           labels =  c("Influent", 
                                       "Effluent",
                                       "Log-Removal"),
                           ordered = T) 



df.label <- df.tidy %>% 
  filter(quantity == "LogRemoval") %>% 
  group_by(sampling) %>% 
  summarise(gt_5 = mean(value > 5),
            gt_6 = mean(value > 6))




grab_vs_comp <- ggplot(df.tidy, aes(x = value, fill = sampling)) + 
  geom_density(alpha = .5) + 
  facet_grid(.~quantity, scales = "free") +
  scale_x_continuous(breaks = 0:9) +
  xlab(label = "MO concetration [lg] / calculated LRV")+
  ggthemes::scale_fill_economist() + 
  
  ggthemes::theme_igray(base_size = 14)+
  ggtitle("Composites and grab samples for LRV calculation") +
  theme(legend.position = "bottom", 
        legend.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
  
        plot.background = element_rect(fill = "white"))

grab_vs_comp

ggsave(filename = "comp_vs_grab_illustration.png", 
       plot = grab_vs_comp,
       device = "png",
       width = 25, 
       height = 15,
       units = "cm",
       dpi = "screen",
       bg = "white")






