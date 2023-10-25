################## PLOTS
rm(list=ls()) # Clear memory

# Load libraries

library(ggplot2)
library(cowplot)
# devtools::install_github("awhstin/awtools")
#library(awtools)
# install.packages("cartography")
#library(cartography)
library(RColorBrewer)

### read in RDS
flies1 <- readRDS('flies1.RDS')%>%
  dplyr::filter(sim <1001)%>%
  glimpse()

flies50 <- readRDS('flies50.RDS')%>%
  glimpse()

flies5 <- readRDS('flies5.RDS')%>%
  glimpse()

allflies <- bind_rows(flies1, flies50)
tail(allflies)
str(allflies)

allflies <- bind_rows(allflies, flies5)%>%
  glimpse()
str(allflies)

#TODO Sort out squishing of plots
#TODO Lure 20 with ineff2 instead of ineff

## Legend Labels
cust <- c("Efficient", "Expert 1", "Expert 2", "Grid", "Inefficient", "Random 1", "Random 2", "Random 3", "Reduced Grid")

######### lure 5m strength
cdf.time5 <- ggplot(
  data=flies5, aes(x=time, group = type, col = type)) +
  stat_ecdf()+
  scale_color_brewer(palette = 'Paired')+
  theme_minimal()+
  labs(col = 'Type', x = 'Days Until Detection', y = 'Cumulative Distribution Function')+
  theme(legend.position = "none")


cdf.time5

#number of trees infested at first detection

cdf.trees5 <- ggplot(
  data=flies5, aes(x=ntrees, group = type, col = type)) +
  stat_ecdf()+
  scale_color_brewer(palette = 'Paired', labels = cust)+
  theme_minimal()+
  labs(col = 'Type', x = 'Infested Trees', y = 'Cumulative Distribution Function')

cdf.trees5

######### lure 20m strength
cdf.time20 <- ggplot(
  data=flies1, aes(x=time, group = type, col = type)) +
  stat_ecdf()+
  scale_color_brewer(palette = 'Paired')+
  theme_minimal()+
  labs(col = 'Type',x = 'Days Until Detection', y = 'Cumulative Distribution Function')+
  theme(legend.position = "none")

cdf.time20

#number of trees infested at first detection

cdf.trees20 <- ggplot(
  data=flies1, aes(x=ntrees, group = type, col = type)) +
  stat_ecdf()+
  scale_color_brewer(palette = 'Paired', labels = cust)+
  theme_minimal()+
  labs(col = 'Type', x = 'Infested Trees', y = 'Cumulative Distribution Function')

cdf.trees20

######### lure 50m strength
cdf.time50 <- ggplot(
  data=flies50, aes(x=time, group = type, col = type)) +
  stat_ecdf()+
  
  scale_color_brewer(palette = 'Paired')+
  theme_minimal()+
  labs(x = 'Days Until Detection', y = 'Cumulative Distribution Function')+
  #coord_cartesian(xlim= c(0,400))+
  theme(legend.position = "none")

cdf.time50

#number of trees infested at first detection

cdf.trees50 <- ggplot(
  data=flies50, aes(x=ntrees, group = type, col = type)) +
  stat_ecdf()+
  scale_color_brewer(palette = 'Paired', labels = cust)+
  theme_minimal()+
  labs(col = 'Type', x = 'Infested Trees', y = 'Cumulative Distribution Function')

cdf.trees50

## 90% ad 99% worst case scenarios

bdat <- flies5 %>%
  arrange(time)

head(bdat)
tail(bdat)

slice(bdat, 0.99*9000)



lure20 <- ggplot(flies1, aes(x = time))+
  geom_histogram(binwidth = 10)+
  theme_minimal()+
  scale_y_continuous(limits = c(0,3000))+
  scale_x_continuous(limits = c(0,350))+
  scale_fill_brewer(palette = 'Paired', labels = cust)+
  geom_vline(xintercept = 91, col = 'grey60', linetype = 'dashed')+
  geom_vline(xintercept = 160, col = 'grey60')+
  labs(fill = 'Type', x= '', y = 'Simulations')+
  theme(axis.text.x = element_blank(),
        axis.line.x = element_line())

lure20

lure5 <- ggplot(flies5, aes(x = time))+
  geom_histogram(binwidth = 10)+
  theme_minimal()+
  scale_y_continuous(limits = c(0,3000))+
  scale_x_continuous(limits = c(0,350))+
  scale_fill_brewer(palette = 'Paired', labels = cust)+
  geom_vline(xintercept = 145, col = 'grey60', linetype = 'dashed')+
  geom_vline(xintercept = 233, col = 'grey60')+
  labs(fill = 'Type', x= '', y = 'Simulations')+
  theme(axis.text.x = element_blank(),
        axis.line.x = element_line())

lure5

lure50 <- ggplot(flies50, aes(x = time))+
  geom_histogram(binwidth = 10)+
  theme_minimal()+
  scale_y_continuous(limits = c(0,3000))+
  scale_x_continuous(limits = c(0,350))+
  scale_fill_brewer(palette = 'Paired', labels = cust)+
  geom_vline(xintercept = 77, col = 'grey60', linetype = 'dashed')+
  geom_vline(xintercept = 134, col = 'grey60')+
  labs(fill = 'Type', x = 'Days to Detection', y = 'Simulations')+
  theme(
    axis.line.x = element_line()
  )

lure50

## trees worst case scenario
# bdat <- flies5 %>%
#   arrange(ntrees)
# 
# slice(bdat, 0.9*9000)

trees5 <- ggplot(flies5, aes(x = ntrees))+
  geom_histogram(binwidth = 10)+
  geom_vline(xintercept = 257, col = 'grey60', linetype = 'dashed')+
  geom_vline(xintercept = 728, col = 'grey60')+
  theme_minimal()+
  scale_y_continuous(limits = c(0,1600))+
  scale_x_continuous(limits = c(0,800))+
  labs(x= '', y = 'Simulations')+
  theme(axis.text.x = element_blank(),
        axis.line.x = element_line())

trees5

####
# 
# bdat <- flies1 %>%
#   arrange(ntrees)
# 
# slice(bdat, 0.99*9000)

trees20 <- ggplot(flies1, aes(x = ntrees))+
  geom_histogram(binwidth = 10)+
  geom_vline(xintercept = 65, col = 'grey60', linetype = 'dashed')+
  geom_vline(xintercept = 267, col = 'grey60')+
  theme_minimal()+
  scale_y_continuous(limits = c(0,1600))+
  scale_x_continuous(limits = c(0,800))+
  labs(x= '', y = 'Simulations')+
  theme(axis.text.x = element_blank(),
        axis.line.x = element_line())

trees20

####

# bdat <- flies50 %>%
#   arrange(ntrees)
# 
# slice(bdat, 0.9*9000)

trees50 <- ggplot(flies50, aes(x = ntrees))+
  geom_histogram(binwidth = 10)+
  geom_vline(xintercept = 27, col = 'grey60', linetype = 'dashed')+
  geom_vline(xintercept = 194, col = 'grey60')+
  theme_minimal()+
  scale_y_continuous(limits = c(0,1600))+
  scale_x_continuous(limits = c(0,800))+
  labs(x = 'No. Infested Trees', y = 'Simulations')+
  theme(axis.line.x = element_line())

trees50

##### Plot arrangements
# by lure strength

plot_grid(trees5, trees20, trees50, ncol = 1, labels = c('A', 'B', 'C'))


ggsave("treesworstcase.png")



          