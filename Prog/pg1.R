require(plyr)
require(tidyr)
require(tidyverse)


DIR.data <- "./Data"
FN.data  <- "data1.csv"
data1 <- read.csv(sprintf("%s/%s",DIR.data,FN.data)) %>%
  mutate(week=as.factor(week))

levels(data1$class) <- c("F0","F1","F2","F3","F4")
levels(data1$base) <- c("F0","F1","F2","F3","F4")


plot1 <- ggplot(data1,aes(x=week,y=class,group=id))
plot1_geom <- geom_line(position=position_dodge(width=0.2))
plot1_facet <- facet_wrap(~base,nrow=2)
plot(plot1+plot1_geom +plot1_facet)