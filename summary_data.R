## Summary Stats
channel_agg <- aggregate(ALL_FLAGS$revenue,by = list(ALL_FLAGS$channel),sum)
abc <- summary(channel_agg$x)
ALL_FLAGS$channel <- as.factor(ALL_FLAGS$channel)
ALL_FLAGS$revenue <- as.numeric(ALL_FLAGS$revenue)
## Advanced Plot ##
library(ggplot2)
channel_agg <- subset(channel_agg,channel_agg$x > abc[4])
p <- ggplot(channel_agg,aes(x = channel_agg$Group.1, y = channel_agg$x, fill = channel_agg$Group.1)) + geom_bar(stat = "Identity") + theme_minimal() 
p + labs(title = "Revenue Split By Channel") + xlab("Channel") + ylab("Amount of Revenue")

### Add percentage splits revenue - high/low value same graph -stacked chart
hist(ALL_FLAGS$revenue)
summary(ALL_FLAGS$revenue)
median(ALL_FLAGS$revenue)
#plot(ave(ALL_FLAGS$revenue)) ## maybe a dateplot
avgtchs <- mean(ALL_FLAGS$rank) %>% round()
avgtchs
mediantchs <- median(ALL_FLAGS$rank)
mediantchs
maxtchs <- max(ALL_FLAGS$rank)
