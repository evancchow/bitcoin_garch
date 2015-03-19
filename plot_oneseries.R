# Script to plot one series really fancily!

require("ggplot2")

temp.data <- df.1coin
temp.data$date <- as.Date(temp.data$datehr)
temp.data$norm <- (temp.data$price / 100)
plt.1coin.special <- ggplot(data=temp.data, aes(x=1:nrow(temp.data), 
                                                y=price,
                                                color=price)) +
  geom_point() +
  geom_line() +
#   geom_smooth(color="red") +
  xlab("") +
  ylab("") +
  scale_x_continuous(labels=temp.data$date[seq(1,nrow(temp.data),100)],
                     breaks=seq(1,nrow(temp.data),100)) +
  theme(axis.text.x=element_text(angle=45,size=12,hjust=1),
        axis.text.y=element_text(size=12)) +
  scale_colour_gradient(low="red")

show(plt.1coin.special)