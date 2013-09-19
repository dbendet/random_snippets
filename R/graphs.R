
# http://docs.ggplot2.org/current/geom_bar.html



library(ggplot2)
library(scales)

budget_ut$date <- as.Date(as.character(budget_ut$date))

p <- ggplot(data = budget_ut, aes(x=date, y=budget_util, group=1))

p + geom_line(size=.5, fill='black', col='black', alpha=0.3) + 
xlab("Date") + 
ylab("Budget Utilization (%)") + 
ggtitle("Search Ads Budget Utilization") + 
geom_smooth(size = .65, se = FALSE, colour='black') + 
scale_x_date(breaks=date_breaks('2 months'), labels = date_format('')) + 
scale_y_continuous("Budget Utilization (%)") + 
theme(panel.background = element_rect(fill = 'white', colour = "black"),
axis.title.x = element_text(colour="black", size=13),
axis.text.x  = element_text(angle=0, vjust=0.5, size=11),
axis.text.y = element_text(angle=0, vjust=0.5, size=11),
axis.title.y = element_text(colour="black", size=13)) 
    

buys_graph$buy_date <- as.Date(as.character(buys_graph$buy_date))

p <- ggplot(data = buys_graph, aes(x=buy_date, y=buys_from_favs, group=1))

p + geom_line(size=.5, fill='black', col='black', alpha=0.3) + 
xlab("Date") + 
ylab("GMS") + 
ggtitle("GMS From Favorites") + 
geom_smooth(size = .65, se = FALSE, colour='black') + 
scale_x_date(breaks=date_breaks('3 months'), labels = date_format('%m-%Y')) + 
scale_y_continuous("GMS ($ thousands)") + 
theme(panel.background = element_rect(fill = 'white', colour = "black"),
axis.title.x = element_text(colour="black", size=13),
axis.text.x  = element_text(angle=0, vjust=0.5, size=11),
axis.text.y = element_text(angle=0, vjust=0.5, size=11),
axis.title.y = element_text(colour="black", size=13)) 



# histogram
b <- ggplot(fav_hist, aes(x=datediff))

b + 
geom_bar(binwidth = 1, fill='black', col='grey') + 
scale_x_continuous(limits=c(0,721),breaks=seq(0,721,50),"Days") + 
ggtitle('Days From Favorite Til Buy') +
ylab('Favoriters') +
ylim(0, 1250000) + 
theme(panel.background = element_rect(fill = 'white', colour = "black"),
axis.text.x = element_text(vjust=0.5, size=12),
axis.text.y = element_text(vjust=0.5, size=12),
axis.title.x = element_text(vjust=0.5, size=15),
axis.title.y = element_text(angle=90, vjust=0.3, size=15),
plot.title = element_text(size=18))



b + 
geom_bar(binwidth = 1, fill='black', col='grey') + 
scale_x_continuous(limits=c(200,600),breaks=seq(200,600,50),"Days") + 
ggtitle('Days From Favorite Til Buy') +
ylab('Favoriters') +
ylim(0, 1000) + 
theme(panel.background = element_rect(fill = 'white', colour = "black"),
axis.text.x = element_text(vjust=0.5, size=12),
axis.text.y = element_text(vjust=0.5, size=12),
axis.title.x = element_text(vjust=0.5, size=15),
axis.title.y = element_text(angle=90, vjust=0.3, size=15),
plot.title = element_text(size=18))



s <- ggplot(aiv)

s + geom_point(aes(buy_aiv, fav_aiv, group = favorites, colour = favorites, size=favorites)) +
xlim(0,50) + 
ylim(0,150)

# line graph with MA

sa_daily$date <- as.Date(as.character(sa_daily$date))
sa_daily$variable <- as.factor(sa_daily$variable)
sa_daily$value <- as.numeric(sa_daily$value)

sa <- ggplot(sa_daily, aes(x = date, y = value, group = variable, colour = variable)) + 
    scale_colour_brewer(palette="Set1") + 
    geom_line(aes(color = variable), size = 0.5, alpha = 0.3) + 
    geom_smooth(size = 1, se = FALSE) + 
    scale_y_continuous("",labels = dollar) + 
    scale_x_date("") + 
    ggtitle("Search Ads Budget Utilization") + 
    theme_bw() +
    # NO LEGEND
    theme(legend.title=element_blank())

# line graph with multiple lines
mobile_growth$date <- as.Date(as.character(mobile_growth$date))
mobile_growth$variable <- as.factor(mobile_growth$variable)
mobile_growth$value <- as.numeric(mobile_growth$value)

m2 <- ggplot(mobile_growth, aes(x = date, y = value, group = variable, colour = variable)) + 
scale_colour_brewer(palette="Set1") + 
geom_line(aes(color = variable), size = 0.5, alpha = 0.3) + 
geom_smooth(size = 1, se = FALSE) 

m2 <- m2 + 
scale_y_continuous("Y/Y growth rates", limits=c(0, 6), 
breaks=seq(0, 6, .5), 
labels = percent) + 
scale_x_date("") + 
ggtitle("Visit Growth by Platform") + 
theme_bw() 






qplot(date, pop, data=economics, geom="line")


qplot(date, pop, data=subset(economics, date > as.Date("2006-1-1")), geom="line")








sa <- ggplot(ads_listings, aes(x = listings, y = spent_per_budget, group = 1)) + 
    scale_colour_brewer(palette="Set1") + 
    geom_line(aes(size = 0.5, alpha = 0.3)) + 
    geom_smooth(size = 1, se = FALSE) + 
    ggtitle("Search Ads Budget Utilization") + 
    theme_bw() +
    # NO LEGEND
    theme(legend.title=element_blank())




users_that_visit$date <- as.Date(as.character(users_that_visit$date))

p <- ggplot(data = users_that_visit, aes(x=date, y=forum_visits, group=1))

p + geom_line(size=.5, fill='black', col='black', alpha=0.3) + 
xlab("") + 
ggtitle("Users That Visit Per Day") + 
geom_smooth(size = .65, se = FALSE, colour='black', span=.2) + 
scale_x_date(breaks=date_breaks('6 months')) + 
scale_y_continuous("Users") + 
theme(panel.background = element_rect(fill = 'white', colour = "black"),
axis.title.x = element_text(colour="black", size=13),
axis.text.x  = element_text(angle=0, vjust=0.5, size=11),
axis.text.y = element_text(angle=0, vjust=0.5, size=11),
axis.title.y = element_text(colour="black", size=13)) 





x$date <- as.Date(as.character(x$date))

p <- ggplot(data = ma, aes(x=date, y=ma.admin.post.percent, group=1))

p + geom_line(size=.5, fill='black', col='black', alpha=0.3) + 
ylab("Percent") + 
xlab("") + 
ggtitle("Admin Post Percent By Day (7 Day Moving Avg)") + 
geom_smooth(size = .65, se = FALSE, colour='black', span=.3) + 
# scale_x_date(breaks=date_breaks('3 months'), labels = date_format('%m-%Y')) + 
# scale_y_continuous("Favorites (millions)") + 
theme(panel.background = element_rect(fill = 'white', colour = "black"),
axis.title.x = element_text(colour="black", size=13),
# axis.text.x  = element_text(angle=0, vjust=0.5, size=11),
axis.text.y = element_text(angle=0, vjust=0.5, size=11),
axis.title.y = element_text(colour="black", size=13)) 


thing=reorder(fav_quartiles$n, fav_quartiles$pct)

ggplot(data=fav_quartiles, aes(x=n, y=pct, fill=ntile)) +
    geom_bar(fill="#D15600", stat="identity") +
    guides(fill=FALSE) +
    ylim(0, 100) + 
    ylab("Favorites (%)") +
    xlab("Quartile (Based on Favorites per Day)") +
    ggtitle("Percent of Favorites By Quartile") +
    theme(panel.background = element_rect(fill = 'white', colour = "black"),
    axis.title.x = element_text(colour="black", vjust=0.001, size=13),
    axis.text.x  = element_text(angle=0, vjust=0.5, size=11),
    axis.text.y = element_text(angle=0, vjust=0.5, size=11),
    axis.title.y = element_text(colour="black", size=13)) 
    # geom_text(aes(label = pct, y = pct), size = 3)  

factor(Position, 
            levels=names(sort(table(Position), 
            decreasing=TRUE))))


ggplot(data=time, aes(x=diff, y=count)) +
    geom_bar(fill="#D15600", stat="identity") +
    guides(fill=FALSE) +
    # ylim(0, 100) + 
    ylab("Favorites (%)") +
    xlab("Quartile") +
    ggtitle("Percent of Favorites By Quartile") +
    theme(panel.background = element_rect(fill = 'white', colour = "black"),
    axis.title.x = element_text(colour="black", size=13),
    axis.text.x  = element_text(angle=0, vjust=0.5, size=11),
    axis.text.y = element_text(angle=0, vjust=0.5, size=11),
    axis.title.y = element_text(colour="black", size=13)) 



sl2 <- ggplot(sl2_daily, aes(x = date, y = value, group = variable)) + 
    scale_colour_brewer(palette="Set1") + 
    geom_line(colour="steelblue", size = 0.5, alpha = 0.3) + 
    geom_smooth(colour="steelblue", size = 1, se = FALSE) + 
    geom_vline(xintercept = as.numeric(as.Date("2012-12-15")), linetype=2, alpha=.5) + 
    annotate("text", x=as.Date("2012-12-15"),y = .011, label = "Intl Priority", size =4) + 
    annotate("text", x=as.Date("2012-12-15"),y = .009, label = "& Express Change", size =4) + 
    geom_vline(xintercept = as.numeric(as.Date("2013-02-01")), linetype=2, alpha=.5) + 
    annotate("text", x=as.Date("2013-02-02"),y = .011, label = "Intl First", size =4) + 
    annotate("text", x=as.Date("2013-02-02"),y = .009, label = "Class Change", size =4) + 
    scale_y_continuous("", labels = percent) + 
    scale_x_date("") + 
    ggtitle("Shipping Labels Margin") + 
    theme_bw() +
    theme(legend.position = "none")


    





ggplot(data=seller_rankings, aes(x=ntile, y=gmss, fill=ntile)) +
    geom_bar(fill="#D15600", stat="identity") +
    ylab("GMS (Millions)") +
    xlab("NTile") +
    ggtitle("Seller GMS Past 12 Months") +
    theme(panel.background = element_rect(fill = 'white', colour = "black"),
    axis.title.x = element_text(colour="black", vjust=0.001, size=13),
    axis.text.x  = element_text(angle=0, vjust=0.5, size=11),
    axis.text.y = element_text(angle=0, vjust=0.5, size=11),
    axis.title.y = element_text(colour="black", size=13)) 


