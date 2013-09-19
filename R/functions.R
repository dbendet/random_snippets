
# sync with vertica

vertica_connect <- function(user, password) {
    library(RJDBC)

    a = "com.vertica.jdbc.Driver"
    
    b = sprintf("/Users/%s/Documents/vertica-jdk5-6.0.0-0.jar", user)

    drv <- JDBC(a, b)

    conn <- dbConnect(drv, "jdbc:vertica://vertica-prod.bla.bla.com", database = "bla", user=user, password=password, port="bla")
}

# query vertica to df

vertica_query <- function(query) {
    rs <- dbSendQuery(conn, statement = query)

    data <- fetch(rs, n = -1)

    df = data.frame(data)

    View(df)
}

# plain line graph

lineGraph <- function(df, x, y, ymin, ymax, xlab, ylab, title) {
    p <- ggplot(data = df, aes_string(x=x, y=y, group=1)) + 
    geom_line(colour="black", size=.5, alpha=.3) + 
    ylim(ymin, ymax) + 
    xlab(xlab) + 
    ylab(ylab) + 
    ggtitle(title) + 
    geom_smooth(size = 0.75, se = FALSE, colour='black') + 
    # scale_x_continuous(limits=c(0,28), breaks=seq(0,28,7),"Days Until Search Ad") + 
    theme(panel.background = element_rect(fill = 'white', colour = "black"),
    axis.text.x = element_text(vjust=0.5, size=12),
    axis.text.y = element_text(vjust=0.5, size=12),
    axis.title.x = element_text(vjust=0.5, size=15),
    axis.title.y = element_text(angle=90, vjust=0.3, size=15),
    plot.title = element_text(size=18))
    p 
}


URL <- 'http://ichart.finance.yahoo.com/table.csv?s=tsla'
dat <- read.csv(URL)
dat$Date <- as.Date(dat$Date, "%Y-%m-%d")

URL <- '<set name=\"',df$timeStamp,'\" value=\"',df$Price,'\" ></set>\n'









