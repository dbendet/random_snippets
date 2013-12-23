install.packages("RMySQL")

library(RMySQL)

mydb = dbConnect(MySQL(), 
                 user='bla', password='bla@d', dbname='bla'
                 , host='bla')

rs = dbSendQuery(dbConnect(MySQL(), 
                 user='bla', password='bla@d', dbname='bla'
                 , host='bla'), "select bla from bla;")

data = fetch(rs, n=-1)


library(RJDBC)

drv <- JDBC("com.vertica.jdbc.Driver","/path/to/vertica-jdk5-6.0.0-0.jar")

conn <- dbConnect(drv, "jdbc:vertica://vertica-prod.bla.bla.com", database = "bla", user="bla", password="bla", port="bla")

rs <- dbSendQuery(conn, statement = 
"
select bla from bla
")

data <- fetch(rs, n = -1)

df = data.frame(data)

df[is.na(df)] <- 0

df$aiv = df$gms/df$transactions

us_mailable_j = df

View(us_mailable_j)

t.test(buyers_control$gms, buyers_mailable$gms)
t.test(buyers_control$transactions, buyers_mailable$transactions)
t.test(buyers_control$aiv, buyers_mailable$aiv, nan.rm = TRUE)



df$one_six <- rowSums(df[3:8])
df$two_seven <- rowSums(df[4:9])
df$three_eight <- rowSums(df[5:10])
df$four_nine <- rowSums(df[6:11])
df$five_ten <- rowSums(df[7:12])
df$six_eleven <- rowSums(df[8:13])
df$seven_twelve <- rowSums(df[9:14])
 



