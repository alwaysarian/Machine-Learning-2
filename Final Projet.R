df <- read.csv("https://query.data.world/s/5wrkcltf0rqhyu2ahlayldrfg", fileEncoding = "UTF-8")
require(sqldf)

query1 <- sqldf('SELECT DISTINCT beer_name, beer_style  FROM df WHERE beer_name ="Gosch"')

str(query1)

