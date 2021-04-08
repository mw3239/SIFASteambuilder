#Grabs the first 80 bond level bonuses. However, this doesn't have everything.
db1 = dbConnect(SQLite(),dbname="masterdata-jun.db")
db2 = dbConnect(SQLite(), dbname="SIFAS.sqlite")

bond <- dbGetQuery(db1,'SELECT * FROM m_love_parameter') %>%
  rename("level"="love_level","bonus"="bonus_rate") %>%
  mutate(bonus = (bonus/10000)+1)

dbWriteTable(db2,"bond",bond,overwrite=T)

dbDisconnect(db1)
dbDisconnect(db2)