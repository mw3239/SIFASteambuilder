dict = dbConnect(SQLite(), dbname="dictionary_ja_k.db")


# There are some strange errors and missing values in the game's
# internal database (that I'm amazed don't mess anything up.)

# #469 Just Believe Ai is missing the description of the 5th level of her
# passive in the dictionary database. This manually adds it in (assuming it
# does not get fixed.) How does this not break in-game!?
# It seems this points to v.passive_skill_description rather than
# k.. If I had that database then this wouldn't be needed.
# The v database is supposed to be used for GPS present
# messages, and sure enough, this skill description is the only
# other entry lol.
tryCatch(
  {
    dbExecute(dict, "INSERT INTO m_dictionary
            VALUES('passive_skill_description_20086305', '基本スタミナが3.2%増加
            対象:同属性')") %>%
      dbWriteTable(dict, "m_dictionary", ., append = TRUE)
  },
  error = function(e) invisible(),
  finally = dbDisconnect(dict))
