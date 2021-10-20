# Extracts the scaling skill value from it's description,
# converts it to a number, and (if necessary,) converts it from a
# percentage to decimal.
get_skill_val <- function(text){
  # There are many ways that these values can appear. Simply extracting a number is insufficient
  # because numbers can also appear elsewhere in the description as conditions (ie. 10 Notes) or
  # activation rates (30% chance of activating)
  str_extract(text,"([0-9]|\\.)+\\%?(\\)|増加|SP|上昇|シールド|スタミナ|軽減)") %>%
    str_extract("([0-9]|\\.|\\%)+") %>%
    {
      suppressWarnings(ifelse(str_detect(., "\\%"),
                             as.numeric(str_extract(., "([0-9]|\\.)+"))/100,
                             as.numeric(.)))
    } %>%
    return()
}


db <- dbConnect(SQLite(), "masterdata.db")
db1 <- dbConnect(SQLite(), "SIFAS.sqlite")

invisible(dbExecute(db, "ATTACH 'dictionary_ja_k.db' AS 'dict'"))
invisible(dbExecute(db, "ATTACH 'SIFAS.sqlite' AS 'SIFAS'"))

#To do: Rewrite with CTE.
dbGetQuery(db, "SELECT DISTINCT S.accessory_num,
                G.accessory_master_id AS 'accessory_id',
                S.accessory_name, S.accessory_name_EN, L.description, D.message
           FROM m_accessory_passive_skill A
           JOIN m_accessory_passive_skill_level L ON A.id = L.accessory_passive_skill_master_id
           JOIN m_accessory_grade_up G on A.id = G.accessory_passive_skill_1_master_id
           JOIN dict.m_dictionary as D ON L.description = 'k.' || D.id
           JOIN SIFAS.accessories S ON G.accessory_master_id = S.id") %>%
  mutate(skill_level = as.numeric(str_extract(description, "[:digit:]+$"))) %>%
  arrange(accessory_num, accessory_id, skill_level) %>%
  mutate(skill_val = get_skill_val(message)) %>%
  select(accessory_num, accessory_id, accessory_name, accessory_name_EN,
         skill_level, skill_val) %>%
  dbWriteTable(db1, "accessories_skill_vals", ., overwrite=T)

invisible(map(c(db, db1), dbDisconnect))
