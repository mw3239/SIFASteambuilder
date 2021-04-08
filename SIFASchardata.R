#Not updated for shioriko, although this should still work.

db = dbConnect(SQLite(), "masterdata-jun.db")
db1 = dbConnect(SQLite(), "SIFAS.sqlite")

invisible(dbExecute(db,"ATTACH 'dictionary_ja_k-jun.db' AS 'dict'"))
dbGetQuery(db,"SELECT M.id, A.message AS 'idol', M.school_grade AS 'year', B.message AS 'school', C.message AS 'subunit' 
           FROM m_member M
           JOIN m_member_unit_detail D ON M.id = D.member_m_id
           JOIN m_member_unit U ON D.member_unit = U.member_unit
           JOIN m_member_group G ON M.member_group = G.member_group
           JOIN dict.m_dictionary A ON M.name_romaji = 'k.' || A.id
           JOIN dict.m_dictionary B ON G.group_name  = 'k.' || B.id
           JOIN dict.m_dictionary C ON U.unit_name = 'k.' || C.id
           ORDER BY M.id") %>%
  mutate(id = rank(id)) %>%
  mutate(idol = str_remove(idol,"[:blank:][:graph:]+")) %>%
  mutate(idol = tolower(idol)) %>%
  mutate(school = case_when(str_detect(school,"&apos;")~str_replace(school,"&apos;","'"),
                            str_detect(school,"虹")~"Nijigasaki",
                            T~school)) %>%
  select(id,idol,year,school,subunit) %>%
  dbWriteTable(db1,"idol_data",.,overwrite=T)

invisible(map(c(db,db1),dbDisconnect))
