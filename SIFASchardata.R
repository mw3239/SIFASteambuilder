db = dbConnect(SQLite(), "masterdata.db")
db1 = dbConnect(SQLite(), "SIFAS.sqlite")

invisible(dbExecute(db,"ATTACH 'dictionary_ja_k.db' AS 'dict'"))

# Joining 4 times on dict is extremely inefficient. This should be rewritten.
dbGetQuery(db,"SELECT M.id, A1.message AS 'name', A2.message AS 'name_en',
                M.school_grade AS 'year', A3.message AS 'school',
                A4.message AS 'subunit' 
              FROM m_member M
              LEFT JOIN m_member_unit_detail D ON M.id = D.member_m_id
              LEFT JOIN m_member_unit U ON D.member_unit = U.member_unit
              LEFT JOIN m_member_group G ON M.member_group = G.member_group
              LEFT JOIN dict.m_dictionary A1 ON M.name = 'k.' || A1.id
              LEFT JOIN dict.m_dictionary A2 ON M.name_romaji = 'k.' || A2.id
              LEFT JOIN dict.m_dictionary A3 ON G.group_name  = 'k.' || A3.id
              LEFT JOIN dict.m_dictionary A4 ON U.unit_name = 'k.' || A4.id
              ORDER BY M.id") %>%
  mutate(id = rank(id) %>% as.integer(),
         name_en = str_remove(name_en, "[:blank:][:graph:]+") %>% tolower(),
         school = case_when(str_detect(school, "&apos;")~str_replace(school, "&apos;", "'"),
                            str_detect(school, "虹")~"Nijigasaki",
                            T~school)) %>%
  select(id, name, name_en, year, school, subunit) %>%
  dbWriteTable(db1, "member_data", ., overwrite=T)

invisible(map(c(db,db1), dbDisconnect))
