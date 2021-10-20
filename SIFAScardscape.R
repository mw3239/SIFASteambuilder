db = dbConnect(SQLite(), dbname = "masterdata.db")
db1 = dbConnect(SQLite(), dbname = "SIFAS.sqlite")

# Base Stats (at levels that could be a max level)
dbGetQuery(db,"SELECT C.school_idol_no, CP.level, CP.appeal, CP.stamina, CP.technique
              FROM m_card AS C
              LEFT JOIN m_card_parameter AS CP ON C.id = CP.card_m_id
              WHERE (card_rarity_type = 10 AND level IN (40, 46, 52, 58, 64, 70)) 
              		OR (card_rarity_type = 20 AND level IN (60, 64, 68, 72, 76, 80)) 
              		OR (card_rarity_type = 30 AND level IN (80, 82, 84, 86, 88, 90))
              ORDER BY C.school_idol_no") %>%
  mutate(increase_count = (row_number()-1) %% 6) %>%
  pivot_wider(id_cols = school_idol_no,
              values_from = c(appeal, stamina, technique),
              names_sep = "",
              names_from = (increase_count)) %>%
  dbWriteTable(db1, "cards_level_cap", ., overwrite = T)

# Training Tree Nodes
dbGetQuery(db, "SELECT C.school_idol_no, TTCP.training_content_type, TTCC.required_grade, SUM(TTCP.value) AS value
                FROM m_card AS C
                LEFT JOIN m_training_tree AS TT ON C.id = TT.id
                LEFT JOIN m_training_tree_card_param AS TTCP ON TTCP.id = TT.id
                LEFT JOIN m_training_tree_cell_content AS TTCC ON (TT.training_tree_mapping_m_id = TTCC.id AND TTCP.training_content_no = TTCC.training_content_no)
                WHERE TTCC.training_tree_cell_type = 2
                GROUP BY C.id, TTCP.training_content_type, TTCC.required_grade
                ORDER BY C.school_idol_no") %>%
  mutate(training_content_type = case_when(training_content_type == 2~"stamina",
                                           training_content_type == 3~"appeal",
                                           training_content_type == 4~"tech")) %>%
  pivot_wider(id_cols = school_idol_no,
              names_from = c(training_content_type, required_grade),
              values_from = value,
              values_fill = 0,
              names_sep = "",
              names_sort = T) %>%
  dbWriteTable(db1, "cards_tree_nodes", ., overwrite = T)

# Awakening Stats
dbGetQuery(db,"SELECT C.school_idol_no, A.parameter2 AS appeal,
                A.parameter1 AS stamina, A.parameter3 AS tech
              FROM m_card_awaken_parameter A
              LEFT JOIN m_card C ON A.card_master_id = C.id
              ORDER BY C.school_idol_no") %>%
  dbWriteTable(db1, "cards_awakening", ., overwrite = T)


invisible(map(c(db, db1), dbDisconnect))