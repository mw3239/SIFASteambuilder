db = dbConnect(SQLite(), "masterdata.db")
db1 = dbConnect(SQLite(), "SIFAS.sqlite")

invisible(dbExecute(db, "ATTACH 'dictionary_ja_k.db' AS 'dict'"))
invisible(dbExecute(db, "ATTACH 'dictionary_en_k.db' AS 'dict_en'"))

dbGetQuery(db,"SELECT A.accessory_no AS 'accessory_num', 
                 D.message AS 'accessory_name',
                 E.message as 'accessory_name_EN',
                 A.rarity_type AS 'accessory_rarity',
                 A.attribute AS 'accessory_attribute',
                 G.grade AS 'accessory_uncaps',
                 P.level AS accessory_max_level,
                 P.appeal AS accessory_appeal,
                 P.stamina AS accessory_stam,
                 P.technique AS accessory_tech,
                 S.max_level AS 'accessory_max_sl',
                 D2.message, A.id
               FROM m_accessory A
               JOIN dict.m_dictionary D ON A.name = 'k.' || D.id
               JOIN m_accessory_grade_up G ON A.id = G.accessory_master_id
               JOIN m_accessory_parameter P ON A.id = P.accessory_master_id
               LEFT JOIN m_accessory_passive_skill S ON G.accessory_passive_skill_1_master_id = S.id
               LEFT JOIN dict.m_dictionary D2 ON D2.id = 'accessory_passive_skill_description_' || S.id || '_1'
               LEFT JOIN dict_en.m_dictionary E ON E.id = D.id
               WHERE P.level = G.max_level
               ORDER BY A.accessory_no, G.grade") %>%
  fill(accessory_max_sl) %>%
  mutate(accessory_max_sl = ifelse(accessory_max_sl %% 5 == 0,
                                   accessory_max_sl-5+accessory_uncaps,
                                   accessory_max_sl),
         accessory_name_EN = case_when(is.na(accessory_name_EN)~accessory_name,
                                       T~accessory_name_EN),
         accessory_name_EN = case_when(str_detect(accessory_name_EN, "&apos;")~str_replace(accessory_name_EN, "&apos;", "'"),
                                       T~accessory_name_EN),
         accessory_attribute = case_when(accessory_attribute == 1~"smile",
                                         accessory_attribute == 2~"pure",
                                         accessory_attribute == 3~"cool",
                                         accessory_attribute == 4~"active",
                                         accessory_attribute == 5~"natural",
                                         accessory_attribute == 6~"elegant",
                                         T~as.character(accessory_attribute)),
         accessory_name = case_when(accessory_rarity == 10~str_c(accessory_name, " (R)"),
                                    accessory_rarity == 20~str_c(accessory_name, " (SR)"),
                                    accessory_rarity == 30~str_c(accessory_name, " (UR)"),
                                    T~accessory_name),
         accessory_name_EN = case_when(accessory_rarity == 10~str_c(accessory_name_EN, " (R)"),
                                    accessory_rarity == 20~str_c(accessory_name_EN, " (SR)"),
                                    accessory_rarity == 30~str_c(accessory_name_EN, " (UR)"),
                                    T~accessory_name_EN),
         accessory_skill = case_when(str_detect(message, "SPゲージ獲得量")~"SP Gain per Tap",
                                     str_detect(message, "スタミナダメージ")~"Damage Reduction",
                                     str_detect(message, "アピールが(増加|([:digit:]|.))+\\%")~"Increase Appeal (Percentage)",  # Two possibilies here: "アピールが増加" or アピールが増加##%", but NOT アピールが増加## (without the percentage)
                                     str_detect(message, "特技発動率")~"Skill activation rate",
                                     str_detect(message, "SPゲージを獲得")~"SP gauge fill (Percentage)",
                                     str_detect(message, "クリティカル値")~"Critical Value",
                                     str_detect(message, "シールド")~"Shield",
                                     str_detect(message, "回復")~"Stamina Recovery",
                                     str_detect(message, "アピールが")~"Increase Appeal (Flat)",
                                     str_detect(message, "獲得するボルテージ")~"SP Voltage",
                                     str_detect(message, "ボルテージの上限")~"Voltage Cap",
                                     str_detect(message, "クリティカル率")~"Critical Rate",
                                     str_detect(message, "獲得ボルテージ")~"Voltage Increase (Percentage)",
                                     str_detect(message,  "SPゲージを[:digit:]+")~"SP gauge fill (Flat)",
                                     T~message),
         accessory_cond = case_when(str_detect(message, "多いほど")~"Healthy",
                                    str_detect(message, "\\(AC\\)成功時")~"AC Clear",
                                    str_detect(message, "コンボ数に応じて")~"Combo",
                                    str_detect(message, "楽曲開始時")~"Song Start",
                                    str_detect(message, "スタミナが80")~"80% Stamina",
                                    str_detect(message, "作戦変更時")~"Strategy Swap",
                                    str_detect(message, "\\(AC\\)開始時")~"AC Start",
                                    str_detect(message, "ダメージを受けた時")~"Receive Damage",
                                    str_detect(message, "楽曲中")~"Permanent",
                                    str_detect(message, "SP特技発動")~"SP Skill Activation",
                                    T~message)) %>%
  # Some accessory skills/conditions will be NA, so filling needs to occur before extracting them.
  fill(message) %>%
  fill(accessory_skill) %>%
  fill(accessory_cond) %>%
  mutate(accessory_rate = ifelse(str_detect(message, "確率:([:digit:]|.)+\\%"),
                          as.numeric(str_remove(str_remove(str_extract(message, "確率:([:digit:]|.)+\\%"), "確率:"), "\\%"))/100,
                          NA),
         accessory_dur = ifelse(str_detect(message, "[:digit:]+ノーツ"),
                                str_extract(message, "^[:digit:]+"),
                                NA),
         accessory_num = seq(1:length(accessory_name)),
         accessory_limit = str_extract(message, "[:digit:]+回だけ") %>%
           str_remove("回だけ") %>%
           as.integer()) %>%
  select(accessory_num, id, accessory_name, accessory_name_EN,
         accessory_uncaps, accessory_max_level, accessory_max_sl,
         accessory_appeal, accessory_stam, accessory_tech, accessory_attribute,
         accessory_skill, accessory_cond, accessory_rate, accessory_dur,
         accessory_limit) %>%
  dbWriteTable(db1, "accessories", ., overwrite=T)

invisible(map(c(db, db1), dbDisconnect))
