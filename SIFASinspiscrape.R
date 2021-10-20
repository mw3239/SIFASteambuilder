db = dbConnect(SQLite(), "masterdata.db")
db1 = dbConnect(SQLite(), "SIFAS.sqlite")

invisible(dbExecute(db, "ATTACH 'dictionary_ja_k.db' AS 'dict'"))
invisible(dbExecute(db, "ATTACH 'dictionary_en_k.db' AS 'dict_en'"))

dbGetQuery(db, "WITH skill_names AS (
              	SELECT S.id, D.message AS 'name'
              	FROM dict.m_dictionary AS D
              	JOIN m_passive_skill AS S ON S.name == 'k.' || D.id
              	WHERE S.name = 'k.replacement_skill_name_' || S.id),
              	
              	skill_descs AS (
              	SELECT S.id, D.message AS 'description'
              	FROM dict.m_dictionary AS D
              	JOIN m_passive_skill AS S ON S.description = 'k.' || D.id
              	WHERE S.name = 'k.replacement_skill_name_' || S.id),
              	
              	en_names AS (
              	SELECT S.id, D.message AS 'name_en'
              	FROM dict_en.m_dictionary AS D
              	JOIN m_passive_skill AS S ON S.name == 'k.' || D.id
              	WHERE S.name = 'k.replacement_skill_name_' || S.id)
              	
              SELECT name, name_en, description
              FROM skill_names
              LEFT JOIN skill_descs ON skill_names.id = skill_descs.id
              LEFT JOIN en_names ON skill_names.id = en_names.id
              ") %>%
  mutate(inspi_effect = case_when(str_detect(description, "アピールが")~"Appeal+",
                                str_detect(description, "クリティカル値")~"Critical value",
                                str_detect(description, "特技発動率")~"Skill activation rate",
                                str_detect(description, "タイプ効果")~"Role bonus",
                                str_detect(description, "SP特技")~"SP Skill Up",
                                str_detect(description, "ボルテージを獲得")~"Voltage Up",
                                str_detect(description, "スタミナを回復")~"Stamina recovery",
                                str_detect(description, "シールドを獲得")~"Shield",
                                str_detect(description, "コンボ数")~"Combo Count Up",
                                str_detect(description, "ボルテージが")~"Voltage Up",
                                str_detect(description, "クリティカル率")~"Critical rate",
                                str_detect(description, "スタミナダメージ")~"Damage reduction",
                                str_detect(description, "SPゲージを[:digit:]+獲得")~"SP gauge gain (flat)",
                                str_detect(description, "SPゲージ獲得量が")~"SP gauge gain (percentage)",
                                T~""),
         inspi_val_text = str_extract(description, "(^SPゲージを[:digit:]+\\.?[:digit:]*|[:digit:]+\\.?[:digit:]*%?(シールド|増加|スタミナ|上昇|ボルテージ|軽減)|×[:digit:]+\\.?[:digit:]*%?)"),
         is_percentage = case_when(str_detect(inspi_val_text, "%")~T,
                                                    T~F),
         inspi_val = case_when(is_percentage~as.numeric(str_extract(inspi_val_text, "[:digit:]+\\.?[:digit:]*"))/100,
                                         T~as.numeric(str_extract(inspi_val_text, "[:digit:]+\\.?[:digit:]*"))),
         inspi_dur = case_when(str_detect(description, "ノーツ")~str_replace(str_extract(description, "[:digit:]+ノーツ"), "ノーツ", " notes"),
                             str_detect(description, "終了まで")~"Remainder",
                             str_detect(description, "基本")~"Permanent",
                             T~""),
         inspi_target = case_when(str_detect(description, "自身以外")~"allies",
                                  str_detect(description, "同タイプ")~"role",
                                  str_detect(description, "同学校")~"school",
                                  str_detect(description, "同学年")~"year",
                                  str_detect(description, "全員")~"all",
                                  str_detect(description, "同作戦")~"strategy",
                                  str_detect(description, "同属性")~"attribute",
                                  str_detect(description, "自身")~"self",
                                  T~""),
         inspi_cond = case_when(str_detect(description, "楽曲開始")~"Song Start",
                                str_detect(description, "30％達成")~"30% Target",
                                str_detect(description, "成功時")~"AC Clear",
                                str_detect(description, "80％以下")~"80% Stam",
                                str_detect(description, "AC\\)開始時")~"AC Start",
                                T~"Permanent"),
         inspi_chance = ifelse(str_detect(description, "確率:"),
                               suppressWarnings(as.numeric(str_remove(str_extract(description, "確率:[0-9]*"),"確率:")))/100,
                               1),
         inspi_limit = str_extract(description, "[:digit:]+回だけ") %>%
           str_remove("回だけ") %>%
           as.numeric()) %>%
  arrange(name) %>%
  mutate(inspi_num = seq(1:length(name))) %>%
  select(inspi_num, name, name_en, inspi_effect, inspi_val, inspi_dur, inspi_target, 
         inspi_cond, inspi_chance, inspi_limit) %>%
  dbWriteTable(db1, "inspi_skills", ., overwrite=T)

invisible(map(c(db,db1),dbDisconnect))
