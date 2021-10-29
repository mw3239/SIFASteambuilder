db = dbConnect(SQLite(), dbname="masterdata.db")
db1 = dbConnect(SQLite(), dbname="SIFAS.sqlite")

invisible(dbExecute(db, "ATTACH 'dictionary_ja_k.db' AS 'dict'"))
invisible(dbExecute(db, "ATTACH 'dictionary_en_k.db' AS 'dict_en'"))

# Below are several helper functions used by the scrape_card_actives function.


# Take in a list of active skill messages and a string indicating their skill level.
# Returns an n x 2 tibble with the active skill messages split between the
# columns (in the event that the skill has multiple effects.)
# If a card does not have a dual effect on its active skill, the entirety
# of the message will be copied into column 1 and column 2 will be NA.
# I hope to deprecate this soon and replace its usage with dplyr's 'separate' function.
dual_active_split <- function(active_message, sl) {
  strsplit(active_message, split = "(?<=(獲得|増加|回復|上昇|軽減)し)", perl=T) %>%
    lapply(`length<-`, 2) %>%
    invoke(rbind, .) %>%
    `colnames<-`(c(str_c("active1_sl", sl), str_c("active2_sl", sl))) %>%
    as_tibble() %>%
    return()
}

# Returns the effect of a given active skill given its text.
active_get_effect <- function(message) {
  case_when(str_detect(message, "ボルテージを[:digit:]+獲得")~"Voltage Boost (flat)",
            str_detect(message, "%ボルテージを獲得")~"Voltage Boost (percent)",
            str_detect(message, "SPゲージ獲得量")~"SP Gauge Gain",
            str_detect(message, "次に発動するSP特技で獲得するボルテージが自身の(アピール|テクニック|スタミナ)の([:digit:]|\\.)+%増加")~"SP Voltage Boost (%)",
            str_detect(message, "次に発動するSP特技で獲得するボルテージが自身の(アピール|テクニック|スタミナ)の([:digit:]|\\.)+増加")~"SP Voltage Boost (flat)",
            str_detect(message, "スタミナダメージ")~"Damage Reduction",
            str_detect(message, "SPゲージを[:digit:]+獲得")~"SP Gauge (flat)",
            str_detect(message, "%SPゲージ")~"SP Gauge (percent)",
            str_detect(message, "クリティカル率")~"Crit rate",
            str_detect(message, "クリティカル値")~"Crit value",
            str_detect(message, "%スタミナを回復")~"Stamina Recovery (percent)",
            str_detect(message, "[:digit:]+回復")~"Stamina Recovery (flat)",
            str_detect(message, "シールドを[:digit:]+獲得")~"Shield (flat)",
            str_detect(message, "%シールドを獲得")~"Shield (percent)",
            str_detect(message, "ボルテージが([:digit:]|\\.)+%増加")~"Voltage Gain (percent)",
            str_detect(message, "ボルテージが[:digit:]+増加")~"Voltage Gain (flat)",
            str_detect(message, "特技発動率")~"Skill Activation Rate",
            str_detect(message, "アピールが[:digit:]+増加")~"Appeal Boost (flat)",
            str_detect(message, "アピールが([:digit:]|\\.)+%増加")~"Appeal Boost (percent)",
            T~message) %>%
    return()
}

# Returns which cards are affected by the active skill.
active_get_target <- function(message) {
  case_when(str_detect(message, "全員")~"all",
            str_detect(message, "同作戦")~"same strategy",
            str_detect(message, "同タイプ")~"same type",
            str_detect(message, "同属性")~"same element",
            str_detect(message, "同学年")~"same year",
            str_detect(message, "同学校")~"same school",
            str_detect(message, "(仲間|自身以外)")~"allies") %>%
    return()
}

# Returns how many notes the active skill lasts for.
active_get_dur <- function(message) {
  case_when(str_detect(message, "ノーツの間") ~
              as.integer(str_remove(str_extract(message, "[:digit:]+ノーツの間"), "ノーツの間"))) %>%
    return()
}

# Determines which of the cards' stats is used for the active skill's value.
active_get_param <- function(message) {
  case_when(str_detect(message, "自身のアピール")~"appeal",
            str_detect(message, "自身のスタミナ")~"stamina",
            str_detect(message, "自身のテクニック")~"tech") %>%
    return()
}

# Extracts the value of an active skill at a given skill level.
active_get_val <- function(message) {
  case_when(str_detect(message, "[:digit:]+(獲得|回復)")~as.double(str_extract(message, "[:digit:]+")),
            str_detect(message, "[:digit:]+増加")~as.double(str_remove(str_extract(message, "[:digit:]+増加"),"増加")),
            str_detect(message, "([:digit:]|\\.)+%(上昇|増加|軽減|ボルテージ|スタミナ|シールド|SP)")~as.double(str_remove(str_extract(message, "([:digit:]|\\.)+%"), "%"))) %>%
    return()
}

#########################################################################

# Passive Skill Helpers

passive_get_target <- function(message) {
  case_when(str_detect(message, "全員")~"all",
            str_detect(message, "同作戦")~"same strategy",
            str_detect(message, "同タイプ")~"same type",
            str_detect(message, "同ユニット")~"same subunit",
            str_detect(message, "同属性")~"same element",
            str_detect(message, "同学年")~"same year",
            str_detect(message, "同学校")~"same school",
            str_detect(message, "(仲間|自身以外)")~"allies",
            str_detect(message, "自身")~"self",
            T~"same character") %>%
    return()
}


passive_get_stat <- function(message) {
  case_when(is.na(message)~"",
            str_detect(message, "アピール")~"appeal",
            str_detect(message, "スタミナ")~"stamina",
            str_detect(message, "テクニック")~"technique") %>%
    return()
}

#########################################################################

# Live Skill Helpers

# A list of character names is required for the few live skills that use them.
sifas <- dbConnect(SQLite(), dbname="SIFAS.sqlite")
char_names <- dbGetQuery(sifas, "SELECT name
           FROM member_data") %>%
  unlist()

live_get_effect <- function(message){
  case_when(str_detect(message, "ボルテージを[:digit:]+獲得")~"Voltage Boost (flat)",
            str_detect(message, "%ボルテージを獲得")~"Voltage Boost (percent)",
            str_detect(message, "SPゲージ獲得量")~"SP Gauge Gain",
            str_detect(message, "次に発動するSP特技で獲得するボルテージが自身の(アピール|テクニック|スタミナ)の([:digit:]|\\.)+%増加")~"SP Voltage Boost (%)",
            str_detect(message, "次に発動するSP特技で獲得するボルテージが自身の(アピール|テクニック|スタミナ)の([:digit:]|\\.)+増加")~"SP Voltage Boost (flat)",
            str_detect(message, "スタミナダメージ")~"Damage Reduction",
            str_detect(message, "SPゲージを[:digit:]+獲得")~"SP Gauge (flat)",
            str_detect(message, "%SPゲージ")~"SP Gauge (percent)",
            str_detect(message, "クリティカル率")~"Crit rate",
            str_detect(message, "クリティカル値")~"Crit value",
            str_detect(message, "%スタミナを回復")~"Stamina Recovery (percent)",
            str_detect(message, "[:digit:]+回復")~"Stamina Recovery (flat)",
            str_detect(message, "シールドを[:digit:]+獲得")~"Shield (flat)",
            str_detect(message, "%シールドを獲得")~"Shield (percent)",
            str_detect(message, "ボルテージが([:digit:]|\\.)+%増加")~"Voltage Gain (percent)",
            str_detect(message, "ボルテージが[:digit:]+増加")~"Voltage Gain (flat)",
            str_detect(message, "特技発動率")~"Skill Activation Rate",
            str_detect(message, "アピールが[:digit:]+増加")~"Appeal Boost (flat)",
            str_detect(message, "アピールが([:digit:]|\\.)+%増加")~"Appeal Boost (percent)",
            str_detect(message, "0になる")~"Revive",
            str_detect(message, "解除")~"Cleanse",
            str_detect(message, "クリティカル時のみ獲得ボルテージの上限")~"Critical Voltage Cap Increase",
            str_detect(message, "上限")~"Voltage Cap Increase",
            str_detect(message, "含まれるボルテージタイプの数×")~"Appeal Boost (vo count)",
            str_detect(message, "含まれるガードタイプの数×300スタミナを回復")~"Stamina Recovery (gd count)",
            str_detect(message, "作戦切替ボーナスによるスタミナ回復量")~"Stamina Recovery Boost (swap)",
            str_detect(message, "スタミナが多いほど基本アピールが増加")~"Appeal Boost (percentage health)",
            str_detect(message, "オーバーチャージ")~"SP Overcharge",
            str_detect(message, "SP特技ゲージの消費量に応じて")~"SP Voltage Boost (SP Spent)",
            T~message) %>%
    return()
}

live_get_target <- function(message, names = char_names) {
  case_when(str_detect(message, "全員")~"all",
            str_detect(message, "同作戦")~"same strategy",
            str_detect(message, "同タイプ")~"same type",
            str_detect(message, "同属性")~"same element",
            str_detect(message, "同学年")~"same year",
            str_detect(message, "同学校")~"same school",
            str_detect(message, "(仲間|自身以外)")~"allies",
            str_detect(message, "対象:自身")~"self",
            str_detect(message, names) %>%
              any() %>%
              replace_na(FALSE)~"same character") %>%
    return()
}

# Damage-based conditions are treated as unique skills for 2 reasons.
# 1) A separate column would be needed to store their values and
# 2) Treating it as categorical would make life easier during model training.
live_get_condition <- function(message) {
  case_when(str_detect(message, "条件:アピールチャンス\\(AC\\)成功時")~"AC Clear",
            str_detect(message, "条件:アピールチャンス\\(AC\\)開始時")~"AC Start",
            str_detect(message, "条件:アピールチャンス\\(AC\\)失敗時")~"AC Fail",
            str_detect(message, "条件:作戦変更時")~"Swap",
            str_detect(message, "条件:楽曲開始時")~"Song Start",
            str_detect(message, "条件:楽曲と属性一致時")~"Song Element",
            str_detect(message, "条件:SP特技発動時")~"SP Skill",
            str_detect(message, "条件:スタミナが50%以下")~"50% Stamina",
            str_detect(message, "条件:スタミナが80％以下")~"80% Stamina",
            str_detect(message, "条件:自身のクリティカル時")~"Own Crit",
            str_detect(message, "条件:自身のアピール時")~"Own Appeal",
            str_detect(message, "条件:200ダメージ以上受けた時")~"200 Damage",
            str_detect(message, "条件:350ダメージ以上受けた時")~"350 Damage",
            str_detect(message, "条件:500ダメージ以上受けた時")~"500 Damage",
            str_detect(message, "条件:1,000ダメージ以上受けた時")~"1000 Damage",
            str_detect(message, "条件:2,000ダメージ以上受けた時")~"2000 Damage",
            T~message) %>%
    return()
}

live_get_chance <- function(message) {
  str_extract(message, "確率:([0-9]|\\.|\\%)+") %>%
    str_remove_all("[^([0-9]|\\.)+]") %>%
    as.numeric() %>%
    divide_by(100) %>%
    return()
}

live_get_dur <- function(message) {
  case_when(str_detect(message, "ノーツの間")~
              str_extract(message, "[:digit:]+ノーツの間") %>%
              str_remove("ノーツの間"),
            str_detect(message, "終了まで")~"Remainder") %>%
    return()
}

live_get_val <- function(message) {
  case_when(str_detect(message, "[:digit:]+(獲得|回復)")~
              str_extract(message, "[:digit:]+") %>%
              as.double(),
            str_detect(message, "[:digit:]+増加")~
              str_extract(message, "[:digit:]+増加") %>%
              str_remove("増加") %>%
              as.double(),
            str_detect(message, "([:digit:]|\\.)+%(上昇|増加|軽減|ボルテージ|スタミナ|シールド|SP|回復|まで)")~
              str_extract(message, "([:digit:]|\\.)+%") %>%
              str_remove("%") %>%
              as.double()) %>%
    return()
}

live_get_limit <- function(message) {
  str_extract(message, "[0-9]+回だけ") %>%
    str_remove("回だけ") %>%
    as.double() %>%
    return()
}


# Basic Info (awakened name, character, attribute, type, and other info used
# to determine what kind of UR it is.)
dbGetQuery(db, "WITH names AS(
              	SELECT c.school_idol_no, a.card_name AS 'name', c.card_rarity_type,
                          c.card_attribute, c.role, c.max_passive_skill_slot
              	FROM m_card_appearance AS a
              	LEFT JOIN m_card AS c ON c.id = a.card_m_id
              	WHERE a.appearance_type = 2),
              	
              	names_jp AS(
              	SELECT n.school_idol_no, d.message AS 'card_name', n.card_rarity_type,
                          n.card_attribute, n.role, n.max_passive_skill_slot
              	FROM dict.m_dictionary AS d
              	JOIN names AS n ON n.name = 'k.' || d.id),
              	
              	names_en AS(
              	SELECT n.school_idol_no, d.message AS 'card_name_en'
              	FROM dict_en.m_dictionary AS d
              	JOIN names AS n ON n.name = 'k.' || d.id),
              	
              	members AS(
              	SELECT c.school_idol_no, lower(substr(d.message, 1, instr(d.message, ' ') - 1)) AS 'member'
              	FROM dict.m_dictionary AS d
              	JOIN m_member AS m ON m.name_romaji = 'k.' || d.id
              	JOIN m_card AS c ON c.member_m_id = m.id),
              	
              	passive_skill AS(
              	SELECT c.school_idol_no, t.training_tree_card_passive_skill_increase_m_id
              	FROM m_card AS c
              	LEFT JOIN m_training_tree AS t ON c.id = t.id)
              	
              SELECT j.school_idol_no, REPLACE(j.card_name, '&apos;', '''') AS 'card_name',
              	REPLACE(e.card_name_en, '&apos;', '''') AS 'card_name_en', m.member, 
              	CASE
              		WHEN j.card_rarity_type = 10 THEN 'R'
              		WHEN j.card_rarity_type = 20 THEN 'SR'
              		WHEN j.card_rarity_type = 30 THEN 'UR'
              		ELSE j.card_rarity_type
              	END AS 'card_rarity_type',
              	CASE
              		WHEN j.card_attribute = 1 THEN 'smile'
              		WHEN j.card_attribute = 2 THEN 'pure'
              		WHEN j.card_attribute = 3 THEN 'cool'
              		WHEN j.card_attribute = 4 THEN 'active'
              		WHEN j.card_attribute = 5 THEN 'natural'
              		WHEN j.card_attribute = 6 THEN 'elegant'
              	END AS 'card_attribute',
              	CASE
              		WHEN j.role = 1 THEN 'vo'
              		WHEN j.role = 2 THEN 'sp'
              		WHEN j.role = 3 THEN 'gd'
              		WHEN j.role = 4 THEN 'sk'
              	END AS 'role',
              	j.max_passive_skill_slot, p.training_tree_card_passive_skill_increase_m_id
              FROM names_jp AS j
              LEFT JOIN names_en AS e ON j.school_idol_no = e.school_idol_no
              LEFT JOIN members AS m ON j.school_idol_no = m.school_idol_no
              LEFT JOIN passive_skill AS p ON j.school_idol_no = p.school_idol_no
              ORDER BY j.school_idol_no") %>%
  # Did this here instead of in the query since the query looked ugly enough already.
  mutate(card_name = str_replace_all(card_name, "&amp;", "&"),
         card_name = str_replace_all(card_name, "&quot;", '"'),
         card_name_en = str_replace_all(card_name_en, "&amp;", "&"),
         card_name_en = str_replace_all(card_name_en, "&quot;", '"')) %>%
  dbWriteTable(db1, "cards_basic_info", ., overwrite = T)

# Active Skills
#
# Cards which have a dual active skill where only one of the effects has a target
# will have a target listed for both skills. This should not cause issues, but
# noting it just in case if affects the environment.
#
# This should rewritten with the `separate` function (like what was done for live
# skills).
dbGetQuery(db, "WITH reference AS(
              	SELECT row_number() OVER (
              		ORDER BY c.school_idol_no) rownum, cas.skill_level,
              		c.school_idol_no, cas.name, ask.description, ask.trigger_probability
              	FROM m_card AS c
              	LEFT JOIN m_card_active_skill AS cas ON c.id = cas.card_master_id
              	LEFT JOIN m_active_skill AS ask ON cas.active_skill_master_id = ask.id),
              	
              	descriptions AS(
              	SELECT r.rownum, d.message
              	FROM m_dictionary AS d
              	LEFT JOIN reference AS r ON r.description = 'k.' || d.id)
              
              SELECT r.school_idol_no, r.skill_level, d.message, r.trigger_probability/100 AS 'act_prob'
              FROM reference AS r
              LEFT JOIN descriptions AS d ON r.rownum = d.rownum
              ORDER BY r.school_idol_no") %>%
  pivot_wider(names_from = skill_level,
              values_from = message,
              names_glue = "sl_{skill_level}") %>%
  cbind(., dual_active_split(use_series(., sl_1), "1"), 
        dual_active_split(use_series(. ,sl_2), "2"),
        dual_active_split(use_series(. ,sl_3), "3"),
        dual_active_split(use_series(. ,sl_4), "4"),
        dual_active_split(use_series(. ,sl_5), "5")) %>%
  mutate(effect1 = active_get_effect(active1_sl1),
         effect2 = active_get_effect(active2_sl1),
         target1 = active_get_target(active1_sl1),
         target2 = active_get_target(active2_sl1),
         target1 = ifelse(!is.na(target2), target2, target1),
         dur1 = active_get_dur(active1_sl1),
         dur2 = active_get_dur(active2_sl1),
         param1 = active_get_param(active1_sl1),
         param2 = active_get_param(active2_sl1),
         act1_val1 = active_get_val(active1_sl1),
         act1_val2 = active_get_val(active1_sl2),
         act1_val3 = active_get_val(active1_sl3),
         act1_val4 = active_get_val(active1_sl4),
         act1_val5 = active_get_val(active1_sl5),
         act2_val1 = active_get_val(active2_sl1),
         act2_val2 = active_get_val(active2_sl2),
         act2_val3 = active_get_val(active2_sl3),
         act2_val4 = active_get_val(active2_sl4),
         act2_val5 = active_get_val(active2_sl5)) %>%
  select(school_idol_no, effect1, act1_val1, act1_val2,
         act1_val3, act1_val4, act1_val5, target1, dur1, param1,
         effect2, act2_val1, act2_val2,
         act2_val3, act2_val4, act2_val5, target2, dur2, param2,
         act_prob) %>%
  dbWriteTable(db1, "cards_active_skill", ., overwrite = T)

# Passive Skills

dbGetQuery(db, "SELECT C.school_idol_no, P.skill_level, D.message
                FROM dict.m_dictionary AS D
                JOIN m_card_passive_skill_original AS P ON
                  D.id = 'passive_skill_description_' || P.passive_skill_master_id
                LEFT JOIN m_card AS C ON C.id = P.card_master_id
                WHERE P.position == 1
                ORDER BY C.school_idol_no") %>%
  pivot_wider(names_from = skill_level, 
              values_from = message, 
              names_glue = "sl{skill_level}") %>%
  mutate(pass_target = passive_get_target(sl1)) %>%
  # This could probably be rewritten to use a walrus operator
  # inside of a mutate, but I'll need to sit down and figure
  # out how that will work.
  separate(sl1, c("passive1_sl1", "passive2_sl1"), "\n基本",
           remove = TRUE, fill = "right") %>%
  separate(sl2, c("passive1_sl2", "passive2_sl2"), "\n基本",
           remove = TRUE, fill = "right") %>%
  separate(sl3, c("passive1_sl3", "passive2_sl3"), "\n基本",
           remove = TRUE, fill = "right") %>%
  separate(sl4, c("passive1_sl4", "passive2_sl4"), "\n基本",
           remove = TRUE, fill = "right") %>%
  separate(sl5, c("passive1_sl5", "passive2_sl5"), "\n基本",
           remove = TRUE, fill = "right") %>%
  separate(sl6, c("passive1_sl6", "passive2_sl6"), "\n基本",
           remove = TRUE, fill = "right") %>%
  separate(sl7, c("passive1_sl7", "passive2_sl7"), "\n基本",
           remove = TRUE, fill = "right") %>%
  mutate(pass1_val1 = str_extract(passive1_sl1, "([:digit:]|\\.)+") %>% as.numeric(),
         pass2_val1 = str_extract(passive2_sl1, "([:digit:]|\\.)+") %>% as.numeric(),
         pass1_val2 = str_extract(passive1_sl2, "([:digit:]|\\.)+") %>% as.numeric(),
         pass2_val2 = str_extract(passive2_sl2, "([:digit:]|\\.)+") %>% as.numeric(),
         pass1_val3 = str_extract(passive1_sl3, "([:digit:]|\\.)+") %>% as.numeric(),
         pass2_val3 = str_extract(passive2_sl3, "([:digit:]|\\.)+") %>% as.numeric(),
         pass1_val4 = str_extract(passive1_sl4, "([:digit:]|\\.)+") %>% as.numeric(),
         pass2_val4 = str_extract(passive2_sl4, "([:digit:]|\\.)+") %>% as.numeric(),
         pass1_val5 = str_extract(passive1_sl5, "([:digit:]|\\.)+") %>% as.numeric(),
         pass2_val5 = str_extract(passive2_sl5, "([:digit:]|\\.)+") %>% as.numeric(),
         pass1_val6 = str_extract(passive1_sl6, "([:digit:]|\\.)+") %>% as.numeric(),
         pass2_val6 = str_extract(passive2_sl6, "([:digit:]|\\.)+") %>% as.numeric(),
         pass1_val7 = str_extract(passive1_sl7, "([:digit:]|\\.)+") %>% as.numeric(),
         pass2_val7 = str_extract(passive2_sl7, "([:digit:]|\\.)+") %>% as.numeric(),
         pass1_stat = passive_get_stat(passive1_sl1),
         pass2_stat = passive_get_stat(passive2_sl1)) %>%
  select(school_idol_no, pass1_val1, pass1_val2, pass1_val3, pass1_val4,
         pass1_val5, pass1_val6, pass1_val7, pass2_val1, pass2_val2,
         pass2_val3, pass2_val4, pass2_val5, pass2_val6, pass2_val7,
         pass_target, pass1_stat, pass2_stat) %>%
  dbWriteTable(db1, "cards_passive_skill", ., overwrite = T)

# Live Skills

dbGetQuery(db, "WITH live_descriptions AS (
                SELECT C.school_idol_no, D.message
                FROM dict.m_dictionary AS D
                LEFT JOIN m_card_passive_skill_original AS P ON
                  D.id = 'passive_skill_description_' || P.passive_skill_master_id
                CROSS JOIN m_card AS C ON C.id = P.card_master_id
                WHERE P.position == 2
                ORDER BY C.school_idol_no)
                
                SELECT C.school_idol_no, L.message
                FROM m_card AS C
                LEFT JOIN live_descriptions AS L ON C.school_idol_no = L.school_idol_no") %>%
  separate(message, c("live1", "live2"), "となり、",
           remove = FALSE, fill = "right") %>%
  mutate(live1_effect = live_get_effect(live1),
         live2_effect = live_get_effect(live2),
         live1_val = live_get_val(live1),
         live2_val = live_get_val(live2),
         live1_param = active_get_param(live1),
         live2_param = active_get_param(live2),
         live_target = lapply(message, live_get_target) %>% unlist(),
         live_condition = live_get_condition(message),
         live_chance = live_get_chance(message),
         live_dur = live_get_dur(message),
         live_limit = live_get_limit(message)) %>%
  select(-c(message, live1, live2)) %>%
  dbWriteTable(db1, "cards_live_skill", ., overwrite = T)


#############################################################################

invisible(map(c(db, db1, sifas), dbDisconnect))
