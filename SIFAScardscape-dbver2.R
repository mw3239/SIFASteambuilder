#Grabs the basic info for each card - its name, character, rarity, etc. Stats and Skills are scattered across many
#different tables, so breaking this down into smaller tables makes life easier.
scrape_card_info <- function(masterdata="masterdata.db",SIFAS="SIFAS.sqlite"){
db = dbConnect(SQLite(),dbname=masterdata)
db1 = dbConnect(SQLite(),dbname=SIFAS)

invisible(dbExecute(db,"ATTACH 'dictionary_ja_k-jun.db' AS 'dict'"))

  dbGetQuery(db,"SELECT C.id, C.school_idol_no, A.message AS 'idol', C.card_rarity_type, C.card_attribute, C.passive_skill_slot, C.role, D.message as 'card_name'
             FROM m_card C
             JOIN dict.m_dictionary D ON D.id = 'card_name_awaken_' || C.id
             LEFT JOIN m_member M ON C.member_m_id = M.id
             JOIN dict.m_dictionary A ON M.name_romaji = 'k.' || A.id") %>%
  arrange(school_idol_no) %>%
  #Will need to be updated for Party URs. I can't wait for that woo-hoo...
  mutate(card_rarity_type = case_when(card_rarity_type==10~"R",
                                      card_rarity_type==20~"SR",
                                      card_rarity_type==30 & passive_skill_slot==2~"UR(Fes)",
                                      card_rarity_type==30~"UR",
                                      T~as.character(card_rarity_type)),
         card_attribute = case_when(card_attribute==1~"Smile",
                                    card_attribute==2~"Pure",
                                    card_attribute==3~"Cool",
                                    card_attribute==4~"Active",
                                    card_attribute==5~"Natural",
                                    card_attribute==6~"Elegant",
                                    T~as.character(card_attribute)),
         role = case_when(role==1~"vo",
                          role==2~"sp",
                          role==3~"gd",
                          role==4~"sk",
                          T~as.character(role)),
         idol = str_remove(idol,"[:blank:][:graph:]+"),
         idol = tolower(idol),
         card_name = case_when(str_detect(card_name,"&amp;")~str_replace(card_name,"&amp;","&"),
                               T~card_name)) %>% #There's likely more of these special characters. Actually, this doesn't account for
                                                 #multiple special characters in a string. Would need to write a function that
                                                 #checks for and replaces any number of any possible special characters.
  select(school_idol_no,card_name,card_rarity_type,idol,card_attribute,role,idol,id) %>%
  dbWriteTable(db1,"cards_basic_info",.,overwrite=T)

invisible(map(c(db,db1),dbDisconnect))
}

##############################ACTIVE SKILL#############################################

#Now works properly for cards with multiple effects on their active skills, although more columns were added as a result.
scrape_card_actives <- function(masterdata="masterdata.db",SIFAS="SIFAS.sqlite",dict="dictionary_ja_k.db"){
  db = dbConnect(SQLite(),dbname=masterdata)
  db1 = dbConnect(SQLite(),dbname=SIFAS)
  
  
  invisible(dbExecute(db,"ATTACH 'dictionary_ja_k-jun.db' AS 'dict'"))
  invisible(dbExecute(db,"ATTACH 'SIFAS.sqlite' AS 'SIFAS'"))
  
  dbGetQuery(db,"SELECT B.school_idol_no, D.message
             FROM m_card C
             JOIN dict.m_dictionary D ON D.id LIKE 'active_skill_description_' || C.id || '%'
             JOIN SIFAS.cards_basic_info B ON B.id = C.id
             ORDER BY B.school_idol_no") %>% #Grabs the id and all active skill descriptions
    mutate(skill_level = row_number(school_idol_no) %% 5) %>% #Creates a column to mark the skill level
    mutate(skill_level = case_when(skill_level == 0~5,
                                   skill_level %in% seq(1,4)~skill_level)) %>% #Ensures skill level 5 is skill level 5 (not 0)
    pivot_wider(names_from=skill_level,values_from=message,names_glue="sl_{skill_level}")%>% #Makes one row for each id and 5 columns for each level.
    cbind(.,dual_active_split(use_series(.,sl_1),"1"),dual_active_split(use_series(.,sl_2),"2"), #Might be a more efficient way to do this
          dual_active_split(use_series(.,sl_3),"3"),dual_active_split(use_series(.,sl_4),"4"), #but it's fine. Splits the messages for the 5
          dual_active_split(use_series(.,sl_5),"5")) %>% #SLs into 10 columns. If there is no secondary effect, it will be NA.
    mutate(effect1 = active_get_effect(active1_sl1),
           effect2 = active_get_effect(active2_sl1),
           target1 = active_get_target(active1_sl1),
           target2 = active_get_target(active2_sl1),
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
    select(school_idol_no,effect1,act1_val1,act1_val2,act1_val3,act1_val4,act1_val5,target1,dur1,param1,
           effect2,act2_val1,act2_val2,act2_val3,act2_val4,act2_val5,target2,dur2,param2) %>%
    dbWriteTable(db1,"cards_active_skill",.,overwrite=T)
  
  invisible(map(c(db,db1),dbDisconnect))
  
}

#############################################################################

#Below are several helper functions used by the scrape_card_actives function.


#Take in a list of active skill messages and a string indicating their skill level.
#Returns an n x 2 tibble with the active skill messages split between the columns (in the event that the skill has multiple effects.)
#If a card does not have a dual effect on its active skill, the entirety of the message will be copied into column 1 and column 2 will be NA.
dual_active_split <- function(active_message,sl) {
  strsplit(active_message,split="(?<=(獲得|増加|回復|上昇|軽減)し)",perl=T) %>% #Uses a lookaround to keep all text when it detects a match.
    lapply(`length<-`,2) %>% #Force all elements to have length 2. In other words, assign NA if it's not a dual effect.
    invoke(rbind,.) %>% #Combine all lists into rows.
    `colnames<-`(c(str_c("active1_sl",sl),str_c("active2_sl",sl))) %>% #Rename based on skill level.
    as_tibble() %>%
    return()
}

#Returns the effect of a given active skill given its text.
active_get_effect <- function(message){
  case_when(str_detect(message,"ボルテージを[:digit:]+獲得")~"Voltage Boost (flat)",
            str_detect(message,"%ボルテージを獲得")~"Voltage Boost (percent)",
            str_detect(message,"SPゲージ獲得量")~"SP Gauge Gain",
            str_detect(message,"次に発動するSP特技で獲得するボルテージが自身の(アピール|テクニック|スタミナ)の([:digit:]|\\.)+%増加")~"SP Voltage Boost (%)",
            str_detect(message,"次に発動するSP特技で獲得するボルテージが自身の(アピール|テクニック|スタミナ)の([:digit:]|\\.)+増加")~"SP Voltage Boost (flat)",
            str_detect(message,"スタミナダメージ")~"Damage Reduction",
            str_detect(message,"SPゲージを[:digit:]+獲得")~"SP Gauge (flat)",
            str_detect(message,"%SPゲージ")~"SP Gauge (percent)",
            str_detect(message,"クリティカル率")~"Crit rate",
            str_detect(message,"クリティカル値")~"Crit value",
            str_detect(message,"%スタミナを回復")~"Stamina Recovery (percent)",
            str_detect(message,"[:digit:]+回復")~"Stamina Recovery (flat)",
            str_detect(message,"シールドを[:digit:]+獲得")~"Shield (flat)",
            str_detect(message,"%シールドを獲得")~"Shield (percent)",
            str_detect(message,"ボルテージが([:digit:]|\\.)+%増加")~"Voltage Gain (percent)",
            str_detect(message,"ボルテージが[:digit:]+増加")~"Voltage Gain (flat)",
            str_detect(message,"特技発動率")~"Skill Activation Rate",
            str_detect(message,"アピールが[:digit:]+増加")~"Appeal Boost (flat)",
            str_detect(message,"アピールが([:digit:]|\\.)+%増加")~"Appeal Boost (percent)",
            T~message) %>%
    return()
}

#Returns which cards are affected by the active skill.
active_get_target <- function(message){
  case_when(str_detect(message,"全員")~"all",
            str_detect(message,"同作戦")~"same strategy",
            str_detect(message,"同タイプ")~"same type",
            str_detect(message,"同属性")~"same element",
            str_detect(message,"同学年")~"same year",
            str_detect(message,"同学校")~"same school",
            str_detect(message,"仲間")~"allies") %>%
    return()
}

#Returns how many notes the active skill lasts for.
active_get_dur <- function(message){
  case_when(str_detect(message,"ノーツの間")~as.integer(str_remove(str_extract(message,"[:digit:]+ノーツの間"),"ノーツの間"))) %>%
    return()
}

#Determines which of the card's stats is used for the active skill's value.
active_get_param <- function(message){
  case_when(str_detect(message,"自身のアピール")~"Appeal",
            str_detect(message,"自身のスタミナ")~"Stamina",
            str_detect(message,"自身のテクニック")~"Tech") %>%
    return()
}

#Extracts the value of an active skill at a given skill level.
active_get_val <- function(message){
  case_when(str_detect(message,"[:digit:]+(獲得|回復)")~as.double(str_extract(message,"[:digit:]+")),
            str_detect(message,"[:digit:]+増加")~as.double(str_remove(str_extract(message,"[:digit:]+増加"),"増加")),
            str_detect(message,"([:digit:]|\\.)+%(上昇|増加|軽減|ボルテージ|スタミナ|シールド|SP)")~as.double(str_remove(str_extract(message,"([:digit:]|\\.)+%"),"%"))) %>%
    return()
}



#############################PASSIVE SKILL################################################

scrape_card_passives <- function(masterdata="masterdata.db",SIFAS="SIFAS.sqlite",dict="dictionary_ja_k.db"){
  db = dbConnect(SQLite(),dbname=masterdata)
  db1 = dbConnect(SQLite(),dbname=SIFAS)
  
  invisible(dbExecute(db,"ATTACH 'dictionary_ja_k-jun.db' AS 'dict'"))
  invisible(dbExecute(db,"ATTACH 'SIFAS.sqlite' AS 'SIFAS'"))
  
  dbGetQuery(db,"SELECT B.school_idol_no, C.skill_level, D.message
           FROM m_card_passive_skill_original C
           JOIN SIFAS.cards_basic_info B on B.id = C.card_master_id
           JOIN dict.m_dictionary D on D.id LIKE 'passive_skill_description_' || C.passive_skill_master_id || '%'
           WHERE C.position == 1
           ORDER BY B.school_idol_no") %>%
    pivot_wider(names_from=skill_level,values_from=message,names_glue="sl_{skill_level}") %>% #Makes one row for each id and 5 columns for each level.
  
  invisible(map(c(db,db1),dbDisconnect))
}

#THE BELOW FUNCTION IS UNMODIFIED AND WHERE I LEFT OFF

#Take in a list of passive skill messages and a string indicating their skill level.
#Returns an n x 2 tibble with the active skill messages split between the columns (in the event that the skill has multiple effects.)
#If a card does not have a dual effect on its active skill, the entirety of the message will be copied into column 1 and column 2 will be NA.
dual_passive_split <- function(passive_message,sl) {
  
  
  
  strsplit(active_message,split="(?<=(獲得|増加|回復|上昇|軽減)し)",perl=T) %>% #Uses a lookaround to keep all text when it detects a match.
    lapply(`length<-`,2) %>% #Force all elements to have length 2. In other words, assign NA if it's not a dual effect.
    invoke(rbind,.) %>% #Combine all lists into rows.
    `colnames<-`(c(str_c("active1_sl",sl),str_c("active2_sl",sl))) %>% #Rename based on skill level.
    as_tibble() %>%
    return()
}

db = dbConnect(SQLite(),dbname="masterdata-jun.db")
db1 = dbConnect(SQLite(),dbname="SIFAS.sqlite")

invisible(dbExecute(db,"ATTACH 'dictionary_ja_k-jun.db' AS 'dict'"))
invisible(dbExecute(db,"ATTACH 'SIFAS.sqlite' AS 'SIFAS'"))

testdb <- dbGetQuery(db,"SELECT B.school_idol_no, C.skill_level, D.message
           FROM m_card_passive_skill_original C
           JOIN SIFAS.cards_basic_info B on B.id = C.card_master_id
           JOIN dict.m_dictionary D on D.id LIKE 'passive_skill_description_' || C.passive_skill_master_id || '%'
           WHERE C.position == 1
           ORDER BY B.school_idol_no") %>%
  pivot_wider(names_from=skill_level,values_from=message,names_glue="sl_{skill_level}") #Makes one row for each id and 5 columns for each level.
  



invisible(map(c(db,db1),dbDisconnect))



scrape_card_info(masterdata="masterdata-jun.db")
scrape_card_actives(masterdata="masterdata-jun.db")




test1 <- "基本アピールが2.6%増加\n基本テクニックが5.2%増加\n対象:同属性"

test2 <- testdb$sl_1[1]


str_detect(test2,"\\\n基本")


