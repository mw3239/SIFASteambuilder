
##########################################################################
db = dbConnect(SQLite(),dbname="masterdata-jun.db")

tree <- dbGetQuery(db,"SELECT T.*, C.school_idol_no, C.card_rarity_type
              FROM m_training_tree_card_param T
              LEFT JOIN m_card C ON T.id=C.id
              ORDER BY C.school_idol_no")

total.cards = tail(tree$school_idol_no,1)

#Stats from the skill tree, regardless of level.
stat.tree <- tibble(card.num=seq(1,total.cards),appeal0=0,appeal1=0,appeal2=0,appeal3=0,appeal4=0,appeal5=0,
       stam0=0,stam1=0,stam2=0,stam3=0,stam4=0,stam5=0,
       tech0=0,tech1=0,tech2=0,tech3=0,tech4=0,tech5=0)

#Stats at various level caps. 40, 46, 52, 58 for R. 60, 64, 68, 72 for SR, 80, 82, 84, 86 for UR. So 'alev1' = appeal with 1
#level uncap increase, so either 46, 64, or 82 depending on the rarity.
stat.level.cap <- tibble(card.num=seq(1,total.cards),alev0=0,alev1=0,alev2=0,alev3=0,
       slev0=0,slev1=0,slev2=0,slev3=0,
       tlev0=0,tlev1=0,tlev2=0,tlev3=0)

#The data from the tables is unfortunately missing the very important uncap info which is tied to the rarity and content number (aka
#the node on the tree.) I swear there's no easier way than manually assigning the nodes of each tree their appropriate uncap value.
stat.tree[,] <- tree %>% mutate(uncap = case_when(card_rarity_type==10 & training_content_no %in% c(seq(1,12),seq(21,32))~0,
                                  card_rarity_type==10 & training_content_no %in% c(13,14,33,34)~1,
                                  card_rarity_type==10 & training_content_no %in% c(15,16,35,36)~2,
                                  card_rarity_type==10 & training_content_no %in% c(17,seq(37,40))~3,
                                  card_rarity_type==10 & training_content_no %in% c(18,19,seq(41,44))~4,
                                  card_rarity_type==10 & training_content_no %in% c(20,seq(45,48))~5,
                                  card_rarity_type==20 & training_content_no %in% c(seq(1,13),seq(23,39))~0,
                                  card_rarity_type==20 & training_content_no %in% c(14,15,seq(40,43))~1,
                                  card_rarity_type==20 & training_content_no %in% c(16,17,seq(44,47))~2,
                                  card_rarity_type==20 & training_content_no %in% c(18,seq(48,51))~3,
                                  card_rarity_type==20 & training_content_no %in% c(19,20,21,seq(52,54))~4,
                                  card_rarity_type==20 & training_content_no %in% c(22,seq(55,60))~5,
                                  card_rarity_type==30 & training_content_no %in% c(seq(1,15),seq(27,47))~0,
                                  card_rarity_type==30 & training_content_no %in% c(16,17,seq(48,51))~1,
                                  card_rarity_type==30 & training_content_no %in% c(18,19,20,seq(52,55))~2,
                                  card_rarity_type==30 & training_content_no %in% c(21,22,seq(56,61))~3,
                                  card_rarity_type==30 & training_content_no %in% c(23,24,25,seq(62,66))~4,
                                  card_rarity_type==30 & training_content_no %in% c(26,seq(67,72))~5)) %>%
  group_by(school_idol_no,training_content_type,uncap) %>% #We need each card, each of the 3 parameters, and all uncaps levels for each of them.
  summarize(stat=sum(value)) %>% #The sum of each stat obtained from each uncap level for each card.
  dcast(school_idol_no ~ training_content_type + uncap) %>% #Widen the df so that it matches the format of stat.components
  #rename_with(~ str_replace(.,"2_","stam")) %>% #Renaming is a waste if I'm just reassigning it to an already existing df
  #rename_with(~ str_replace(.,"3_","appeal")) %>% #But I'm keeping it here in case I want to reuse this syntax later.
  #rename_with(~ str_replace(.,"4_","tech")) %>%
  select(1,8:13,2:7,14:19) #For some reason content_type 2 is stamina and 3 is appeal internally. Dunno why they're reversed. This fixes that.

rm(tree)

#Adds the stat bonuses from awakening to the appropriate uncap 0 stats.
stat.tree[,c(2,8,14)] <- dbGetQuery(db,"SELECT A.parameter1, A.parameter2, A.parameter3 
              FROM m_card_awaken_parameter A
              LEFT JOIN m_card C ON A.card_master_id=C.id
              ORDER BY C.school_idol_no") %>%
  arrange(2,1,3) %>% #This table also puts stamina before appeal for some reason.
  add(stat.tree[,c(2,8,14)])

#And finally, the base stats are each of the level caps. The base level cap is 40/60/80 depending on the rarity,
#but can be increased by 6/4/2 up to 3 times. 
stat.level.cap[,] <- dbGetQuery(db,"SELECT C.school_idol_no, C.card_rarity_type, L.level, L.appeal, L.stamina, L.technique
                              FROM m_card_parameter L
                              LEFT JOIN m_card C ON L.card_m_id=C.id
                              ORDER BY C.school_idol_no") %>%
  #The m_card_parameter table holds stat values for all cards up to level 100, but
  #we only care about four of them currently.
  filter((card_rarity_type==10 & level %in% c(40,46,52,58))|
                (card_rarity_type==20 & level %in% c(60,64,68,72))|
                (card_rarity_type==30 & level %in% c(80,82,84,86))) %>%
  mutate(level = case_when(level %in% c(40,60,80)~0,
                   level %in% c(46,64,82)~1,
                   level %in% c(52,68,84)~2,
                   level %in% c(58,72,86)~3)) %>%
  select(1,3:6) %>%
  pivot_wider(names_from=level, values_from = c(appeal,stamina,technique)) #Pivot wider is so much nicer than reshape2 wow

dbDisconnect(db)

db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
dbWriteTable(db,"cards_tree_nodes",stat.tree,overwrite=T)
dbWriteTable(db,"cards_level_cap",stat.level.cap,overwrite=T)
dbDisconnect(db)


##########################################################################################################
