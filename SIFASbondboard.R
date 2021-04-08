db = dbConnect(SQLite(),dbname="masterdata-jun.db")

#Thankfully the columns in the tables for both the cells and bonuses have the same exact structure, so we can union them.
bond_board <- dbGetQuery(db,"SELECT member_love_panel_master_id, bonus_type, bonus_value
           FROM m_member_love_panel_cell
           WHERE member_love_panel_master_id LIKE '%%001'
           
           UNION
           
           SELECT *
           FROM m_member_love_panel_bonus
           WHERE member_love_panel_master_id LIKE '%%001' ") %>% #id is formated as board# + character#. The boards for all characters are the same, so all but once character can be filtered out.
  mutate(bonus_value = ifelse(bonus_type < 13, bonus_value/100, bonus_value)) %>% #13-15 are the level cap increases - the only ones that aren't percentages
  mutate(bonus_type = case_when(bonus_type==1~"Appeal",
         bonus_type==2~"Stamina",bonus_type==3~"Technique",
         bonus_type==4~"Crit Rate",bonus_type==5~"Crit Value",
         bonus_type==6~"SP gauge per tap",bonus_type==7~"SP Skill Voltage",
         bonus_type==8~"Sp Minus Effect",bonus_type==9~"Vo Minus Effect",
         bonus_type==10~"Gd Minus Effect",bonus_type==11~"Sk Minus Effect",
         bonus_type==12~"Element Bonus",bonus_type==13~"R Level Cap",
         bonus_type==14~"SR Level Cap",bonus_type==15~"UR Level Cap")) %>%
  mutate(member_love_panel_master_id = str_remove(member_love_panel_master_id,"001")) %>%
  rename("board.no"=1,"stat"=2,"value"=3)

#Retrieves the required bond level required to access each board. 
bond_level_req <- dbGetQuery(db,"SELECT id, love_level_master_love_level
           FROM m_member_love_panel
           WHERE member_master_id == 1
           AND id LIKE '%%001'") %>%
  mutate(id = str_remove(id,"001")) %>%
  rename(board.no=1,level.req=2)

dbDisconnect(db)



db = dbConnect(SQLite(),"SIFAS.sqlite")
dbWriteTable(db,"bond_board",bond_board,overwrite=T)
dbWriteTable(db,"bond_board_level_req",board_level_req,overwrite=T)
dbDisconnect(db)
