#Not updated for the 7 summer skills.

db = dbConnect(SQLite(), "masterdata-jun.db")
db1 = dbConnect(SQLite(),"SIFAS.sqlite")

#The relevant data for inspiration skills is spread out across two different database files.
#masterdata only contains a pointer to the dictionary file, where the actual text for the skills is stored.
invisible(dbExecute(db,"ATTACH 'dictionary_ja_k-jun.db' AS 'dict'"))
#The passive_skill table contains more than just inspiration skills. Internally, inspiration skills
#are known as "replacement" skills. Also note that there seems to be more inspiration skills in the game's data
#than are actually obtainable in game, but I have no confirmed this. EDIT: The excess skill problem has been fixed
#by changing the query. Now only skills that appear in game are used.
dbGetQuery(db,"SELECT A.message AS 'name', B.message AS 'description'
           FROM m_passive_skill AS S
           LEFT JOIN dict.m_dictionary AS A ON S.name = 'k.' || A.id
           LEFT JOIN dict.m_dictionary AS B ON S.description = 'k.' || B.id
           WHERE S.name = 'k.replacement_skill_name_' || S.id") %>%
  #Translating the name of each skill is a lengthy endeavor that's divided into four parts.
  #First, the Effect.
  mutate(name=case_when(str_detect(name,"アピール\uFF0B")~str_replace(name,"アピール\uFF0B","Appeal\\+"),
                        str_detect(name,"クリティカル\uFF0B")~str_replace(name,"クリティカル\uFF0B","Critical Value\\+"),
                        str_detect(name,"特技発動率\uFF0B")~str_replace(name,"特技発動率\uFF0B","Skill Activation Rate\\+"),
                        str_detect(name,"タイプ効果\uFF0B")~str_replace(name,"タイプ効果\uFF0B","Type Bonus\\+"),
                        str_detect(name,"ボルテージ獲得")~str_replace(name,"ボルテージ獲得","Gain Voltage"),
                        str_detect(name,"スタミナ回復")~str_replace(name,"スタミナ回復","Stamina Recovery"),
                        str_detect(name,"シールド獲得")~str_replace(name,"シールド獲得","Shield"),
                        str_detect(name,"コンボ数UP")~str_replace(name,"コンボ数UP","Combo UP"),
                        str_detect(name,"ボルテージUP")~str_replace(name,"ボルテージUP","Voltage UP"),
                        str_detect(name,"クリティカルUP")~str_replace(name,"クリティカルUP","Crit Rate UP"),
                        str_detect(name,"SP特技UP")~str_replace(name,"SP特技UP","SP Skill Voltage"),
                        str_detect(name,"ダメージ軽減")~str_replace(name,"ダメージ軽減","Damage Reduction"),
                        str_detect(name,"アピールUP")~str_replace(name,"アピールUP","Appeal UP"),
                        str_detect(name,"特技発動率UP")~str_replace(name,"特技発動率UP","Skill Activation Rate UP"),
                        T~name)) %>%
  #Then its strength
  mutate(name=case_when(str_detect(name,"小")~str_replace(name,"小","Small"),
                        str_detect(name,"中")~str_replace(name,"中","Medium"),
                        str_detect(name,"大")~str_replace(name,"大","Large"),
                        str_detect(name,"特")~str_replace(name,"特","Special"),
                        str_detect(name,"極")~str_replace(name,"極","Ultimate"),
                        T~name)) %>%
  #Target
  mutate(name=case_when(str_detect(name,"仲間")~str_replace(name,"仲間"," Allies"),
                        str_detect(name,"タイプ")~str_replace(name,"タイプ"," Type"),
                        str_detect(name,"同学校")~str_replace(name,"同学校"," Same School"),
                        str_detect(name,"同学年")~str_replace(name,"同学年"," Same Year"),
                        str_detect(name,"全員")~str_replace(name,"全員"," All"),
                        str_detect(name,"同作戦")~str_replace(name,"同作戦"," Same Strategy"),
                        str_detect(name,"同属性")~str_replace(name,"同属性"," Same Element"),
                        T~name)) %>%
  #Activation Condition
  mutate(name=case_when(str_detect(name,"曲開始時")~str_replace(name,"曲開始時"," Song Start "),
                        str_detect(name,"30\\%達成時")~str_replace(name,"30\\%達成時"," 30\\% Target "),
                        str_detect(name,"残80\\%時")~str_replace(name,"残80\\%時"," 80\\% Stamina "),
                        str_detect(name,"AC成功時")~str_replace(name,"AC成功時"," AC Clear "),
                        str_detect(name,"AC時")~str_replace(name,"AC時"," AC Start　"),
                        T~name)) %>%
  #While the name makes it easy for users to search for the skills they're looking for, it unfortunately does not
  #do much for any calculations. For calculations, the information must be divided further based on its description.
  #First is the effect.
  mutate(inspi.effect=case_when(str_detect(description,"アピールが")~"Appeal+",
                                str_detect(description,"クリティカル値")~"Critical value",
                                str_detect(description,"特技発動率")~"Skill activation rate",
                                str_detect(description,"タイプ効果")~"Type bonus",
                                str_detect(description,"SP特技")~"SP Skill Up",
                                str_detect(description,"ボルテージを獲得")~"Voltage Up",
                                str_detect(description,"スタミナを回復")~"Stamina recovery",
                                str_detect(description,"シールドを獲得")~"Shield",
                                str_detect(description,"コンボ数")~"Combo Count Up",
                                str_detect(description,"ボルテージが")~"Voltage Up",
                                str_detect(description,"クリティカル率")~"Critical rate",
                                str_detect(description,"スタミナダメージ")~"Damage reduction",
                                T~"")) %>%
  #Then the value. This can either be a percentage increase of flat boost.
  mutate(inspi.val = get.inspi.val(description)) %>%
  #Next the duration.
  #Note that the 基本~Permanet check MUST be at the end of this sequence as some of the ones that fell into the notes and remainder
  #categories also contain that text.
  mutate(inspi.dur=case_when(str_detect(description,"ノーツ")~str_replace(str_extract(description,"[:digit:]+ノーツ"),"ノーツ"," notes"),
                             str_detect(description,"終了まで")~"Remainder",
                             str_detect(description,"基本")~"Permanent",
                             T~"")) %>%
  #Who's effected by the skill
  mutate(inspi.target = case_when(str_detect(description,"自身以外")~"allies",
                                  str_detect(description,"同タイプ")~"type",
                                  str_detect(description,"同学校")~"school",
                                  str_detect(description,"同学年")~"year",
                                  str_detect(description,"全員")~"all",
                                  str_detect(description,"同作戦")~"strategy",
                                  str_detect(description,"同属性")~"attribute",
                                  str_detect(description,"自身")~"self",
                                  T~"")) %>%
  #Condition required to activate.
  mutate(inspi.cond = case_when(str_detect(description,"楽曲開始")~"Song Start",
                                str_detect(description,"30％達成")~"30% Target",
                                str_detect(description,"成功時")~"AC Clear",
                                str_detect(description,"80％以下")~"80% Stam",
                                str_detect(description,"AC\\)開始時")~"AC Start",
                                T~"Permanent")) %>%
  #Chance of activating
  mutate(inspi.chance = ifelse(str_detect(description,"確率:"),
                               suppressWarnings(as.numeric(str_remove(str_extract(description,"確率:[0-9]*"),"確率:")))/100,
                               1)) %>%
  #Sorts alphabetically
  arrange(name) %>%
  #Sets an id column
  mutate(inspi.num = seq(1:length(name))) %>%
  #Renamed to be consistant with my previous version of the db. Don't want to retroactively break anything by changing column names.
  rename("inspi.name"=name) %>%
  #The description column is no longer relevant, so it can be removed.
  select(inspi.num,inspi.name,inspi.effect,inspi.val,inspi.dur,inspi.target,inspi.cond,inspi.chance) %>%
  dbWriteTable(db1,"inspi_skills",.,overwrite=T)

invisible(map(c(db,db1),dbDisconnect))