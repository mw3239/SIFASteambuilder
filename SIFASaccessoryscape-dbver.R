db = dbConnect(SQLite(), "masterdata-jun.db")
db1 = dbConnect(SQLite(),"SIFAS.sqlite")

invisible(dbExecute(db,"ATTACH 'dictionary_ja_k-jun.db' AS 'dict'"))
dbGetQuery(db,"SELECT A.accessory_no AS 'accessory.num', D.message AS 'accessory.name', A.rarity_type AS 'accessory.rarity', A.attribute AS 'accessory.element',
           G.grade AS 'accessory.uncaps', P.level, P.appeal, P.stamina, P.technique, S.max_level as 'acc.max.sl', D2.message, A.id
           FROM m_accessory A
           JOIN dict.m_dictionary D ON A.name = 'k.' || D.id
           JOIN m_accessory_grade_up G ON A.id = G.accessory_master_id
           JOIN m_accessory_parameter P ON A.id = P.accessory_master_id
           LEFT JOIN m_accessory_passive_skill S ON G.accessory_passive_skill_1_master_id = S.id
           LEFT JOIN dict.m_dictionary D2 ON D2.id = 'accessory_passive_skill_description_' || S.id || '_1'
           WHERE P.level = G.max_level
           ORDER BY A.accessory_no, G.grade") %>% 
  #Only the 0th uncap has a max sl entry (even though it displays the max sl for 5 uncaps). All uncap entries just say NA instead.
  #This fills in those NAs with the most recent non-NA.
  fill(acc.max.sl) %>%
  #Max skill level info is only stored at max uncaps aside from special accessories (the dream parade ones), so all others must be calculated
  mutate(acc.max.sl = ifelse(acc.max.sl %% 5 == 0,acc.max.sl-5+accessory.uncaps,acc.max.sl)) %>%
  mutate(accessory.name = case_when(accessory.name=="星のイヤリング"~"Star Earrings",
                                    accessory.name=="ボーダーのタオル"~"Striped Towel",
                                    accessory.name=="音ノ木坂学院の校章"~"Otonokizaka Badge",
                                    accessory.name=="浦の星女学院の校章"~"Uranohoshi Badge",
                                    accessory.name=="虹ヶ咲学園の校章"~"Nijigasaki Badge",
                                    accessory.name=="サンタ帽（μ&apos;s）"~"Santa Hat (μ's)",
                                    accessory.name=="サンタ帽（Aqours）"~"Santa Hat (Aqours)",
                                    accessory.name=="サンタ帽（ニジガク）"~"Santa Hat (Niji)",
                                    accessory.name=="ニジガクスクールバッグ"~"Nijigasaki Schoolbag",
                                    accessory.name=="ユニットピンズ（DiverDiva）"~"Unit Pins (DiverDiva)",
                                    accessory.name=="ユニットピンズ（A・ZU・NA）"~"Unit Pins (A・ZU・NA)",
                                    accessory.name=="ユニットピンズ（QU4RTZ）"~"Unit Pins (QU4RTZ)",
                                    accessory.name=="星のネックレス"~"Star Necklace",
                                    accessory.name=="ヒツジのニット帽"~"Sheep-knit Hat",
                                    accessory.name=="はじまりのCD"~"First CD",
                                    accessory.name=="ユニットピンズ（CYaRon！）"~"Unit Pins (CYaRon!)",
                                    accessory.name=="ユニットピンズ（AZALEA）"~"Unit Pins (AZALEA)",
                                    accessory.name=="ユニットピンズ（Guilty Kiss）"~"Units Pins (Guilty Kiss)",
                                    accessory.name=="ハートのベルト"~"Heart Belt",
                                    accessory.name=="四葉のチョーカー"~"Four-Leaf Choker",
                                    accessory.name=="雪結晶のバングル"~"Snow Crystal Bangle",
                                    accessory.name=="音符のブローチ"~"Musical Note Brooch",
                                    accessory.name=="音符のキーホルダー"~"Musical Note Keychain",
                                    accessory.name=="フラワーのブレスレット"~"Flower Bracelet",
                                    accessory.name=="フラワーのヘアピン"~"Flower Hairpin",
                                    accessory.name=="チェックのリボン"~"Checkered Ribbon",
                                    accessory.name=="チェックのポーチ"~"Checkered Pouch",
                                    accessory.name=="ボーダーのリストバンド"~"Striped Wristband",
                                    accessory.name=="ローダンセの髪飾り"~"Rhodanthe Hair Clip"
                                   T~accessory.name)) %>%
  mutate(accessory.attribute = case_when(accessory.element==1~"Smile",
                                       accessory.element==2~"Pure",
                                       accessory.element==3~"Cool",
                                       accessory.element==4~"Active",
                                       accessory.element==5~"Natural",
                                       accessory.element==6~"Elegant",
                                       T~as.character(accessory.element))) %>%
  mutate(accessory.name = case_when(accessory.rarity==10~str_c(accessory.name," (R)"),
                                    accessory.rarity==20~str_c(accessory.name," (SR)"),
                                    accessory.rarity==30~str_c(accessory.name," (UR)"),
                                    T~accessory.name)) %>%
  mutate(accessory.skill = case_when(str_detect(message,"SPゲージ獲得量")~"SP Gain per Tap",
                                     str_detect(message,"スタミナダメージ")~"Damage Reduction",
                                     str_detect(message,"アピールが(増加|([:digit:]|.))+\\%")~"Increase Appeal (Percentage)", #Two possibilies here: "アピールが増加" or アピールが増加##%", but NOT アピールが増加## (without the percentage)
                                     str_detect(message,"特技発動率")~"Skill activation rate",
                                     str_detect(message,"SPゲージを獲得")~"SP gauge fill",
                                     str_detect(message,"クリティカル値")~"Critical Value",
                                     str_detect(message,"シールド")~"Shield",
                                     str_detect(message,"回復")~"Stamina Recovery",
                                     str_detect(message,"アピールが")~"Increase Appeal (Flat)",
                                     str_detect(message,"獲得するボルテージ")~"SP Voltage",
                                     str_detect(message,"ボルテージの上限")~"Voltage Cap",
                                     str_detect(message,"クリティカル率")~"Critical Rate",
                                     T~message)) %>%
  mutate(accessory.cond = case_when(str_detect(message,"多いほど")~"Healthy",
                                    str_detect(message,"\\(AC\\)成功時")~"AC Clear",
                                    str_detect(message,"コンボ数に応じて")~"Combo",
                                    str_detect(message,"楽曲開始時")~"Song Start",
                                    str_detect(message,"スタミナが80\\％以下")~"80% Stamina",
                                    str_detect(message,"作戦変更時")~"Strategy Swap",
                                    str_detect(message,"\\(AC\\)開始時")~"AC Start",
                                    str_detect(message,"ダメージを受けた時")~"Receive Damage",
                                    str_detect(message,"楽曲中")~"Permanent",
                                    T~message)) %>%
  #Some accessory skills/conditions will be NA, so filling needs to occur before extracting them.
  fill(message) %>%
  fill(accessory.skill) %>%
  fill(accessory.cond) %>%
  mutate(accessory.rate = ifelse(str_detect(message,"確率:([:digit:]|.)+\\%"),
                                 as.numeric(str_remove(str_remove(str_extract(message,"確率:([:digit:]|.)+\\%"),"確率:"),"\\%"))/100, #Tried this with two extracts rather than an extract plus two removes but it didn't work and I have no idea why.
                                 NA)) %>%
  mutate(accessory.dur = ifelse(str_detect(message,"[:digit:]+ノーツ"),
                                str_extract(message,"^[:digit:]+"),
                                NA)) %>%
  mutate(accessory.num = seq(1:length(accessory.name))) %>%
  #Rearrange to be consistent with the old table.
  select(1,12,2,5,6,10,7,8,9,13,14,15,16,17) %>%
  #Rename to be consistent with the old table as to not break something somewhere else
  rename(accessory.element=accessory.attribute,accessory.max.level=level,accessory.appeal=appeal,accessory.stam=stamina,accessory.tech=technique) %>%
  dbWriteTable(db1,"accessories",.,overwrite=T)

invisible(map(c(db,db1),dbDisconnect))
