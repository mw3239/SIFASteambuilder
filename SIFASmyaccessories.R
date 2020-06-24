# !diagnostics suppress=<;>

load_accessories <- function() {
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  my_accessories <<- dbGetQuery(db,'SELECT * FROM my_accessories')
  dbDisconnect(db)
}

save_accessories <- function() {
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  dbWriteTable(db,"my_accessories",my_accessories,overwrite=T)
  dbDisconnect(db)
}

load_accessory_table <- function(){
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  accessory.table <<- dbGetQuery(db,'SELECT * FROM accessories')
  accessory.skill.table <<- dbGetQuery(db,'SELECT * FROM accessories_skill_vals')
  dbDisconnect(db)
}

init_my_accessories <- function() {
  my_accessories <<- tibble('acc.num' = NA,'acc.name'=NA,'acc.rarity'=NA,
                            'acc.element'=NA,'acc.uncaps'=NA,'acc.appeal'=NA,
                            'acc.stam'=NA,'acc.tech'=NA,'acc.skill.lvl'=NA,
                            'acc.skill'=NA,'acc.skill.cond'=NA,'acc.skill.val'=NA,
                            'acc.skill.rate'=NA,'acc.skill.dur'=NA)
}

#FIRST ARGUMENT: Enter the number corresponding to the accessory's name
#1 = Earrings, 2 = Necklace, 3 = Brooch, 4 = Keychain
#5 = Bracelet, 6 = Hairpin, 7 = Ribbon, 8 = Pouch
#9 = Wristband, 10 = Towel, 11 = Badge, 12 = Santa Hat

#SECOND ARGUMENT: Enter of element of the accessory
#1 = Smile, 2 = Pure, 3 = Cool
#4 = Active, 5 = Natural, 6 = Elegant

#THIRD ARGUMENT: Enter the rarity of the accessory
#1 = R, 2 = SR, 3 = UR

#FOURTH ARGUMENT: Enter the number of uncaps of the accessory
#This will be an integer from 0-5.

#FIFTH ARGUMENT: Enter the skill level of accessory
#For Rs this will be between 1 and 10, 1 and 15 for SRs, and 1 and 20 for URs.
add.accessory <- function(name,element,rarity,uncaps=0,skill=1) {
  if(!exists('my_accessories') || nrow(my_accessories) == 0) {
    init_my_accessories()
  }
  
  if(!exists('accessory.table') || !exists('accessory.skill.table')){
    load_accessory_table()
  }
  
  if((rarity==1 && skill>10) || (rarity==2 && skill>15)){
    print("This rarity can not reach this skill level.")
    stop(call. = F)
  }
  
  to.add <- tibble('acc.num' = NA,'acc.name'=NA,'acc.rarity'=NA,
                            'acc.element'=NA,'acc.uncaps'=NA,'acc.appeal'=NA,
                            'acc.stam'=NA,'acc.tech'=NA,'acc.skill.lvl'=NA,
                            'acc.skill'=NA,'acc.skill.cond'=NA,'acc.skill.val'=NA,
                            'acc.skill.rate'=NA,'acc.skill.dur'=NA)
  
  if(name == 1){name="Earrings"}
  else if(name == 2){name="Necklace"}
  else if(name == 3){name="Brooch"}
  else if(name == 4){name="Keychain"}
  else if(name == 5){name="Bracelet"}
  else if(name == 6){name="Hairpin"}
  else if(name == 7){name="Ribbon"}
  else if(name == 8){name="Pouch"}
  else if(name == 9){name="Wristband"}
  else if(name == 10){name="Towel"}
  else if(name == 11){name="Badge"}
  else if(name == 12){name="Santa Hat"}
  else {print("Invalid name. Please enter an integer between 1 and 12.")
    stop(call. = F)}
  
  if(element==1){element="Smile"}
  else if(element==2){element="Pure"}
  else if(element==3){element="Cool"}
  else if(element==4){element="Active"}
  else if(element==5){element="Natural"}
  else if(element==6){element="Elegant"}
  else {print("Invalid element. Please enter an integer between 1 and 6.")
    stop(call. = F)}
  
  if(rarity==1){rarity="\\(R\\)"}
  else if(rarity==2){rarity="(SR)"}
  else if(rarity==3){rarity="(UR)"}
  else {print("Invalid rarity. Please enter an integer between 1 and 3.")
    stop(call. = F)}
  
  if(!(uncaps %in% seq(0,5))){
    print("Invalid uncap count. Please enter an integer between 0 and 5.")
  }
  
  row.data <- accessory.table %>% filter(str_detect(accessory.name,name)) %>%
    filter(accessory.element==element) %>%
    filter(str_detect(accessory.name,rarity)) %>%
    filter(accessory.uncaps==uncaps)
  if(nrow(row.data)==0){
    print("This accessory is not available for this element.")
    stop(call. = F)
  }
  
  #Rarities were set to have the () because that's how their name is written in the accsessory table.
  #(The accessory table has no column for rarity, so we use the name to detect it.)
  #Since the user will see this, we want to switch it back to something nicer looking without the
  #parenthesis before adding it.
  if(rarity=="\\(R\\)"){rarity="R"}
  else if(rarity=="(SR)"){rarity="SR"}
  else if(rarity=="(UR)"){rarity="UR"}
  
  skill.type <- row.data$accessory.skill.type
  
  to.add$acc.num <- row.data$accessory.num
  to.add$acc.name <- row.data$accessory.name
  to.add$acc.rarity <- rarity
  to.add$acc.element <- element
  to.add$acc.uncaps <- uncaps
  to.add$acc.appeal <- row.data$accessory.appeal
  to.add$acc.stam <- row.data$accessory.stam
  to.add$acc.tech <- row.data$accessory.tech
  to.add$acc.skill.lvl <- skill
  to.add$acc.skill <- row.data$accessory.skill
  to.add$acc.skill.val <- accessory.skill.table %>% filter(accessory.skill.type==skill.type) %>%
    filter(accessory.sl==skill) %>%
    use_series(accessory.vals)
  to.add$acc.skill.cond <- row.data$accessory.cond
  to.add$acc.skill.rate <- row.data$accessory.rate
  to.add$acc.skill.dur <- row.data$accessory.dur
  
  if(is.na(my_accessories$acc.num[1])) {
    my_accessories <<- to.add
  }
  else {
    my_accessories <<- rbind(my_accessories,to.add) 
    #Removing the sorting because it'll break the way I keep track of adding accessories onto my team.
    #%>% arrange(desc(acc.appeal))
  }
  
  save_accessories()
}

#1, 2, or 3 for R, SR, or UR respectively.
#Badges and Santa hats do not have SRs or URs.
#By default, changing the rarity will reset the uncaps to 0.
edit.rarity <- function(row.num,new.rare,uncaps=0) {
  check_rows(row.num)
  
  name <- my_accessories[row.num,'acc.name'] %>% str_remove("\\(S?U?R{1}\\)")
  
  if(new.rare==1){new.rare="\\(R\\)"}
  else if(new.rare==2){new.rare="\\(SR\\)"}
  else if(new.rare==3){new.rare="\\(UR\\)"}
  else {print("Invalid rarity. Please enter an integer between 1 and 3.")
    stop(call. = F)}
  
  new.data <- accessory.table %>% 
    filter(str_detect(accessory.name,name)) %>%
    filter(accessory.element == as.character(my_accessories[row.num,'acc.element'])) %>%
    filter(str_detect(accessory.name,new.rare)) %>%
    filter(accessory.uncaps==uncaps)

  new.vals <- accessory.skill.table %>%
    filter(accessory.skill.type==new.data$accessory.skill.type) %>%
    filter(accessory.sl==as.integer(my_accessories[row.num,'acc.skill.lvl'])) %>%
    use_series(accessory.vals)
  
  if(is.na(new.vals[1])){
    print("The skill level is too high to lower the rarity of this accesory.")
    stop(call. = F)
  }
  
  if(new.rare=="\\(R\\)"){new.rare="R"}
  else if(new.rare=="\\(SR\\)"){new.rare="SR"}
  else if(new.rare=="\\(UR\\)"){new.rare="UR"}
  
  my_accessories$acc.rarity[row.num] <<- new.rare
  my_accessories$acc.num[row.num] <<- new.data$accessory.num
  my_accessories$acc.name[row.num] <<- new.data$accessory.name
  my_accessories$acc.uncaps[row.num] <<- new.data$accessory.uncaps
  my_accessories$acc.appeal[row.num] <<- new.data$accessory.appeal
  my_accessories$acc.stam[row.num] <<- new.data$accessory.stam
  my_accessories$acc.tech[row.num] <<- new.data$accessory.tech
  my_accessories$acc.skill.val[row.num] <<- new.vals
  
  #Sorting them is neat, but messes things up on the team.
  #my_accessories <<- my_accessories %>% arrange(desc(acc.appeal))
  
  if(exists('accessories.on.team')){
    if(row.num %in% accessories.on.team){
      pos <- which(accessories.on.team==row.num)
      my_team$acc.num[pos] <<- new.data$accessory.num
      my_team$acc.name[pos] <<- new.data$accessory.name
      my_team$acc.appeal[pos] <<- new.data$accessory.appeal
      my_team$acc.stam[pos] <<- new.data$accessory.stam
      my_team$acc.tech[pos] <<- new.data$accessory.tech
      my_team$acc.skill.val[pos] <<- new.vals
    }
  }
  
  save_accessories()
}

edit.skill.lvl <- function(row.num,new.lvl) {
  check_rows(row.num)
  
  num <- my_accessories[row.num,'acc.num'] %>% as.integer()
  skill.type.num <- accessory.table[num,'accessory.skill.type'] %>% as.integer()
  new.val <- accessory.skill.table %>% filter(accessory.skill.type==skill.type.num) %>%
    filter(accessory.sl == new.lvl) %>%
    use_series('accessory.vals')
  
  if(length(new.val)==0){
    print("This rarity does not contain that skill level")
    stop(call. = F)
  }
  
  my_accessories[row.num,'acc.skill.lvl'] <<- new.lvl
  my_accessories[row.num,'acc.skill.val'] <<- new.val
  
  if(exists('accessories.on.team')){
    if(row.num %in% accessories.on.team){
      pos <- which(accessories.on.team==row.num)
      my_team$acc.skill.val[pos] <<- new.val
    }
  }
  
  save_accessories()
}

edit.acc.uncap <- function(row.num,new.uncap) {
  check_rows(row.num)
  
  full.name <- my_accessories[row.num,'acc.name']
  name <- full.name %>% str_remove("\\(S?U?R{1}\\)")
  if(str_detect(full.name,'\\(R\\)')){rare="\\R\\)"}
  else if(str_detect(full.name,'SR')){rare="SR"}
  else{rare="UR"}
  
  ele <- as.character(my_accessories[row.num,'acc.element'])
  
  new.row <- accessory.table %>%
    filter(str_detect(accessory.name,name)) %>%
    filter(str_detect(accessory.name,rare)) %>%
    filter(accessory.element==ele) %>%
    filter(accessory.uncaps==new.uncap)
  
  if(nrow(new.row)==0){
    print("Invalid uncap number entered. Please enter an integer between 0 and 5")
  }

  my_accessories[row.num,'acc.num'] <<- new.row$accessory.num
  my_accessories[row.num,'acc.name'] <<- new.row$accessory.name
  my_accessories[row.num,'acc.uncaps'] <<- new.uncap
  my_accessories[row.num,'acc.appeal'] <<- new.row$accessory.appeal
  my_accessories[row.num,'acc.stam'] <<- new.row$accessory.stam
  my_accessories[row.num,'acc.tech'] <<- new.row$accessory.tech
  
  my_accessories <<- my_accessories %>% arrange(desc(acc.appeal))
  
  #Update the increased stats from uncapping on your team.
  if(exists('accessories.on.team')){
    if(row.num %in% accessories.on.team){
      pos <- which(accessories.on.team==row.num)
      my_team$acc.appeal[pos] <<- new.row$accessory.appeal
      my_team$acc.stam[pos] <<- new.row$accessory.stam
      my_team$acc.tech[pos] <<- new.row$accessory.tech
    }
  }
  
  save_accessories()
}


#Since your accessories may not be unique, the user will need to look at their my_accessories table
#and provide the row number for the accessory that they wish to remove.
remove.accessory <- function(row.num) {
  check_rows(row.num)
  
  my_accessories <<- my_accessories[-c(row.num),]
  
  #Removing an accessory can mess up the list of which accessories are on your team.
  if(exists('accessories.on.team')){
    for(i in 1:length(accessories.on.team)){
      if(row.num < accessories.on.team[i]){
        accessories.on.team[i] <<- accessories.on.team[i] - 1
      }
      else if(row.num==accessories.on.team[i]){
        team.remove.accessory(i)
      }
    }
  }
  
  save_accessories()
}

check_rows <- function(row.num){
  if(!(row.num %in% seq(1,nrow(my_accessories)))) {
    print("Accessory row number out of bounds.")
    stop(call. = F)
  }
}
