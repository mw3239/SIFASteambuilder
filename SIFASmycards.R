load_my_cards <- function() {
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  
  my_cards <<- dbGetQuery(db,'SELECT * FROM my_cards')
  my_bonds <<- dbGetQuery(db,'SELECT * FROM my_bonds')
  bond <<- dbGetQuery(db,'SELECT * FROM bond')
  inspi.skills <<- dbGetQuery(db,'SELECT * FROM inspi_skills')
  
  dbDisconnect(db)
}

save_cards <- function() {
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  dbWriteTable(db,"my_cards",my_cards,overwrite=T)
  dbDisconnect(db)
}

init_my_cards <- function() {
  my_cards <<- tibble('card.num' = NA,'card.name' = NA,'rarity' = NA,
                     'type' = NA, 'element' = NA, 'idol' = NA, 'uncaps' = NA,
                     'appeal' = NA, 'stamina' = NA, 'technique' = NA,
                     'live.effect' = NA, 'live.target' = NA, 'live.val' = NA,
                     'live.param' = NA, 'live.length' = NA, 'pass.target' = NA,
                     'pass.stat' = NA, 'pass.val' = NA,'activ.cond' = NA, 'activ.chance' = NA,
                     'activ.effect' = NA, 'activ.value' = NA, 'activ.stat' = NA,
                     'activ.dur' = NA, 'activ.target' = NA, 'insp.slots' = NA,
                     'insp1' = "", 'insp2' = "", 'insp3' = "", 'insp4' = "")
}

#insp = inspiration skill.
#If bulk adding, set save=T only on the final card to speed up the process.
add.card <- function(card.num,uncaps=0,insp1=0,insp2=0,insp3=0,insp4=0,save=F) {
  if(!exists('my_cards') || nrow(my_cards) == 0) {
    init_my_cards()
  }
  
  if(!exists("card_table") || !exists("my_bonds") || !exists("bond") || !exists("inspi.skills")){
    load_my_cards() 
  }
  
  if(!(card.num %in% seq(1,nrow(card_table)))){
    print("Invalid card number entered.")
    stop(call. = F)
  }
  
  if(check_dupe(card.num)){
    print("This card has already been added. If you would like to change its uncap count please use the 'edit_uncap' function")
    stop(call. = F)
  }
  
  
  to.add <- tibble('card.num' = NA,'card.name' = NA,'rarity' = NA,
                     'type' = NA, 'element' = NA, 'idol' = NA, 'uncaps' = NA,
                     'appeal' = NA, 'stamina' = NA, 'technique' = NA,
                     'live.effect' = NA, 'live.target' = NA, 'live.val' = NA,
                     'live.param' = NA, 'live.length' = NA, 'pass.target' = NA,
                     'pass.stat' = NA, 'pass.val' = NA,'activ.cond' = NA, 'activ.chance' = NA,
                     'activ.effect' = NA, 'activ.value' = NA, 'activ.stat' = NA,
                     'activ.dur' = NA, 'activ.target' = NA, 'insp.slots' = NA,
                     'insp1' = "", 'insp2' = "", 'insp3' = "", 'insp4' = "")
  
  data <- card_table %>% filter(number == card.num)
  
  to.add$card.num <- card.num %>% as.integer()
  to.add$card.name <- data %>% select(card.name) %>% magrittr::extract(1) %>% as.character()
  to.add$rarity <- data %>% select(rarity) %>% magrittr::extract(1) %>% as.character()
  to.add$type <- data %>% select(type) %>% magrittr::extract(1) %>% as.character()
  to.add$element <- data %>% select(element) %>% magrittr::extract(1) %>% as.character()
  to.add$idol <- data %>% select(idol) %>% magrittr::extract(1) %>% as.character()
  to.add$uncaps <- uncaps %>% as.integer()
  if(uncaps <= 0) {
    to.add$appeal = data %>% select(appeal0) %>% magrittr::extract(1) %>% as.integer()
    to.add$stamina = data %>% select(stam0) %>% magrittr::extract(1) %>% as.integer()
    to.add$technique = data %>% select(tech0) %>% magrittr::extract(1) %>% as.integer()
    to.add$live.val = data %>% select(val3) %>% magrittr::extract(1) %>% as.numeric()
    to.add$pass.val = data %>% select('pass val3') %>% magrittr::extract(1) %>% as.numeric()
    if(to.add$rarity == 'UR(Fes)') {
      to.add$insp.slots = 3
    }
    else {
      to.add$insp.slots = 2
    }
  }
  else if(uncaps == 1) {
    to.add$appeal = data %>% select(appeal1) %>% magrittr::extract(1) %>% as.integer()
    to.add$stamina = data %>% select(stam1) %>% magrittr::extract(1) %>% as.integer()
    to.add$technique = data %>% select(tech1) %>% magrittr::extract(1) %>% as.integer()
    to.add$live.val = data %>% select(val3) %>% magrittr::extract(1) %>% as.numeric()
    if(to.add$rarity == 'UR' || to.add$rarity == 'UR(Fes)') {
      to.add$pass.val = data %>% select('pass val4') %>% magrittr::extract(1) %>% as.numeric()
    }
    else {
      to.add$pass.val = data %>% select('pass val3') %>% magrittr::extract(1) %>% as.numeric()
    }
    if(to.add$rarity == 'UR(Fes)') {
      to.add$insp.slots = 3
    }
    else {
      to.add$insp.slots = 2
    }
  }
  else if(uncaps == 2) {
    to.add$appeal = data %>% select(appeal2) %>% magrittr::extract(1) %>% as.integer()
    to.add$stamina = data %>% select(stam2) %>% magrittr::extract(1) %>% as.integer()
    to.add$technique = data %>% select(tech2) %>% magrittr::extract(1) %>% as.integer()
    to.add$live.val = data %>% select(val3) %>% magrittr::extract(1) %>% as.numeric()
    if(to.add$rarity == 'UR' || to.add$rarity == 'UR(Fes)') {
      to.add$pass.val = data %>% select('pass val4') %>% magrittr::extract(1) %>% as.numeric()
    }
    else {
      to.add$pass.val = data %>% select('pass val3') %>% magrittr::extract(1) %>% as.numeric()
    }
    if(to.add$rarity == 'UR(Fes)') {
      to.add$insp.slots = 3
    }
    else {
      to.add$insp.slots = 2
    }
  }
  else if(uncaps == 3) {
    to.add$appeal = data %>% select(appeal3) %>% magrittr::extract(1) %>% as.integer()
    to.add$stamina = data %>% select(stam3) %>% magrittr::extract(1) %>% as.integer()
    to.add$technique = data %>% select(tech3) %>% magrittr::extract(1) %>% as.integer()
    to.add$live.val = data %>% select(val4) %>% magrittr::extract(1) %>% as.numeric()
    if(to.add$rarity != 'R') {
      to.add$pass.val = data %>% select('pass val4') %>% magrittr::extract(1) %>% as.numeric()
    }
    else {
      to.add$pass.val = data %>% select('pass val3') %>% magrittr::extract(1) %>% as.numeric()
    }
    if(to.add$rarity == 'UR(Fes)') {
      to.add$insp.slots = 3
    }
    else {
      to.add$insp.slots = 2
    }
  }
  else if(uncaps == 4) {
    to.add$appeal = data %>% select(appeal4) %>% magrittr::extract(1) %>% as.integer()
    to.add$stamina = data %>% select(stam4) %>% magrittr::extract(1) %>% as.integer()
    to.add$technique = data %>% select(tech4) %>% magrittr::extract(1) %>% as.integer()
    to.add$live.val = data %>% select(val4) %>% magrittr::extract(1) %>% as.numeric()
    to.add$pass.val = data %>% select('pass val4') %>% magrittr::extract(1) %>% as.numeric()
    if(to.add$rarity == 'UR(Fes)') {
      to.add$insp.slots = 4
    }
    else if(to.add$rarity == 'R'){
      to.add$insp.slots = 2
    }
    else {
      to.add$insp.slots = 3
    }
  }
  else if(uncaps >= 5) {
    to.add$appeal = data %>% select(appeal5) %>% magrittr::extract(1) %>% as.integer()
    to.add$stamina = data %>% select(stam5) %>% magrittr::extract(1) %>% as.integer()
    to.add$technique = data %>% select(tech5) %>% magrittr::extract(1) %>% as.integer()
    to.add$live.val = data %>% select(val5) %>% magrittr::extract(1) %>% as.numeric()
    to.add$pass.val = data %>% select('pass val5') %>% magrittr::extract(1) %>% as.numeric()
    if(to.add$rarity == 'UR(Fes)') {
      to.add$insp.slots = 4
    }
    else if(to.add$rarity == 'R'){
      to.add$insp.slots = 2
    }
    else {
      to.add$insp.slots = 3
    }
  }
  to.add$live.effect = data %>% select(effect) %>% magrittr::extract(1) %>% as.character()
  to.add$live.target = data %>% select(target) %>% magrittr::extract(1) %>% as.character()
  to.add$live.param = data %>% select(param) %>% magrittr::extract(1) %>% as.character()
  to.add$live.length = data %>% select(length) %>% magrittr::extract(1) %>% as.character()
  to.add$pass.target = data %>% select('passive target') %>% magrittr::extract(1) %>% as.character()
  to.add$pass.stat = data %>% select('passive stat') %>% magrittr::extract(1) %>% as.character()
  to.add$activ.cond = data %>% select('active condition') %>% magrittr::extract(1) %>% as.character()
  to.add$activ.chance = data %>% select('active chance') %>% magrittr::extract(1) %>% as.character()
  to.add$activ.effect = data %>% select('active effect') %>% magrittr::extract(1) %>% as.character()
  to.add$activ.value = data %>% select('active value') %>% magrittr::extract(1) %>% as.character()
  to.add$activ.stat = data %>% select('active stat') %>% magrittr::extract(1) %>% as.character()
  to.add$activ.dur = data %>% select('active duration') %>% magrittr::extract(1) %>% as.character()
  to.add$activ.target = data %>% select('active target') %>% magrittr::extract(1) %>% as.character()

  #Adds the bond bonus to the stats.
  bond_lvl <- my_bonds %>% filter(idols == as.character(to.add$idol[1])) %>% select(current_bond_levels) %>% as.numeric()
  bonus <- bond %>% filter(level == bond_lvl) %>% select(bonus) %>% as.numeric()
  
  to.add$appeal <- as.numeric(to.add$appeal[1])%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor() %>% as.integer()
  to.add$stamina <- as.numeric(to.add$stamina[1])%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor() %>% as.integer()
  to.add$technique <- as.numeric(to.add$technique[1])%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor() %>% as.integer()
  
  if(insp1 > 0 && insp1 <= 335){
    to.add$insp1 <- add.insp.skill(insp1)
  }
  if(insp2 > 0 && insp2 <= 335){
    to.add$insp2 <- add.insp.skill(insp2)
  }
  if(insp3 > 0 && insp3 <= 335 && to.add$insp.slots >= 3){
    to.add$insp3 <- add.insp.skill(insp3)
  }
  if(insp4 > 0 && insp4 <= 335 && to.add$insp.slots >= 4){
    to.add$insp4 <- add.insp.skill(insp4)
  }
  
  
  if(is.na(my_cards$card.num[1])) {
    my_cards <<- to.add
  }
  else {
    my_cards <<- rbind(my_cards,to.add) %>% arrange(card.num)
  }
  
  if(save){
    save_cards() 
  }
}


remove.card <- function(card.number) {
  check_owned(card.number)
  my_cards <<- my_cards %>% filter(card.num != card.number)
  save_cards()
}

edit.uncap <- function(card.num,uncaps) {
  check_owned(card.num)
  
  uncaps <- as.integer(uncaps)
  #Don't even bother if the uncap level didn't actually change.
  if(my_cards[which(my_cards$card.num == card.num),'uncaps'] == uncaps || uncaps < 0 || uncaps > 5){
    print("The card you entered either already has this many uncaps or the number of uncaps you entered is invalid.")
    stop(call. = F)
  }
  
  #Having card.num as a column name and a variable name was causing some issues.
  test <- card.num
  my_cards[which(my_cards$card.num == test),'uncaps'] <<- uncaps
  
  #These are for stat updates
  char <- my_cards %>% filter(card.num == test) %>% select(idol) %>% unlist() %>% as.character()
  level.t <- my_bonds[which(my_bonds$idols == char),'current_bond_levels'] %>% unlist() %>% as.integer()
  bonus <- bond[which(bond$level == level.t),'bonus'] %>% unlist() %>% as.numeric()
  
  #These are for skill updates
  rare <- card_table[which(card_table$number == card.num),'rarity'] %>% unlist() %>% as.character()
  
  if(uncaps == 0) {
    new.appeal <- card_table[which(card_table$number == card.num),'appeal0']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.stam <- card_table[which(card_table$number == card.num),'stam0']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.tech <- card_table[which(card_table$number == card.num),'tech0']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.live.val <- card_table[which(card_table$number == card.num),'val3'] %>% unlist() %>% as.numeric()
    new.pass.val <- card_table[which(card_table$number == card.num),'pass val3'] %>% unlist() %>% as.numeric()
    if(rare == 'UR(Fes)') {
      new.inslot = 3
      #Lowering the number of uncaps may change the number of available inspiration skill slots.
      #This removes any skills in slots that are no longer available.
      my_cards[which(my_cards$card.num == test),'insp4'] <<- ""
    }
    else {
      new.inslot = 2
      my_cards[which(my_cards$card.num == test),'insp3'] <<- ""
    }
  }
  else if(uncaps == 1) {
    new.appeal <- card_table[which(card_table$number == card.num),'appeal1']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.stam <- card_table[which(card_table$number == card.num),'stam1']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.tech <- card_table[which(card_table$number == card.num),'tech1']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.live.val <- card_table[which(card_table$number == card.num),'val3'] %>% unlist() %>% as.numeric()
    if(rare == 'UR' || rare == 'UR(Fes)') {
      new.pass.val <- card_table[which(card_table$number == card.num),'pass val4'] %>% unlist() %>% as.numeric()
    }
    else {
      new.pass.val <- card_table[which(card_table$number == card.num),'pass val3'] %>% unlist() %>% as.numeric()
    }
    if(rare == 'UR(Fes)') {
      new.inslot = 3
      my_cards[which(my_cards$card.num == test),'insp4'] <<- ""
    }
    else {
      new.inslot = 2
      my_cards[which(my_cards$card.num == test),'insp3'] <<- ""
    }
  }
  else if(uncaps == 2) {
    new.appeal <- card_table[which(card_table$number == card.num),'appeal2']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.stam <- card_table[which(card_table$number == card.num),'stam2']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.tech <- card_table[which(card_table$number == card.num),'tech2']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.live.val <- card_table[which(card_table$number == card.num),'val3'] %>% unlist() %>% as.numeric()
    if(rare == 'UR' || rare == 'UR(Fes)') {
      new.pass.val <- card_table[which(card_table$number == card.num),'pass val4'] %>% unlist() %>% as.numeric()
    }
    else {
      new.pass.val <- card_table[which(card_table$number == card.num),'pass val3'] %>% unlist() %>% as.numeric()
    }
    if(rare == 'UR(Fes)') {
      new.inslot = 3
      my_cards[which(my_cards$card.num == test),'insp4'] <<- ""
    }
    else {
      new.inslot = 2
      my_cards[which(my_cards$card.num == test),'insp3'] <<- ""
    }
  }
  else if(uncaps == 3) {
    new.appeal <- card_table[which(card_table$number == card.num),'appeal3']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.stam <- card_table[which(card_table$number == card.num),'stam3']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.tech <- card_table[which(card_table$number == card.num),'tech3']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.live.val <- card_table[which(card_table$number == card.num),'val4'] %>% unlist() %>% as.numeric()
    if(rare != 'R') {
      new.pass.val <- card_table[which(card_table$number == card.num),'pass val4'] %>% unlist() %>% as.numeric()
    }
    else {
      new.pass.val <- card_table[which(card_table$number == card.num),'pass val3'] %>% unlist() %>% as.numeric()
    }
    if(rare == 'UR(Fes)') {
      new.inslot = 3
      my_cards[which(my_cards$card.num == test),'insp4'] <<- ""
    }
    else {
      new.inslot = 2
      my_cards[which(my_cards$card.num == test),'insp3'] <<- ""
    }
  }
  else if(uncaps == 4) {
    new.appeal <- card_table[which(card_table$number == test),'appeal4']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.stam <- card_table[which(card_table$number == test),'stam4']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.tech <- card_table[which(card_table$number == test),'tech4']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.live.val <- card_table[which(card_table$number == card.num),'val4'] %>% unlist() %>% as.numeric()
    new.pass.val <- card_table[which(card_table$number == card.num),'pass val4'] %>% unlist() %>% as.numeric()
    if(rare == 'UR(Fes)') {
      new.inslot = 4
    }
    else if(rare == 'R'){
      new.inslot = 2
    }
    else {
      new.inslot = 3
    }
  }
  else {
    new.appeal <- card_table[which(card_table$number == card.num),'appeal5']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.stam <- card_table[which(card_table$number == card.num),'stam5']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.tech <- card_table[which(card_table$number == card.num),'tech5']%>% unlist() %>% as.integer() %>% multiply_by(bonus) %>% floor()
    new.live.val <- card_table[which(card_table$number == card.num),'val5'] %>% unlist() %>% as.numeric()
    new.pass.val <- card_table[which(card_table$number == card.num),'pass val5'] %>% unlist() %>% as.numeric()
    if(rare == 'UR(Fes)') {
      new.inslot = 4
    }
    else if(rare == 'R'){
      new.inslot = 2
    }
    else {
      new.inslot = 3
    }
  }
  
  my_cards[which(my_cards$card.num == test),'appeal'] <<- new.appeal
  my_cards[which(my_cards$card.num == test),'stamina'] <<- new.stam
  my_cards[which(my_cards$card.num == test),'technique'] <<- new.tech
  my_cards[which(my_cards$card.num == test),'live.val'] <<- new.live.val
  my_cards[which(my_cards$card.num == test),'pass.val'] <<- new.pass.val
  my_cards[which(my_cards$card.num == test),'insp.slots'] <<- new.inslot
  
  #Apply the changes to any cards currently on your team.
  if(test %in% my_team$card.num){
    my_team$base.appeal[which(my_team$card.num==test)] <<- new.appeal
    my_team$base.stam[which(my_team$card.num==test)] <<- new.stam
    my_team$base.tech[which(my_team$card.num==test)] <<- new.tech
    my_team$active.val[which(my_team$card.num==test)] <<- new.live.val
    my_team$pass.val[which(my_team$card.num==test)] <<- new.pass.val
    
    apply.passive()
    get.calc.stat()
  }
  
  save_cards()
}

#Checks to see if a card that's trying to be added is alredy part of my_cards.
check_dupe <- function(card.number) {
  ifelse(nrow(filter(my_cards,card.num==card.number)) == 1, return(T), return(F))
}

check_owned <- function(card.number) {
  if(!(card.number %in% my_cards$card.num)){
    print("This card has not been added to your cards yet.")
    stop(call. = F)
  }
}

#This is a helper function used by the add.card (and add.guests) function. If you would like to add an inspiration skill
#onto a card that's already been added to your card list, please use the edit.inspi.skill function.
add.insp.skill <- function(insp){
  inspi.skills %>% filter(inspi.num == insp) %>% select(inspi.name) %>% as.character() %>% return()
}

#Take a card, choose a slot to insert the new skill (1-4), and then pick the number of the skill
#you wish to add.
#To remove a skill from an already occupied slot, set to.add to 0 (Or anything that's not an integer between 1 and 335, really.)
edit.inspi.skill <- function(card,slot,to.add=0) {
  
  check_owned(card)
  
  #Checks if the inspiration skill slot that's attempted to be editted is valid for the card's
  #rarity and uncap level.
  rare <- my_cards[which(my_cards$card.num==card),'rarity'] %>% as.character()
  uncap <- my_cards[which(my_cards$card.num==card),'uncaps'] %>% as.character()
  if((rare == 'UR(Fes)' && uncap %in% seq(0,3)) || (rare %in% c('UR','SR') && uncap %in% c(4,5))) {
    max <- 3
  }
  else if(rare == 'UR(Fes)' && uncap %in% c(4,5)) {
    max <- 4
  }
  else {
    max <- 2
  }
  if(max < slot){
    print("Not enough inspiration slots available at this rarity and uncap level.")
    stop(call. = F)
  }
  
  #Removes the inspiration skill if an invalid number is entered.
  #Also updates the inspiration skill if the card is on your team.
  if(slot == 1){
    tryCatch({
      my_cards[which(my_cards$card.num==card),'insp1'] <<- inspi.skills[which(inspi.skills$inspi.num==to.add),'inspi.name'] %>% as.character()
      if(card %in% my_team$card.num) {
        my_team[which(my_team$card.num==card),'inspi1'] <<- inspi.skills[which(inspi.skills$inspi.num==to.add),'inspi.name'] %>% as.character()
      }
    },
       error = function(e){my_cards[which(my_cards$card.num==card),'insp1'] <<- ""})
  }
  else if(slot == 2){
    tryCatch({
      my_cards[which(my_cards$card.num==card),'insp2'] <<- inspi.skills[which(inspi.skills$inspi.num==to.add),'inspi.name'] %>% as.character()
      if(card %in% my_team$card.num) {
        my_team[which(my_team$card.num==card),'inspi2'] <<- inspi.skills[which(inspi.skills$inspi.num==to.add),'inspi.name'] %>% as.character()
      }
    },
      error = function(e){my_cards[which(my_cards$card.num==card),'insp2'] <<- ""})
  }
  else if(slot == 3){
    tryCatch({
      my_cards[which(my_cards$card.num==card),'insp3'] <<- inspi.skills[which(inspi.skills$inspi.num==to.add),'inspi.name'] %>% as.character()
      if(card %in% my_team$card.num) {
        my_team[which(my_team$card.num==card),'inspi3'] <<- inspi.skills[which(inspi.skills$inspi.num==to.add),'inspi.name'] %>% as.character()
      }
    },
      error = function(e){my_cards[which(my_cards$card.num==card),'insp3'] <<- ""})
  }
  else{
    tryCatch({
      my_cards[which(my_cards$card.num==card),'insp4'] <<- inspi.skills[which(inspi.skills$inspi.num==to.add),'inspi.name'] %>% as.character()
      if(card %in% my_team$card.num) {
        my_team[which(my_team$card.num==card),'inspi4'] <<- inspi.skills[which(inspi.skills$inspi.num==to.add),'inspi.name'] %>% as.character()
      }
    },
    error = function(e){my_cards[which(my_cards$card.num==card),'insp4'] <<- ""})
  }
  
  #Applies stat changes to my_team
  apply.insp()
  get.calc.stat()
  
  save_cards()
}
