load_guests <- function() {
  db = dbConnect(SQLite(), dbname="C:/Users/Mike/Documents/R/SIFASteambuilder/SIFAS.sqlite")
  my_guests <<- dbGetQuery(db,'SELECT * FROM my_guests')
  dbDisconnect(db)
}

save_guests <- function() {
  db = dbConnect(SQLite(), dbname="C:/Users/Mike/Documents/R/SIFASteambuilder/SIFAS.sqlite")
  dbWriteTable(db,"my_guests",my_guests,overwrite=T)
  dbDisconnect(db)
}

init_my_guests <- function() {
  my_guests <<- tibble('guest.name'="",'card.num' = NA,'card.name' = NA,'rarity'=NA,'uncaps' = NA,
                      'pass.target' = NA, 'pass.stat' = NA, 'pass.val' = NA,
                      'live.cond' = NA, 'live.chance' = NA,
                      'live.effect' = NA, 'live.val' = NA, 'live.stat' = NA,
                      'live.dur' = NA, 'live.target' = NA, 'insp.slots' = NA,
                      'insp1' = NA, 'insp2' = NA, 'insp3' = NA, 'insp4' = NA)
}

add_guests <- function(card.num,uncaps=0,insp1=0,insp2=0,insp3=0,insp4=0,name="",save=F) {
  if(!exists('my_guests') || nrow(my_guests) == 0) {
    init_my_guests()
  }
  
  to.add <<- tibble('guest.name'="",'card.num' = NA,'card.name' = NA,'rarity'=NA,'uncaps' = NA,
                       'pass.target' = NA, 'pass.stat' = NA, 'pass.val' = NA,
                       'live.cond' = NA, 'live.chance' = NA,
                       'live.effect' = NA, 'live.val' = NA, 'live.stat' = NA,
                       'live.dur' = NA, 'live.target' = NA, 'insp.slots' = NA,
                       'insp1' = NA, 'insp2' = NA, 'insp3' = NA, 'insp4' = NA)
  
  row.data <- card_table[card.num,]
  
  to.add$guest.name <- name
  to.add$card.num <- row.data$number
  to.add$card.name <- row.data$card.name
  to.add$rarity <- row.data$rarity
  to.add$uncaps <- uncaps
  to.add$pass.target <- row.data$'passive target'
  to.add$pass.stat <- row.data$'passive stat'
  to.add$live.cond <- row.data$'active condition'
  to.add$live.chance <- row.data$'active chance'
  to.add$live.effect <- row.data$'active effect'
  to.add$live.val <- row.data$'active value'
  to.add$live.stat <- row.data$'active stat'
  to.add$live.dur <- row.data$'active duration'
  to.add$live.target <- row.data$'active target'
  to.add$insp1 <- NA
  to.add$insp2 <- NA
  to.add$insp3 <- NA
  to.add$insp4 <- NA
  
  
  if(uncaps == 0){
    to.add$pass.val <- row.data$'pass val3'
    if(to.add$rarity == 'UR(Fes)'){
      to.add$insp.slots <- 3
    }
    else {
      to.add$insp.slots <- 2
    }
  }
  else if(uncaps == 1){
    if(to.add$rarity == 'UR' || to.add$rarity == 'UR(Fes)') {
      to.add$pass.val <- row.data$'pass val4'
    }
    else{to.add$pass.val <- row.data$'pass val3'}
    if(to.add$rarity == 'UR(Fes)'){
      to.add$insp.slots <- 3
    }
    else {
      to.add$insp.slots <- 2
    }
  }
  else if(uncaps == 2){
    if(to.add$rarity == 'UR' || to.add$rarity == 'UR(Fes)') {
      to.add$pass.val <- row.data$'pass val4'
    }
    else{to.add$pass.val <- row.data$'pass val3'}
    if(to.add$rarity == 'UR(Fes)'){
      to.add$insp.slots <- 3
    }
    else {
      to.add$insp.slots <- 2
    }
  }
  else if(uncaps == 3){
    if(to.add$rarity != 'R') {
      to.add$pass.val <- row.data$'pass val4'
    }
    else{to.add$pass.val <- row.data$'pass val3'}
    if(to.add$rarity == 'UR(Fes)'){
      to.add$insp.slots <- 3
    }
    else {
      to.add$insp.slots <- 2
    }
  }
  else if(uncaps == 4){
    to.add$pass.val <- row.data$'pass val4'
    if(to.add$rarity == 'UR(Fes)'){
      to.add$insp.slots <- 4
    }
    else if(to.add$rarity == 'R'){
      to.add$insp.slots <- 2
    }
    else{to.add$insp.slots <- 3}
  }
  else if(uncaps == 5){
    to.add$pass.val <- row.data$'pass val5'
    if(to.add$rarity == 'UR(Fes)'){
      to.add$insp.slots <- 4
    }
    else if(to.add$rarity == 'R'){
      to.add$insp.slots <- 2
    }
    else{to.add$insp.slots <- 3}
  }
  else{
    print("Error in either uncap count or inspiration skills.")
    stop(call. = F)
  }
  
  #The add.insp.skill function is from the SIFASmycards.file
  if(insp1 %in% seq(1,335)){
    to.add$insp1 <- add.insp.skill(insp1)
  }
  if(insp1 %in% seq(1,335)){
    to.add$insp2 <- add.insp.skill(insp2)
  }
  if(insp1 %in% seq(1,335) && to.add$insp.slots >= 3){
    to.add$insp3 <- add.insp.skill(insp3)
  }
  if(insp1 %in% seq(1,335) && to.add$insp.slots >= 4){
    to.add$insp4 <- add.insp.skill(insp4)
  }
  
  if(is.na(my_guests$card.num[1])) {
    my_guests <<- to.add
  }
  else {
    my_guests <<- rbind(my_guests,to.add) %>% arrange(card.num)
  }
  
  if(save){
    save_guests()
  }
}

remove_guest <- function(row.num) {
  check_rows_guest(row.num)
  
  my_guests <<- my_guests[-c(row.num),]
  
  save_guests()
}

check_rows_guest <- function(row.num) {
  if(!(row.num %in% seq(1,nrow(my_guests)))) {
    print("Guest row number out of bounds.")
    stop(call. = F)
  }
}

edit_uncap_guest <- function(row.num,uncaps){
  check_rows_guest(row.num)
  
  
  row.data <- card_table[my_guests$card.num[1],]
  
  if(uncaps == 0){
    my_guests$pass.val[row.num] <<- row.data$'pass val3'
    if(my_guests$rarity[row.num] == 'UR(Fes)'){
      my_guests$insp.slots[row.num] <<- 3
      my_guests$insp4[row.num] <<- NA
    }
    else {
      my_guests$insp.slots[row.num] <<- 2
      my_guests$insp3[row.num] <<- NA
    }
  }
  else if(uncaps == 1){
    if(my_guests$rarity[row.num] == 'UR' || my_guests$rarity[row.num] == 'UR(Fes)') {
      my_guests$pass.val[row.num] <<- row.data$'pass val4'
    }
    else{my_guests$pass.val[row.num] <<- row.data$'pass val3'}
    if(my_guests$rarity[row.num] == 'UR(Fes)'){
      my_guests$insp.slots[row.num] <<- 3
      my_guests$insp4[row.num] <<- NA
    }
    else {
      my_guests$insp.slots[row.num] <<- 2
      my_guests$insp3[row.num] <<- NA
    }
  }
  else if(uncaps == 2){
    if(my_guests$rarity[row.num] == 'UR' || my_guests$rarity[row.num] == 'UR(Fes)') {
      my_guests$pass.val[row.num] <<- row.data$'pass val4'
    }
    else{my_guests$pass.val[row.num] <<- row.data$'pass val3'}
    if(my_guests$rarity[row.num] == 'UR(Fes)'){
      my_guests$insp.slots[row.num] <<- 3
      my_guests$insp4[row.num] <<- NA
    }
    else {
      my_guests$insp.slots[row.num] <<- 2
      my_guests$insp3[row.num] <<- NA
    }
  }
  else if(uncaps == 3){
    if(my_guests$rarity[row.num] != 'R') {
      my_guests$pass.val[row.num] <<- row.data$'pass val4'
    }
    else{my_guests$pass.val[row.num] <<- row.data$'pass val3'}
    if(my_guests$rarity[row.num] == 'UR(Fes)'){
      my_guests$insp.slots[row.num] <<- 3
      my_guests$insp4[row.num] <<- NA
    }
    else {
      my_guests$insp.slots[row.num] <<- 2
      my_guests$insp3[row.num] <<- NA
    }
  }
  else if(uncaps == 4){
    my_guests$pass.val[row.num] <<- row.data$'pass val4'
    if(my_guests$rarity[row.num] == 'UR(Fes)'){
      my_guests$insp.slots[row.num] <<- 4
      my_guests$insp4[row.num] <<- NA
    }
    else if(my_guests$rarity[row.num] == 'R'){
      my_guests$insp.slots[row.num] <<- 2
    }
    else{my_guests$insp.slots[row.num] <<- 3}
  }
  else if(uncaps == 5){
    my_guests$pass.val[row.num] <<- row.data$'pass val5'
    if(my_guests$rarity[row.num] == 'UR(Fes)'){
      my_guests$insp.slots[row.num] <<- 4
    }
    else if(my_guests$rarity[row.num] == 'R'){
      my_guests$insp.slots[row.num] <<- 2
    }
    else{my_guests$insp.slots[row.num] <<- 3}
  }
  else{
    print("Error in uncap value. Enter an integer between 0 and 5.")
    stop(call. = F)
  }
  
  my_guests$uncaps[row.num] <<- uncaps
  
  #Changing the uncaps may affect the passive level of the guest on my_team
  guest.passive.apply()
  
  save_guests
}

edit_inspi_guest <- function(row.num,slot,to.add=0) {
  check_rows_guest(row.num)
  
  rare <- my_guests$rarity[row.num]
  uncap <- my_guests$uncaps[row.num]
  
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
  
  if(slot == 1){
    tryCatch({my_guests$insp1[row.num] <<- inspi.skills$inspi.name[to.add]},
    error = function(e){my_guests$insp1[row.num] <<- NA})
  }
  else if(slot == 2){
    tryCatch({my_guests$insp2[row.num] <<- inspi.skills$inspi.name[to.add]},
             error = function(e){my_guests$insp2[row.num] <<- NA})
  }
  else if(slot == 3){
    tryCatch({my_guests$insp3[row.num] <<- inspi.skills$inspi.name[to.add]},
             error = function(e){my_guests$insp3[row.num] <<- NA})
  }
  else if(slot == 4){
    tryCatch({my_guests$insp4[row.num] <<- inspi.skills$inspi.name[to.add]},
             error = function(e){my_guests$insp4[row.num] <<- NA})
  }
  
  #Reapplies the guest inspiration skills on your team.
  guest.insp.apply()
  
  save_guests()
}

add_guests(36,5,5,328,319,0,"ふぃあ")
add_guests(36,3,316,316,0,0,"KOHJU")
add_guests(182,4,322,322,313,313,"ほのほの")
add_guests(146,5,313,322,14,310,"ほのほの")
add_guests(146,5,313,322,310,310,"toki")
add_guests(146,5,322,316,319,328,"yoppe-DC2")
add_guests(146,4,322,322,316,23,"もも")
add_guests(146,4,322,23,319,319,"Yule")
add_guests(146,5,322,310,319,17,"ふぃあ")
add_guests(112,5,322,328,328,0,"なお")
add_guests(112,5,322,328,319,0,"ケンタ")
add_guests(112,5,328,328,328,0,"夢見草")
add_guests(47,5,316,319,316,0,"丸条 玢奈")
add_guests(72,2,322,322,0,0,"なびちゃ")
add_guests(139,4,322,316,313,0,"yoppe-DC2")
add_guests(152,5,328,319,319,0,"toki")
add_guests(152,0,323,312,0,0,"忍")
add_guests(181,2,319,319,319,0,"ｲﾇﾁｬﾝ")
add_guests(181,1,322,322,322,0,"まるこめ")
add_guests(181,1,316,328,328,0,"夢見草")
add_guests(218,0,210,0,0,0,"ヤマヨシ",save=T)

View(my_guests)
