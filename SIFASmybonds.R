load_bonds = function() {
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  my_bonds <<- dbGetQuery(db,'SELECT * FROM my_bonds')
  bond <<- dbGetQuery(db,'SELECT * FROM bond')
  if(!exists("my_cards")){
    my_cards <<- dbGetQuery(db,'SELECT * FROM my_cards')
  }
  if(!exists("my_team")){
    my_team <<- dbGetQuery(db,'SELECT * FROM my_team')
  }
  
  dbDisconnect(db)
}

save_bonds = function() {
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  dbWriteTable(db,"my_bonds",my_bonds,overwrite=T)
  dbWriteTable(db,'my_cards',my_cards,overwrite=T)
  dbDisconnect(db)
}

init.bond <- function() {
  idols <- unique(card_table$idol)
  current_bond_levels <- rep(1,27)
  
  my_bonds <<- tibble(idols,current_bond_levels)
}

#Simply run update_bond(character) if you wish to increase their bond level by 1.
#You can also manually enter in a bond level as the second argument.
#Alternatively, you can set it to increment the bond level by some amount, if you pull a UR and want to increase it by 3,
#for example.
update_bond <- function(character,new.bond=0,increase.by=F) {
  if(!exists('my_bonds') || !exists('my_cards') || !exists('my_team') || !exists('bond')){
    load_bonds()
  }
  
  #In case some smart ass enters a decimal
  new.bond <- as.integer(new.bond)
  if(new.bond == 0 && increase.by==F){
    new.bond <- my_bonds %>% filter(idols == character) %>% select(current_bond_levels) %>% unlist() %>% as.integer() %>% add(1)
    my_bonds[which(my_bonds$idols==character),'current_bond_levels'] <<- new.bond
  }
  else if (new.bond > 0 && new.bond <= 150 && increase.by == F){
    my_bonds[which(my_bonds$idols==character),'current_bond_levels'] <<- new.bond
  }
  else if(increase.by==T) {
    new.bond <- my_bonds %>% filter(idols == character) %>% select(current_bond_levels) %>% unlist() %>% as.integer() %>% add(new.bond)
    if(new.bond > 150) {
      print("Bond level can not exceed 150.")
      stop()
    }
    my_bonds[which(my_bonds$idols==character),'current_bond_levels'] <<- new.bond
  }
  else{
    print("Bond must be an integer between 1 and 150.")
    stop()
  }
  
  
  #Update card for my_cards
  new.bonus <- bond %>% filter(level == new.bond) %>% select(bonus) %>% unlist() %>% as.numeric()
  to.update <- my_cards %>% filter(idol == character)
  to.update.index <- to.update %>% select(card.num) %>% unlist()
  relevant.rows <- card_table %>% filter(number %in% to.update.index)
  
  if(nrow(relevant.rows) >= 1){
    for(i in 1:nrow(relevant.rows)) {
      if(to.update[i,'uncaps'] == 0) {
        new.appeal <- relevant.rows[i,'appeal0'] %>% multiply_by(new.bonus) %>% floor()
        new.stam <- relevant.rows[i,'stam0'] %>% multiply_by(new.bonus) %>% floor()
        new.tech <- relevant.rows[i,'tech0'] %>% multiply_by(new.bonus) %>% floor()
      }
      else if(to.update[i,'uncaps'] == 1) {
        new.appeal <- relevant.rows[i,'appeal1'] %>% multiply_by(new.bonus) %>% floor()
        new.stam <- relevant.rows[i,'stam1'] %>% multiply_by(new.bonus) %>% floor()
        new.tech <- relevant.rows[i,'tech1'] %>% multiply_by(new.bonus) %>% floor()
      }
      else if(to.update[i,'uncaps'] == 2) {
        new.appeal <- relevant.rows[i,'appeal2'] %>% multiply_by(new.bonus) %>% floor()
        new.stam <- relevant.rows[i,'stam2'] %>% multiply_by(new.bonus) %>% floor()
        new.tech <- relevant.rows[i,'tech2'] %>% multiply_by(new.bonus) %>% floor()
      }
      else if(to.update[i,'uncaps'] == 3) {
        new.appeal <- relevant.rows[i,'appeal3'] %>% multiply_by(new.bonus) %>% floor()
        new.stam <- relevant.rows[i,'stam3'] %>% multiply_by(new.bonus) %>% floor()
        new.tech <- relevant.rows[i,'tech3'] %>% multiply_by(new.bonus) %>% floor()
      }
      else if(to.update[i,'uncaps'] == 4) {
        new.appeal <- relevant.rows[i,'appeal4'] %>% multiply_by(new.bonus) %>% floor()
        new.stam <- relevant.rows[i,'stam4'] %>% multiply_by(new.bonus) %>% floor()
        new.tech <- relevant.rows[i,'tech4'] %>% multiply_by(new.bonus) %>% floor()
      }
      else {
        new.appeal <- relevant.rows[i,'appeal5'] %>% multiply_by(new.bonus) %>% floor()
        new.stam <- relevant.rows[i,'stam5'] %>% multiply_by(new.bonus) %>% floor()
        new.tech <- relevant.rows[i,'tech5'] %>% multiply_by(new.bonus) %>% floor()
      }
      
      my_cards[which(my_cards$card.num==to.update.index[i]),'appeal'] <<- new.appeal
      my_cards[which(my_cards$card.num==to.update.index[i]),'stamina'] <<- new.stam
      my_cards[which(my_cards$card.num==to.update.index[i]),'technique'] <<- new.tech
      
      #Also updates the card's base stats if it's on your team.
      #Why the unlist? I have no idea, but it's a list for some reason.
      #EDIT: Unlist is because card_table is a tibble. and it always returns a tibble until unlisted.
      if(relevant.rows$number[i] %in% my_team$card.num){
        my_team$base.appeal[which(my_team$card.num==relevant.rows$number[i])] <<- unlist(new.appeal)
        my_team$base.stam[which(my_team$card.num==relevant.rows$number[i])] <<- unlist(new.stam)
        my_team$base.tech[which(my_team$card.num==relevant.rows$number[i])] <<- unlist(new.tech)
        
        
      }
    }
  }
  #Updates the stats of your team.
  for(i in 1:9){
    set.base.stats(i) 
  }
  apply.passive()
  get.calc.stat()
  
  save_bonds()
}

update_bond('honoka',43)
update_bond('eli',62)
update_bond('kotori',45)
update_bond('umi',77)
update_bond('rin',37)
update_bond('maki',53)
update_bond('nozomi',40)
update_bond('hanayo',52)
update_bond('nico',36)
update_bond('chika',43)
update_bond('riko',43)
update_bond('kanan',47)
update_bond('dia',47)
update_bond('you',41)
update_bond('yoshiko',28)
update_bond('hanamaru',34)
update_bond('mari',44)
update_bond('ruby',41)
update_bond('ayumu',42)
update_bond('kasumi',40)
update_bond('shizuku',41)
update_bond('karin',32)
update_bond('ai',32)
update_bond('kanata',30)
update_bond('setsuna',31)
update_bond('emma',35)
update_bond('rina',52)

#