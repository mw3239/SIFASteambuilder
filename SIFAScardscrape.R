#The main method that builds the card database.
#Set rescrape=T on first time running (when there's no existing card database) or when
#new cards are added to the game.
add.cards <- function(rescrape=F){
  if(rescrape==T || !exists('cards')){
    scrape.page()
  }
  
  #Either load in a card table already present or initialize a new one.
  #counter is the number of cards already added+1. This is so that new cards
  #can be added starting from the newest one rather than rebuilding the whole
  #database from scratch.
  counter <- init_cards()
  
  init_skill_data(counter)
  get.uncap.data(counter)
  get.basic.info(counter)
  get.card.name(counter)
  get.rarity(counter)
  get.uncap0(counter)
  get.activ.skill(counter)
  get.pass.skill(counter)
  get.live.skill(counter)
  
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  dbWriteTable(db,"cards",card_table,overwrite=T)
  dbDisconnect(db)
}

#Run this only when there are new cards to avoid scraping kach's page every single
#time while debugging.
scrape.page <- function(){
  url.cards <- "http://kachagain.com/sifas/cards.php"
  
  #This grabs the basic information of each card as text.
  #Simple information will be retrieved with regexs.
  cards <<- read_html(url.cards,encoding='UTF-8') %>%
    html_nodes(xpath='//*[@id="cards"]') %>%
    html_nodes("div.card-table") %>%
    html_text()
  
  #Grabs the nodes that will later be used to grab uncapped stat
  #information. You need to press a button to access these on
  #the page - they're not shown by default, that's why it needs
  #a seperate way of obtaining the info.
  cards.uncap.nodes <<- read_html(url.cards) %>%
    html_nodes(xpath='//th')
  
  #Miscellaneous Information that can be retrieved easier than from the
  #cards element.
  card.extra.data <<- read_html(url.cards) %>%
    html_nodes(xpath='//*[@id="cards"]') %>%
    html_nodes("div.card-table") %>%
    html_attrs() %>%
    map_df(~ map_df(.x, ~ replace(.x, is.null(.x),NA)))
  #kach keeps adding more columns here. Some of these would've been nice to have when I first started this project, but oh well.
  colnames(card.extra.data) <<- c("class","Rarity","Type","Source","Element","Active","Passive","Group","Year","Subunit","Idol","Live")
}

init_cards <- function() {
  counter <- load_cards()
  
  if(counter == 1){
    card_table <<- tibble(number=NA,card.name=NA,rarity=NA,type=NA,element=NA,idol=NA,appeal0=NA,stam0=NA,tech0=NA,
                          effect=NA,val1=NA,val2=NA,val3=NA,val4=NA,val5=NA,target=NA,length=NA,param=NA,'passive target'=NA,'passive stat'=NA,
                          'pass val1'=NA,'pass val2'=NA,'pass val3'=NA,
                          'pass val4'=NA,'pass val5'=NA,'active condition'=NA,
                          'active chance'=NA,'active effect'=NA,'active value'=NA,
                          'active stat'=NA,'active duration'=NA,'active target'=NA,
                          appeal1=NA,stam1=NA,tech1=NA,appeal2=NA,stam2=NA,
                          tech2=NA,appeal3=NA,stam3=NA,tech3=NA,appeal4=NA,stam4=NA,tech4=NA,appeal5=NA,stam5=NA,tech5=NA) %>%
      uncount(length(cards))
  }
  else if(counter > length(cards)) {
    print("All cards already added.")
    stop(call. = F)
  }
  #Initializes new rows if new rows need to be added
  else{
    card_table[c(seq(counter,length(cards))),] <<- NA
  }
  #Counter was returning from load cards as a boolean for some reason and I had trouble
  #assigning it to my gloabl environment.
  return(as.integer(counter))
}

load_cards <- function(){
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  tryCatch({card_table <<- dbGetQuery(db,'SELECT * FROM cards')
  return(nrow(card_table)+1)},
  error = function(e){return(1)})
  dbDisconnect(db)
}

save_card_table <- function(){
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  dbWriteTable(db,"cards",card_table,overwrite=T)
  dbDisconnect(db)
}

init_skill_data <- function(counter){
  #Actually the active skills.
  card.skills <<- rep(NA,(length(cards)-counter+1))
  
  for(i in counter:length(cards)) {
    j <- i - counter + 1
    card.skills[j] <<- cards[i] %>%
      str_extract('Skill([:graph:]|[:blank:])+Individuality') %>%
      str_remove("Skill") %>%
      str_remove("Individuality")
  }
  
  passives <<- rep(NA,(length(cards)-counter+1))
  
  for (i in counter:length(cards)) {
    j <- i - counter + 1
    passives[j] <<- cards[i] %>%
      str_extract("Passive:([:graph:]|[:blank:])+%") %>% 
      str_remove("Active: ([:graph:]|[:blank:])+") %>%
      str_remove("Passive: ")  
  }
  
  #Actually the live skills
  actives <<- rep(NA,(length(cards)-counter+1))
  
  for (i in counter:length(cards)) {
    j <- i - counter + 1
    actives[j] <<- cards[i] %>%
      str_extract("Active: ([:graph:]|[:blank:])+Unlockable") %>%
      str_remove("Active: ") %>%
      str_remove("Unlockable")
  }
  
  #This removes the condition and activation chance to make parsing the rest easier.
  actives.secondhalf <<- actives %>%
    str_remove("^([[:alnum:]]|[[:blank:]])+, ") %>%
    str_extract("([[:alnum:]]|[[:blank:]]|[[:punct:]])+\\(") %>%
    str_remove(" \\(")
  
  #Stringr can't return a logical if an NA value is presented.
  actives[which(is.na(actives))] <<- ""
  actives.secondhalf[which(is.na(actives.secondhalf))] <<- ""
}

#This function fills in uncap starting from the end and working its way up
#until it reaches the number of cards in table.
#In other words, it gets the uncap data starting with the newest cards and working its
#way backwards, however only every 5th node or so actually contains stat data, so we
#go through the nodes until we find ones that work.
get.uncap.data <- function(counter) {
  #found is the number of nodes that contain stat data that have been found.
  found <- 0
  i <- length(cards.uncap.nodes)+1
  while((found < length(cards)-(counter-1))) {
    
    i <- i-1
    
    tryCatch({
      temp <- xml_child(cards.uncap.nodes[[i]],2)
      #Iterates through and extracts the 5 elements containing the uncapped stats
      for (j in 1:5) {
        temp.attrs <- xml_child(temp,j) %>%
          xml_attrs()
        #appeal, stamina, and technique are located in elements 3, 4, and 5 respectively.
        for (k in 3:5) {
          if(j==1) {
            
            if(k == 3) {
              card_table$appeal1[(length(cards)-found)] <<- temp.attrs %>% extract2(3) %>% as.numeric()
            }
            else if (k == 4) {
              card_table$stam1[(length(cards)-found)] <<- temp.attrs %>% extract2(4) %>% as.numeric()
            }
            else if (k == 5) {
              card_table$tech1[(length(cards)-found)] <<- temp.attrs %>% extract2(5) %>% as.numeric()
            }
          }
          
          if(j==2) {
            
            if(k == 3) {
              card_table$appeal2[(length(cards)-found)] <<- temp.attrs %>% extract2(3) %>% as.numeric()
            }
            else if (k == 4) {
              card_table$stam2[(length(cards)-found)] <<- temp.attrs %>% extract2(4) %>% as.numeric()
            }
            else if (k == 5) {
              card_table$tech2[(length(cards)-found)] <<- temp.attrs %>% extract2(5) %>% as.numeric()
            }
          }
          
          if(j==3) {
            
            if(k == 3) {
              card_table$appeal3[(length(cards)-found)] <<- temp.attrs %>% extract2(3) %>% as.numeric()
            }
            else if (k == 4) {
              card_table$stam3[(length(cards)-found)] <<- temp.attrs %>% extract2(4) %>% as.numeric()
            }
            else if (k == 5) {
              card_table$tech3[(length(cards)-found)] <<- temp.attrs %>% extract2(5) %>% as.numeric()
            }
          }
          
          if(j==4) {
            
            if(k == 3) {
              card_table$appeal4[(length(cards)-found)] <<- temp.attrs %>% extract2(3) %>% as.numeric()
            }
            else if (k == 4) {
              card_table$stam4[(length(cards)-found)] <<- temp.attrs %>% extract2(4) %>% as.numeric()
            }
            else if (k == 5) {
              card_table$tech4[(length(cards)-found)] <<- temp.attrs %>% extract2(5) %>% as.numeric()
            }
          }
          
          if(j==5) {
            
            if(k == 3) {
              card_table$appeal5[(length(cards)-found)] <<- temp.attrs %>% extract2(3) %>% as.numeric()
            }
            else if (k == 4) {
              card_table$stam5[(length(cards)-found)] <<- temp.attrs %>% extract2(4) %>% as.numeric()
            }
            else if (k == 5) {
              card_table$tech5[(length(cards)-found)] <<- temp.attrs %>% extract2(5) %>% as.numeric()
              found <- found+1
            }
          }
        }
      }
      i <- i - 1
      
    },
    error = function(e){i <- i-1})
  }
}

get.basic.info <- function(counter){
  card_table$number[counter:length(cards)] <<- c(seq(counter,length(cards)))
  card_table$idol[counter:length(cards)] <<- card.extra.data$Idol[counter:length(cards)]
  card_table$type[counter:length(cards)] <<- card.extra.data$Type[counter:length(cards)]
  card_table$element[counter:length(cards)] <<- card.extra.data$Element[counter:length(cards)]
}

get.card.name <- function(counter){
  temp <- cards[counter:length(cards)] %>%
    str_extract('([:graph:]|[:blank:])+[:blank:][U,S]?(RBase|RDefault|RGacha|RLimited|REvent)') %>%
    str_remove('#[:digit:]{1,}') %>%
    str_remove('[:blank:][U,S]?(RBase|RDefault|RGacha|RLimited|REvent)') %>%
    str_sub(2)
  
  #Some cards, such as #129 Bleu Tentation Karin, have a little
  #origin story of their title as hover text. This checks for and removes them.
  for(i in 1:length(temp)){
    if(str_detect(temp[i],'[:graph:]+from')) {
      temp[i] <- str_split(temp[i],'(from|\')') %>%
        unlist() %>%
        str_c(tail(.,1)) %>%
        magrittr::extract(1)
    }
  }
  
  card_table$card.name[counter:length(cards)] <<- temp
  
  #There's a typo in this card's name on kach's site.
  if(counter <= 240){
    card_table$card.name[240] <<- "Warm Welcome☆China Maid Rin"
  }
  #This one too. Well, the K in Karin isn't capitalized, so can probably ignore this but whatever.
  if(counter <= 246){
    card_table$card.name[246] <<- "Marching Harmony Karin"
  }
}

get.rarity <- function(counter) {
  card_table$rarity[counter:length(cards)] <<- cards[counter:length(cards)] %>%
    str_extract('[U,S]?(RBase|RDefault|RGacha|RLimited|REvent)( card)*( \\(Fes\\))*') %>%
    str_remove("(Base|Default|Gacha|Limited|Event)( card)* *")
  
  #The first 3 fes URs aren't listed as so on kach's site because ???.
  #Edit: 5/20/20. This seems to have been addressed.
  #Have not tested on a fresh database rebuild, but it should be safe to remove this conditional now.
  if(counter < 129){
    card_table$rarity[127:129] <<- "UR(Fes)"
  }
}

get.uncap0 <- function(counter){
  for(i in counter:length(cards)){
    if(str_detect(cards[i],'[:digit:]{12}')) {
      card <- str_extract(cards[i],'[:digit:]{12}')
      
      card_table$appeal0[i] <<- card %>% str_sub(1,4) %>% as.integer()
      card_table$stam0[i] <<- card %>% str_sub(5,8) %>% as.integer()
      card_table$tech0[i] <<- card %>% str_sub(9,12) %>% as.integer()
    }
    #3 of the new Niji Rs have stats below 1000 at max level so it messes up the way this was
    #originally done. The {12} version does it the old fashioned way, but the {11} version
    #takes a different approach
    else {
      #Tries all 3 appeal-stam-tech arrangements of the 11 numbers and takes
      #the max minus the min to determine which arrangement is correct.
      param <- str_extract(cards[i],'[:digit:]{11}')
      
      #If the appeal and stamina were 4 digits, but the tech was 3.
      aopt1 <- str_sub(param,1,4) %>% as.numeric()
      sopt1 <- str_sub(param,5,8) %>% as.numeric()
      topt1 <- str_sub(param,9,11) %>% as.numeric()
      max1 <- max(c(aopt1,sopt1,topt1)) - min(c(aopt1,sopt1,topt1))
      
      #If the appeal and tech were 4 digits, but the stam was 3.
      aopt2 <- str_sub(param,1,4) %>% as.numeric()
      sopt2 <- str_sub(param,5,7) %>% as.numeric()
      topt2 <- str_sub(param,8,11) %>% as.numeric()
      max2 <- max(c(aopt2,sopt2,topt2)) - min(c(aopt2,sopt2,topt2))
      
      #If the stamina and tech were 4 digits, but the appeal was 3.
      aopt3 <- str_sub(param,1,3) %>% as.numeric()
      sopt3 <- str_sub(param,4,7) %>% as.numeric()
      topt3 <- str_sub(param,8,11) %>% as.numeric()
      max3 <- max(c(aopt3,sopt3,topt3)) - min(c(aopt3,sopt3,topt3))
      
      #Not a surefire way to get it right but... it works for the current cards.
      #Might need to adjust the classifier if future cards have <1000 base stats.
      if(max1 < max2 & max1 < max3){
        card_table$appeal0[i] <<- aopt1
        card_table$stam0[i] <<- sopt1
        card_table$tech0[i] <<- topt1
      }
      else if(max2 < max1 & max2 < max3){
        card_table$appeal0[i] <<- aopt2
        card_table$stam0[i] <<- sopt2
        card_table$tech0[i] <<- topt2
      }
      else {
        card_table$appeal0[i] <<- aopt3
        card_table$stam0[i] <<- sopt3
        card_table$tech0[i] <<- topt3
      }
    }
  }
}

#Gets the ACTIVE skill of a card
get.activ.skill <- function(counter) {
  
  for(i in counter:length(cards)){
    j <- i - counter + 1
    if(str_detect(card.skills[j],"Voltage Boost")) {
      if(str_detect(card.skills[j],"SP skill")) {
        if(str_detect(card.skills[j],"Appeal")) {
          card_table$effect[i] <<- "SP Voltage Boost (%)"
          card_table$param[i] <<- "Appeal"
        }
        else if(str_detect(card.skills[j],"Technique")) {
          card_table$effect[i] <<- "SP Voltage Boost (%)"
          card_table$param[i] <<- "Technique"
        }
        #Doesn't exist, but might as well future proof
        else if(str_detect(card.skills[j],"Stamina")) {
          card_table$effect[i] <<- "SP Voltage Boost (%)"
          card_table$param[i] <<- "Stamina"
        }
        else {
          card_table$effect[i] <<- "SP Voltage Boost (flat value)"
          # #158 Exciting Animal Setsuna SR is missing the "of own Appeal" part of her skill
          #on kach's site.
          if(i == 158){
            card_table$effect[i] <<- "SP Voltage Boost (%)"
            card_table$param[i] <<- "Appeal"
          }
        }
      }
      else {
        if(str_detect(card.skills[j],"Appeal")) {
          card_table$effect[i] <<- "Voltage Boost (%)"
          card_table$param[i] <<- "Appeal"
        }
        else {
          card_table$effect[i] <<- "Voltage Boost (flat value)"
          
        }
      }
    }
    
    else if(str_detect(card.skills[j],"Appeal Boost")) {
      if (str_detect(card.skills[j],"[:digit:]+ for")) {
        card_table$effect[i] <<- "Appeal Boost (flat)"  
      }
      else {
        card_table$effect[i] <<- "Appeal Boost (%)"
      }
    }
    else if(str_detect(card.skills[j],"Critical gain")) {
      card_table$effect[i] <<- "Crit gain"
    }
    else if(str_detect(card.skills[j],"Critical rate")) {
      card_table$effect[i] <<- "Critical rate"
      card_table$target[i] <<- "all"
    }
    else if(str_detect(card.skills[j],"Damage Reduction")) {
      card_table$effect[i] <<- "Damage Reduction"
    }
    else if(str_detect(card.skills[j],"Shield")) {
      if(str_detect(card.skills[j],"blocks")) {
        card_table$effect[i] <<- "Shield (flat)"
      }
      else {
        card_table$effect[i] <<- "Shield (%)"
        if(str_detect(card.skills[j],"Stamina")) {
          card_table$param[i] <<- "Stamina"
        }
        else {
          card_table$param[i] <<- "Technique"
        }
      }
    }
    else if(str_detect(card.skills[j],"Skill Activation Rate")) {
      card_table$effect[i] <<- "Skill Activation Rate"
    }
    
    else if(str_detect(card.skills[j],"SP gauge gain")){
      card_table$effect[i] <<- "SP gauge gain"
      card_table$target[i] <<- "all"
    }
    
    else if(str_detect(card.skills[j],"SP gauge fill")){
      if(str_detect(card.skills[j],"max")){
        card_table$effect[i] <<- "SP Gauge (%)"
      }
      else{
        card_table$effect[i] <<- "SP Gauge (flat)"
      }
    }
    
    else if(str_detect(card.skills[j],"Healer")) {
      if (str_detect(card.skills[j],"equal to")) {
        card_table$effect[i] <<- "Stamina Recovery"
        if (str_detect(card.skills[j],"own Stamina")) {
          card_table$param[i] <<- "Stamina"
        }
        else if(str_detect(card.skills[j],"own Technique")) {
          card_table$param[i] <<- "Technique"
        }
        #This doesn't exist yet, but might as well future-proof
        else if(str_detect(card.skills[j],"own Appeal")) {
          card_table$param[i] <<- "Appeal"
        }
      }
      else {
        card_table$effect[i] <<- "Stamina Recovery (flat)"
      }
    }
    else if(str_detect(card.skills[j],"Voltage gain")) {
      #% ones will say "x% for" flat ones will say "x for"
      if(str_detect(card.skills[j],"[:digit:] for")) {
        card_table$effect[i] <<- "Voltage Gain (flat)"
        #The text on #14 All Stars Umi's skill is inconsistent with every other skill on kach's site.
        if(i == 14){
          card_table$target[i] <<- "all"
        }
      }
      else {
        card_table$effect[i] <<- "Voltage Gain (%)"
      }
    }
    
    temp.values <- card.skills[j] %>%
      str_extract("[:digit:]([:digit:]|[:punct:])+") %>%
      str_extract_all("([:digit:]|\\.)+") %>%
      unlist()
    card_table$val1[i] <<- temp.values[1]
    card_table$val2[i] <<- temp.values[2]
    card_table$val3[i] <<- temp.values[3]
    card_table$val4[i] <<- temp.values[4]
    card_table$val5[i] <<- temp.values[5]
    
    card_table$length[i] <<- str_extract(card.skills[j],"[:digit:]{1,2}[:blank:]notes") %>%
      str_remove(" notes") %>%
      as.numeric()
    #Yet another error on kach's site. #19 Fresh Fruits Rin's length should be 5 notes, but it's
    #listed as 4.
    if(i == 19){
      card_table$length[i] <<- 5
    }
    # #213 Marching Harmony Emma just doesn't even have a length listed at all. kach plz.
    if(i == 213){
      card_table$length[i] <<- 10
    }
    
    if(str_detect(card.skills[j],"all members")) {
      card_table$target[i] <<- "all"
    }
    else if(str_detect(card.skills[j],"same subunit")) {
      card_table$target[i] <<- "same strategy"
    }
    else if(str_detect(card.skills[j],"same year group")) {
      card_table$target[i] <<- "year group"
    }
    else if(str_detect(card.skills[j],"all others")) {
      card_table$target[i] <<- "allies"
    }
    else if(str_detect(card.skills[j],"same attribute")) {
      card_table$target[i] <<- "same element"
    }
    else if(str_detect(card.skills[j],"same school")) {
      card_table$target[i] <<- "same school"
    }
    else if(str_detect(card.skills[j],"same type")) {
      card_table$target[i] <<- "same type"
    }
    
  }
}

get.pass.skill <- function(counter) {
  for(i in counter:length(cards)){
    j <- i - counter + 1
    
    if (str_detect(passives[j],"Appeal")) {
      card_table$`passive stat`[i] <<- "Appeal"
    }
    else if (str_detect(passives[j],"Stamina")) {
      card_table$`passive stat`[i] <<- "Stamina"
    }
    else if (str_detect(passives[j],"Technique")) {
      card_table$`passive stat`[i] <<- "Technique"
    }
    
    
    if (str_detect(passives[j],"own")) {
      card_table$`passive target`[i] <<- "self"
    }
    else if (str_detect(passives[j],"all members")) {
      card_table$`passive target`[i] <<- "all"
    }
    else if (str_detect(passives[j],"same subunit")) {
      card_table$`passive target`[i] <<- "same strategy"
    }
    else if (str_detect(passives[j],"attribute")) {
      card_table$`passive target`[i] <<- "same element"
    }
    else if (str_detect(passives[j],"school")) {
      card_table$`passive target`[i] <<- "same school"
    }
    else if (str_detect(passives[j],"type")) {
      card_table$`passive target`[i] <<- "same type"
    }
    else if (str_detect(passives[j],"all other")) {
      card_table$`passive target`[i] <<- "allies"
    }
    else if (str_detect(passives[j],"Printemps|lily white|BiBi|CYaRon!|Guilty Kiss|AZALEA")) {
      card_table$`passive target`[i] <<- "subunit"
    }
    #Updated 5/29/20 for card #245 True Arabesque Eli.
    #Hers says "card of the same year" for some reason.
    else if (str_detect(passives[j],"(year group|same year)")) {
      card_table$`passive target`[i] <<- "year"
    }
    #Only Honoka for now.
    #Edit 5/1/20 Riko too. Generalizing this was wise!
    else{
      card_table$`passive target`[i] <<- "character"
    }
    
    temp.values <- passives[j] %>%
      str_extract_all("([:digit:]|\\.)+") %>%
      unlist() %>%
      as.numeric()
    #Newer cards have stuff like "[1st years]" written into the passives, which adds an extra 1
    #To the regex and throws things off
    if(length(temp.values) == 6) {
      temp.values <- temp.values[2:6]
    }
    #kach's passive values are off by 1 here for Fes Umi and Fes Ruby because of course they are. 
    #Who knows how many other passives are off.
    if(i %in% c(226,227)){
      card_table$`pass val1`[i] <<- temp.values[1] + 1
      card_table$`pass val2`[i] <<- temp.values[2] + 1
      card_table$`pass val3`[i] <<- temp.values[3] + 1
      card_table$`pass val4`[i] <<- temp.values[4] + 1
      card_table$`pass val5`[i] <<- temp.values[5] + 1
    }
    else{
      card_table$`pass val1`[i] <<- temp.values[1]
      card_table$`pass val2`[i] <<- temp.values[2]
      card_table$`pass val3`[i] <<- temp.values[3]
      card_table$`pass val4`[i] <<- temp.values[4]
      card_table$`pass val5`[i] <<- temp.values[5]
    }
  }

}

#I KNOW THESE ARE LIVE SKILLS AND NOT ACTIVE SKILLS BUT IT'S TOO FAR INTO THE
#PROJECT TO CHANGE IT WITHOUT BREAKING EVERYTHING. I'll fix the names one day, but
#it's not a priority right now.
get.live.skill <- function(counter) {
  
  for(i in counter:length(cards)){
    j <- i - counter + 1
    
    #Stat needs to come earlier because it gets overwritten on some
    #(like damage reduction) that have "appeal chance" written in them.
    #Also need to check for Appeal last.
    #Stat
    
    #There's one duration condition that has "Appeal Chance" in it and this is no good for checking if
    #other parts of the skill work off of the appeal stat. forstat removes that "Appeal chance" part.
    forstat <- str_remove(actives.secondhalf[j],"of (the )?Appeal Chance$")
    
    if(str_detect(forstat,"Appeal")) {
      card_table$`active stat`[i] <<- "Appeal"
    }
    else if(str_detect(forstat,"Technique")) {
      card_table$`active stat`[i] <<- "Technique"
    }
    else if(str_detect(forstat,"Stamina")) {
      if(str_detect(forstat,"max Stamina")) {
        card_table$`active stat`[i] <<- "Max Stamina"
      }
      else {
        card_table$`active stat`[i] <<- "Stamina"
      }
    }
    
    #Conditions
    #Kach is inconsistent here with #252 Marching Harmony Setsuna writing "an Appeal Chance"
    if(str_detect(actives[j],"(beginning of( an)? Appeal Chance|start of( an)? Appeal Chance|during( an)? Appeal Chance)")) {
      card_table$`active condition`[i] <<- "AC start"
    }
    else if(str_detect(actives[j],"(beginning of the Live|beginning of the song)")) {
      card_table$`active condition`[i] <<- "Song start"
    }
    # #238 China Maid Ayumu has a typo on kach's site that reads "Appeal Chance completin"
    else if(str_detect(actives[j],"(completing Appeal Chance|Appeal Chance completi)")) {
      card_table$`active condition`[i] <<- "AC clear"
    }
    else if(str_detect(actives[j],"failing")) {
      card_table$`active condition`[i] <<- "AC fail"
    }
    else if(str_detect(actives[j],"switching subunits")) {
      card_table$`active condition`[i] <<- "Swap"
    }
    #Originally have 500, 1000, and 2000 damage all separate, but now there's one for 200 damage so might as well generalize.
    else if(str_detect(actives[j],"[:digit:]+ damage")){
      card_table$`active condition`[i] <<- str_extract(actives[j],"[:digit:]+ damage")
    }
    else if(str_detect(actives[j],"using SP skill")) {
      card_table$`active condition`[i] <<- "SP Skill"
    }
    else if(str_detect(actives[j],"own taps")) {
      card_table$`active condition`[i] <<- "own taps"
    }
    else if(str_detect(actives[j],"own Crit")) {
      card_table$`active condition`[i] <<- "own crits"
    }
    
    #Chances
    if (str_detect(actives[j],"[:digit:]+% activation")) {
      card_table$`active chance`[i] <<- str_extract(actives[j],"[:digit:]+% activation") %>%
        str_remove("% activation") %>%
        as.numeric()
    }
    
    #Effects
    if(str_detect(actives.secondhalf[j],"to the SP gauge")) {
      if(str_detect(actives.secondhalf[j],"of own")){
        card_table$`active effect`[i] <<- "SP gauge (%)"  
      }
      else {
        card_table$`active effect`[i] <<- "SP gauge (flat)"  
      }
      
    }
    else if(str_detect(actives.secondhalf[j],"cleanse")) {
      card_table$`active effect`[i] <<- "Cleanse"
    }
    #Card 156 Aozora Jumping Heart Dia has a typo on kach's site and says "Voltage boos"
    #So I just deleted the t from the regex.
    else if(str_detect(actives.secondhalf[j],"a Voltage boos")) {
      card_table$`active effect`[i] <<- "Voltage boost"
    }
    else if(str_detect(actives.secondhalf[j],"Voltage boost of")) {
      card_table$`active effect`[i] <<- "SP Voltage"
    }
    else if(str_detect(actives.secondhalf[j],"shield")) {
      card_table$`active effect`[i] <<- "Shield"
    }
    else if(str_detect(actives.secondhalf[j],"hits 0")) {
      card_table$`active effect`[i] <<- "Revive"
    }
    else if(str_detect(actives.secondhalf[j],"skill activation rate")) {
      card_table$`active effect`[i] <<- "Skill rate"
      card_table$`active target`[i] <<- "All"
    }
    else if(str_detect(actives.secondhalf[j],"SP gauge gain")) {{
      card_table$`active effect`[i] <<- "SP gauge gain"
      card_table$`active target`[i] <<- "All"  
    }
    }
    #kach is inconsistent on card #241 China Maid Dia and says "Appeal of" instead of
    #"Appeal stat of"
    else if(str_detect(actives.secondhalf[j],"(Appeal stat|Appeal of)")){
      if(str_detect(actives.secondhalf[j],"per")) {
        card_table$`active effect`[i] <<- "Appeal (# Vo)"
      }
      else{
        card_table$`active effect`[i] <<- "Appeal"
        card_table$`active target`[i] <<- "All"
      }
    }
    else if(str_detect(actives.secondhalf[j],"Critical rate")) {
      card_table$`active effect`[i] <<- "Critical rate"
      card_table$`active target`[i] <<- "All"
    }
    else if(str_detect(actives.secondhalf[j],"Critical taps")) {
      card_table$`active effect`[i] <<- "Critical value"
      card_table$`active target`[i] <<- "All"
    }
    else if(str_detect(actives.secondhalf[j],"Voltage gain")) {
      card_table$`active effect`[i] <<- "Voltage gain"
      card_table$`active target`[i] <<- "All"
    }
    else if(str_detect(actives.secondhalf[j],"recovers")) {
      if(str_detect(actives.secondhalf[j],"per")) {
        card_table$`active effect`[i] <<- "Heal (# Gd)"
      }
      else {
        card_table$`active effect`[i] <<- "Heal"
      }
    }
    #Another inconsistency, this time with #248 Noble Princess Riko
    else if(str_detect(actives.secondhalf[j],"restores")) {
      card_table$`active effect`[i] <<- "Heal"
    }
    
    else if(str_detect(actives.secondhalf[j],"reduces damage")) {
      card_table$`active effect`[i] <<- "Damage reduction"
      
    }
    else if(str_detect(actives.secondhalf[j],"Gd switch bonus")) {
      card_table$`active effect`[i] <<- "switch bonus (Gd)"
    }
    
    
    #Values
    #Edit 5/29/20 - Updated this to account for decimal values. Apparently I just never noticed
    #that this was wrong since launch cards had decimal values here.
    if(str_detect(actives.secondhalf[j],"[:digit:]+% for")) {
      card_table$`active value`[i] <<- str_extract(actives.secondhalf[j],"(\\.|[:digit:])+% for") %>%
        str_remove("% for") %>%
        as.numeric()
    }
    else if(str_detect(actives.secondhalf[j],"[:digit:]+% of")) {
      card_table$`active value`[i] <<- str_extract(actives.secondhalf[j],"(\\.|[:digit:])+% of") %>%
        str_remove("% of") %>%
        as.numeric()
    }
    else if(str_detect(actives.secondhalf[j],"[:digit:]+% Stamina")) {
      card_table$`active value`[i] <<- str_extract(actives.secondhalf[j],"(\\.|[:digit:])+% Stamina") %>%
        str_remove("% Stamina") %>%
        as.numeric()
    }
    else if(str_detect(actives.secondhalf[j],"[:digit:]+% until")) {
      card_table$`active value`[i] <<- str_extract(actives.secondhalf[j],"(\\.|[:digit:])+% until") %>%
        str_remove("% until") %>%
        as.numeric()
    }
    else if(str_detect(actives.secondhalf[j],"[:digit:]+(%)* per")) {
      card_table$`active value`[i] <<- str_extract(actives.secondhalf[j],"(\\.|[:digit:])+(%)* per") %>%
        str_remove("(%)* per") %>%
        as.numeric()
    }
    #105 Twilight Demon Yoshiko
    else if(str_detect(actives.secondhalf[j],"[:digit:]+(%)* max")) {
      card_table$`active value`[i] <<- str_extract(actives.secondhalf[j],"[:digit:]+(%)* max") %>%
        str_remove("(%)* max") %>%
        as.numeric()
    }
    
    #Duration
    if(str_detect(actives.secondhalf[j],"remainder")) {
      card_table$`active duration`[i] <<- "Song"
    }
    #kach is so incredibly inconsistent with this one. It's written so many different ways.
    else if(str_detect(actives.secondhalf[j],"(end|remainder|duration) of (the )*Appeal Chance")) {
      card_table$`active duration`[i] <<- "AC"
    }
    else if(str_detect(actives.secondhalf[j],"[:digit:]+ notes")) {
      card_table$`active duration`[i] <<- str_extract(actives.secondhalf[j],"[:digit:]+ notes") %>%
        str_remove(" notes")
    }
    
    
    #Target
    if(str_detect(actives.secondhalf[j],"(all members|all cards)")) {
      card_table$`active target`[i] <<- "All"
    }
    else if(str_detect(actives.secondhalf[j],"all allies")) {
      card_table$`active target`[i] <<- "Allies"
    }
    else if(str_detect(actives.secondhalf[j],"same attribute")) {
      card_table$`active target`[i] <<- "Same element"
    }
    else if(str_detect(actives.secondhalf[j],"Honoka|Riko")) {
      card_table$`active target`[i] <<- "Character"
    }
    else if(str_detect(actives.secondhalf[j],"same subunit")) {
      card_table$`active target`[i] <<- "same strategy"
    }
    #6/15/20: This was missing oops. Wasn't added in game until #220 Snow Crystal Maki.
    else if(str_detect(actives.secondhalf[j],"same year group")) {
      card_table$`active target`[i] <<- "year"
    }
  }
}