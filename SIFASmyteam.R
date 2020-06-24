#Add the card number from my_cards
#Strategy is either "red", "green", or "blue"
#Pos is a number between 1 and 9 that represents position.
#Position 1 is the center.
#Positions 2 and 3 are the centers that affect your SP skill.
#Positions 4-9 are identical, but you can specify a number if you'd like to replace a specific card.
#Order is the in-strategy order which cards will appeal from 1-3. 0 is essentially a "set the order for me" option.
#Save is for whether you want to automatically save to the database after adding the member.
#If bulk adding team members, you should only use this on the last member to speed up the process.
add.member <- function(card,strategy,pos,strat.order=0,save=F){
  
  if(!exists('my_team')){
    init_my_team()
  }
  
  if(!exists('calc_appeal')){
    init_stat()
  }
  
  check_valid_member(card,strategy,pos,strat.order)
  
  
  card.row <- my_cards[which(my_cards$card.num==card),]
  
  my_team$card.num[pos] <<- card %>% as.integer()
  my_team$card.name[pos] <<- card.row$card.name
  my_team$card.strategy[pos] <<- strategy
  my_team$type[pos] <<- card.row$type
  my_team$idol[pos] <<- card.row$idol
  my_team$element[pos] <<- card.row$element
  my_team$base.appeal[pos] <<- card.row$appeal %>% as.integer()
  my_team$base.stam[pos] <<- card.row$stamina %>% as.integer()
  my_team$base.tech[pos] <<- card.row$technique %>% as.integer()
  my_team$base.skill[pos] <<- get.baseskill(card.row$rarity)
  my_team$base.sptap[pos] <<- get.basesptap(card.row$rarity) %>% as.integer()
  
  #Not touching the appeal, stam, tech, skill.rate, or sptap columns yet.
  
  my_team$active.effect[pos] <<- card.row$live.effect
  my_team$active.tar[pos] <<- card.row$live.target
  my_team$active.val[pos] <<- card.row$live.val
  my_team$active.param[pos] <<- card.row$live.param
  my_team$active.len[pos] <<- card.row$live.length
  
  my_team$pass.tar[pos] <<- card.row$pass.target
  my_team$pass.stat[pos] <<- card.row$pass.stat
  my_team$pass.val[pos] <<- card.row$pass.val
  
  my_team$live.effect[pos] <<- card.row$activ.effect
  my_team$live.tar[pos] <<- card.row$activ.target
  my_team$live.val[pos] <<- card.row$activ.value
  my_team$live.param[pos] <<- card.row$activ.stat
  my_team$live.len[pos] <<- card.row$activ.dur
  my_team$live.cond[pos] <<- card.row$activ.cond
  
  my_team$insp1[pos] <<- card.row$insp1
  my_team$insp2[pos] <<- card.row$insp2
  my_team$insp3[pos] <<- card.row$insp3
  my_team$insp4[pos] <<- card.row$insp4
  
  set.base.stats(pos)
  
  check_strategy_count(pos,strategy)
  #Strat order = 0 means "Don't change it."
  if(strat.order == 0){
    strat.order <- my_team$strategy.order[pos]
  }
  
  my_team$strategy.order[pos] <<- strat.order
  verify.strat.order(pos)
  
  apply.passive()
  apply.insp()
  apply.strategy()
  
  guest.passive.apply()
  guest.insp.apply()  
  
  #Adds up all of the passive/insp/guest bonuses to get the actual stat.
  get.calc.stat()
  
  if(save){
    save_team() 
  }
}



load_team <- function() {
  db = dbConnect(SQLite(), dbname="C:/Users/Mike/Documents/R/SIFASteambuilder/SIFAS.sqlite")
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  my_teams <<- dbGetQuery(db,'SELECT * FROM my_teams')
  accessories.on.team <<- dbGetQuery(db,'SELECT * FROM my_team_accessories') %>% unlist() %>% as.vector()
  calc_appeal <<- dbGetQuery(db,'SELECT * FROM my_team_calc_appeal')
  calc_stam <<- dbGetQuery(db,'SELECT * FROM my_team_calc_stam')
  calc_tech <<- dbGetQuery(db,'SELECT * FROM my_team_calc_tech')
  calc_skill <<- dbGetQuery(db,'SELECT * FROM my_team_calc_skill')
  calc_sp <<- dbGetQuery(db,'SELECT * FROM my_team_calc_sp')
  calc_damage <<- dbGetQuery(db,'SELECT * FROM my_team_calc_damage')
  calc_critval <<- dbGetQuery(db,'SELECT * FROM my_team_calc_critval')
  calc_critrate <<- dbGetQuery(db,'SELECT * FROM my_team_calc_critrate')
  calc_volt <<- dbGetQuery(db,'SELECT * FROM my_team_calc_volt')
  dbDisconnect(db)
}

save_team <- function() {
  db = dbConnect(SQLite(), dbname="C:/Users/Mike/Documents/R/SIFASteambuilder/SIFAS.sqlite")
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  dbWriteTable(db,"my_team",my_team,overwrite=T)
  dbWriteTable(db,"my_team_accessories",as_tibble(accessories.on.team),overwrite=T)
  dbWriteTable(db,"my_team_calc_appeal",calc_appeal,overwrite=T)
  dbWriteTable(db,"my_team_calc_stam",calc_stam,overwrite=T)
  dbWriteTable(db,"my_team_calc_tech",calc_tech,overwrite=T)
  dbWriteTable(db,"my_team_calc_skill",calc_skill,overwrite=T)
  dbWriteTable(db,"my_team_calc_sp",calc_sp,overwrite=T)
  dbWriteTable(db,"my_team_calc_damage",calc_damage,overwrite=T)
  dbWriteTable(db,"my_team_calc_critval",calc_critval,overwrite=T)
  dbWriteTable(db,"my_team_calc_critrate",calc_critrate,overwrite=T)
  dbWriteTable(db,"my_team_calc_volt",calc_volt,overwrite=T)
  dbDisconnect(db)
}

#Self explanatory. URs have a 33% skill activation rate, while everyone else has a 30% rate.
get.baseskill <- function(rarity){
  if(rarity == 'UR' || rarity == 'UR(Fes)'){return(.33)}
  else{return(.3)}
}

#Also self explanatory.
get.basesptap <- function(rarity){
  if(rarity == 'UR' || rarity == 'UR(Fes)'){return(200)}
  else if(rarity == 'SR'){return(150)}
  else{return(100)}
}

#Checks are ordered by how long they should take to exectute, although the data
#involved in my_cards is so small that it shouldn't even matter.
check_valid_member <- function(card,strategy,pos,strat.order){
  if(!(strategy %in% c("red","green","blue"))){
    print("Strategy parameter must be either 'red', 'green', or 'blue'")
    stop(call. = F)
  }
  
  if(!(strat.order %in% seq(0,3))){
    print("Invalid order within strategy.")
  }
  
  if(!(pos %in% seq(1,9))){
    print("Position must be between 1 and 9.")
    stop(call. = F)
  }
  
  if(length(my_cards[which(my_cards$card.num==card),1])==0){
    print("This card number is not in your my_cards list.")
    stop(call. = F)
  }
}


#type, element, and idol needed for skills.
#base, appeal, and stamina are as displayed in my cards. Needed for skills.
#Displayed appeal, stamina, and technique are what you would see on the team formation screen BUT
#it'll be a little higher because guest is also included.
#These values are initialized to 1 because of how I expect the reinforcement agent will work when
#it comes time for model training.

#Skill are copied over directly from their my_cards counterparts with their names corrected.
#Active and live skills were incorrectly labeled as each other.
#Also note that my_cards$activ.stat was renamed to my_team$live.param for consistency.

#New skill activation rate and SP per tap parameters have been added.
#They have base forms (which are set based on their rarity) and these will be modified based
#on the strategy setup and skill activations mid-song.

#Accessory number was only added as a unique identifier for accessories (to ensure that you can't)
#put multiple of the same accessory on a team.

#Didn't include accessory rarity or uncap count as variables because it doesn't really matter.
init_my_team <- function(){
  
  tryCatch({load_team()},error = function(e){
    my_team <<- tibble(pos=seq(1,9),card.num=-1,card.name="Filler",
                       card.strategy="",type="",idol="",element="",
                       base.appeal=1,base.stam=1,base.tech=1,
                       base.skill=0,base.sptap=0,
                       appeal=1,stam=1,tech=1,
                       skill.rate=0,sptap=0,
                       active.effect=NA,active.tar=NA,active.val=NA,active.param=NA,active.len=NA,
                       pass.tar="",pass.stat=NA,pass.val=NA,
                       live.effect=NA,live.tar=NA,live.val=NA,live.param=NA,live.len=NA,live.cond=NA,
                       insp1="",insp2="",insp3="",insp4="",
                       acc.num=NA,acc.name=NA,acc.element=NA,
                       acc.appeal=0,acc.stam=0,acc.tech=0,
                       acc.skill=NA,acc.skill.cond=NA,acc.skill.val=NA,acc.skill.rate=NA,acc.skill.dur=NA,
                       critval=0,critrate=0,damage=0,voltage=0,strategy.order=rep(seq(1,3),3))
  })
  
  accessories.on.team <<- rep(0,9)
  init_stat()
}

#Initializes all the possible ways that these stats could be modified.
#Will likely add more in the future as I make progress on songs.
init_stat <- function() {
  #For appeal, stam, and tech, guest can affect these with both passives and inspiration skills.
  #For everything else, guest just refers to inspiration skills.
  calc_appeal <<- tibble(pos=seq(1,9),base=0,pass=0,insp=0,pass.g=0,insp.g=0,debuff=0)
  calc_stam <<- tibble(pos=seq(1,9),base=0,pass=0,insp=0,pass.g=0,insp.g=0,)
  calc_tech <<- tibble(pos=seq(1,9),base=0,pass=0,insp=0,pass.g=0,insp.g=0,)
  calc_skill <<- tibble(pos=seq(1,9),base=0,insp=0,inspstrat=0,strat=0,debuff=0,guest=0)
  calc_sp <<- tibble(pos=seq(1,9),base=0,insp=0,inspstrat=0,strat=0,debuff=0,guest=0)
  calc_damage <<- tibble(pos=seq(1,9),base=0,inspstrat=0,strat=0,debuff=0,guest=0)
  calc_critval <<- tibble(pos=seq(1,9),base=1.5,insp=0,guest=0)
  calc_critrate <<- tibble(pos=seq(1,9),base=0,insp=0,guest=0)
  calc_volt <<- tibble(pos=seq(1,9),strat=0,inspstrat=0,guest=0)
  
}

#The base values are important for many, many calculations across many different stats
set.base.stats <- function(card){
  calc_appeal$base[card] <<- my_team$base.appeal[card]
  calc_stam$base[card] <<- my_team$base.stam[card]
  calc_tech$base[card] <<- my_team$base.tech[card]
  calc_skill$base[card] <<- my_team$base.skill[card]
  calc_sp$base[card] <<- my_team$base.sptap[card]
}

#Each of the 3 strategies may only have 3 members in it at any given time.
#If you attempt to add a character into a strategy that already has 3 slots taken up,
#the above function will swap one of the members in the strategy with 4 members with
#a strategy that's down to 2 members.
check_strategy_count <- function(new.pos,strategy){
  #We don't want to do anything if the counts line up.
  all.match <- F
  green <- sum(my_team$card.strategy=="green")
  blue <- sum(my_team$card.strategy=="blue")
  red <- sum(my_team$card.strategy=="red")
  
  if((green+red+blue) != 9 && red <= 3 && blue <= 3 && green <= 3){
    return()
  }
  
  if(green==3 && red==3 && blue==3) {
    all.match <- T
  }
  
  #Keep track of which one has too many and which doesn't have enough.
  if(!all.match){
    if(green==4){
      too.many <- "green"
    }
    else if(blue==4){
      too.many <- "blue"
    }
    else{too.many <- "red"}
    
    if(green<3){
      missing <- "green"
    }
    else if(blue<3){
      missing <- "blue"
    }
    else{missing <- "red"}
    
    #Identify which row should receive the color change.
    to.change <- my_team %>%
      filter(card.strategy==too.many) %>%
      filter(pos != new.pos) %>%
      filter(row_number()==1) %>%
      select(pos) %>%
      as.numeric()
    
    my_team$card.strategy[to.change] <<- missing
    
    guest.passive.apply()
    guest.insp.apply()  
    
    print("Strategy colors have been modified to ensure 3 of each color.")
  }
}

#The check_strategy_count function may swap strategies around in an unfavorable fashion.
#This makes adjusting them easier.
swap.strategies <- function(pos1,pos2){
  strat1 <- my_team$card.strategy[pos1]
  strat2 <- my_team$card.strategy[pos2]
  
  my_team$card.strategy[pos1] <<- strat2
  my_team$card.strategy[pos2] <<- strat1
  
  apply.passive()
  apply.insp()
  apply.strategy()
  get.calc.stat()
  
  verify.strat.order(pos1)
  verify.strat.order(pos2)
  
  guest.passive.apply()
  guest.insp.apply()  
  
  save_team()
}

#Applies the passive stat bonuses from each character's passive to each applicable character on the team.
#school, subunit, and year require joining my_team with the idol.data table since that info isn't
#contained in my_team.
apply.passive <- function() {
  
  calc_appeal$pass <<- 0
  calc_stam$pass <<- 0
  calc_tech$pass <<- 0
  
  #i is the card whose passive is being evaluated
  #j is the card whose stats are being modified by the passive.
  for(i in 1:9){
    for(j in 1:9){
      #apply.passive will take care of the i = card scenario already.
      if(my_team$pass.tar[i] == 'all'){
        update.stat(i,j)
      }
      else if(my_team$pass.tar[i] == 'allies'){
        if(i != j){
          update.stat(i,j)
        }
      }
      
      else if(my_team$pass.tar[i] == 'character'){
        if(my_team$idol[i] == my_team$idol[j]){
          update.stat(i,j)
        }
      }
      else if(my_team$pass.tar[i] == 'character'){
        if(my_team$idol[i] == my_team$idol[j]){
          update.stat(i,j)
        }
      }
      else if(my_team$pass.tar[i] == 'same element'){
        if(my_team$element[i] == my_team$element[j]){
          update.stat(i,j)
        }
      }
      else if(my_team$pass.tar[i] == 'same school'){
        match <- left_join(my_team,idol.data,by=c('idol'='idol'))
        if(match$school[i] == match$school[j]){
          update.stat(i,j)
        }
      }
      else if(my_team$pass.tar[i] == 'same strategy'){
        if(my_team$card.strategy[i] == my_team$card.strategy[j]){
          update.stat(i,j)
        }
      }
      else if(my_team$pass.tar[i] == 'same type'){
        if(my_team$type[i] == my_team$type[j]){
          update.stat(i,j)
        }
      }
      else if(my_team$pass.tar[i] == 'self'){
        if(i == j){
          update.stat(i,j)
        }
      }
      else if(my_team$pass.tar[i] == 'same subunit'){
        match <- left_join(my_team,idol.data,by=c('idol'='idol'))
        if(match$subunit[i] == match$subunit[j]){
          update.stat(i,j)
        }
      }
      else if(my_team$pass.tar[i] == 'year'){
        match <- left_join(my_team,idol.data,by=c('idol'='idol'))
        if(match$year[i] == match$year[j]){
          update.stat(i,j)
        }
      }
    }
  }
}

#Hopefully this can be recycled.
#Basically the passive skill values are stored as like, "7" rather than .07, so this adjusts that.
update.stat <- function(i,card,divideby=100,guest=F){
  if(!guest){
    if(my_team$pass.stat[i]=="Appeal"){
      calc_appeal$pass[card] <<- calc_appeal$pass[card] + (my_team$pass.val[i]/divideby)
    }
    else if(my_team$pass.stat[i]=="Stamina"){
      calc_stam$pass[card] <<- calc_stam$pass[card] + (my_team$pass.val[i]/divideby)
    }
    else{
      calc_tech$pass[card] <<- calc_tech$pass[card] + (my_team$pass.val[i]/divideby)
    }
  }
  else{
    if(my_team_guest$pass.stat[i]=="Appeal"){
      calc_appeal$pass.g[card] <<- calc_appeal$pass.g[card] + (my_team_guest$pass.val[i]/divideby)
    }
    else if(my_team_guest$pass.stat[i]=="Stamina"){
      calc_stam$pass.g[card] <<- calc_stam$pass.g[card] + (my_team_guest$pass.val[i]/divideby)
    }
    else{
      calc_tech$pass.g[card] <<- calc_tech$pass.g[card] + (my_team_guest$pass.val[i]/divideby)
    }
  }

}

#Uses the info from the calc_xx tables to calculate updated stat values.
get.calc.stat <- function(){
  for(i in 1:9){
    my_team$appeal[i] <<- floor(calc_appeal$base[i] + floor(calc_appeal$base[i]*calc_appeal$pass[i] +
                                                              calc_appeal$base[i]*calc_appeal$insp[i]))
                                                              #+ calc_appeal$base[i]*calc_appeal$pass.g[i] +
                                                              #calc_appeal$base[i]*calc_appeal$insp.g[i]))
    my_team$stam[i] <<-  floor(calc_stam$base[i] + floor(calc_stam$base[i]*calc_stam$pass[i] +
                                                             calc_stam$base[i]*calc_stam$insp[i]))
                                                             #+ calc_stam$base[i]*calc_stam$pass.g[i] +
                                                             #calc_stam$base[i]*calc_stam$insp.g[i]))
    my_team$tech[i] <<- floor(calc_tech$base[i] + floor(calc_tech$base[i]*calc_tech$pass[i] +
                                                         calc_tech$base[i]*calc_tech$insp[i]))
                                                         #+ calc_tech$base[i]*calc_tech$pass.g[i] +
                                                         #calc_tech$base[i]*calc_tech$insp.g[i]))
    my_team$skill.rate[i] <<- sum(calc_skill[i,2:6])
    my_team$sptap[i] <<- floor(calc_sp$base[i]*(1+sum(calc_sp[i,3:7])))
    my_team$critval[i] <<- sum(calc_critval[i,2:4])
    #Checks if tech is the card's highest stat.
    #This needs to be checked on the card_table level rather than the base stat level because it's possible
    #for an uncap to make tech the highest base stat without actually affecting the crit rate.
    tryCatch({ 
      if((card_table$tech5[which(card_table$card.name==my_team$card.name[i])]) > (card_table$appeal5[which(card_table$card.name==my_team$card.name[i])]) &&
         (card_table$tech5[which(card_table$card.name==my_team$card.name[i])]) > (card_table$stam5[which(card_table$card.name==my_team$card.name[i])])){
        calc_critrate$base[i] <<- (my_team$tech[i]*.0000291)+.15
        my_team$critrate[i] <<- calc_critrate$base[i]
      }
      else{
        calc_critrate$base[i] <<- my_team$tech[i]*.0000291
        my_team$critrate[i] <<- calc_critrate$base[i]
      }
    },error = function(e){})
    #This won't be modified until the song is set. Should just read 0 for now.
    my_team$damage[i] <<- floor(calc_damage$base[i]+(floor(calc_damage$base[i]*sum(calc_damage[i,3:6]))))
    my_team$voltage[i] <<- sum(calc_volt[i,2:4])
    
  }
  #This kind of sucks, but we need to reapply the accessory after each card change.
  #There's probably a more efficient way to do this but since a card change may affect
  #the strategy composition, I don't see an alternative.
  for(i in 1:9){
    for(j in 1:9){
      if(identical(my_team$card.strategy[i],my_team$card.strategy[j])){
        if(identical(tolower(my_team$acc.element[i]),my_team$element[j])){
          my_team$appeal[j] <<- my_team$appeal[j] + floor(my_team$acc.appeal[i]*1.1)
          my_team$stam[j] <<- my_team$stam[j] + floor(my_team$acc.stam[i]*1.1)
          my_team$tech[j] <<- my_team$tech[j] + floor(my_team$acc.tech[i]*1.1)
        }
        else{
          my_team$appeal[j] <<- my_team$appeal[j] + my_team$acc.appeal[i]
          my_team$stam[j] <<- my_team$stam[j] + my_team$acc.stam[i]
          my_team$tech[j] <<- my_team$tech[j] + my_team$acc.tech[i]
        }
      }
    }
  }
}

#i is only used for the type bonus inspiration skills because it needs to check that the strategies match.
inspi.determine.stat <- function(row,k,i,guest=F) {
  if(!guest){
    if(row$inspi.effect == "Appeal+"){
      calc_appeal$insp[k] <<- calc_appeal$insp[k] + row$inspi.val  
    }
    else if(row$inspi.effect == "Critical value"){
      calc_critval$insp[k] <<- calc_critval$insp[k] + row$inspi.val
    }
    else if(row$inspi.effect == "Skill activation rate"){
      calc_skill$insp[k] <<- calc_skill$insp[k] + row$inspi.val
    }
    else if(row$inspi.effect == "Type bonus"){
      if(identical(my_team$card.strategy[k],my_team$card.strategy[i])){
        if(my_team$type[i]=='vo'){
          calc_volt$inspstrat[k] <<- calc_volt$inspstrat[k] + row$inspi.val
        }
        else if(my_team$type[i]=='gd'){
          calc_damage$inspstrat[k] <<- calc_damage$inspstrat[k] + row$inspi.val
        }
        else if(my_team$type[i]=='sp'){
          calc_sp$inspstrat[k] <<- calc_sp$inspstrat[k] + row$inspi.val
        }
        else if(my_team$type[i]=='sk'){
          calc_skill$inspstrat[k] <<- calc_skill$inspstrat[k] + row$inspi.val
        }
      }
    }
  }
  else{
    if(row$inspi.effect == "Appeal+"){
      calc_appeal$insp.g[k] <<- calc_appeal$insp.g[k] + row$inspi.val  
    }
    else if(row$inspi.effect == "Critical value"){
      calc_critval$guest[k] <<- calc_critval$guest[k] + row$inspi.val
    }
    else if(row$inspi.effect == "Skill activation rate"){
      calc_skill$guest[k] <<- calc_skill$guest[k] + row$inspi.val
    }
    else if(row$inspi.effect == "Type bonus"){
      if(identical(my_team$card.strategy[k],my_team$card.strategy[i])){
        if(my_team$type[i]=='vo'){
          calc_volt$guest[k] <<- calc_volt$guest[k] + row$inspi.val
        }
        else if(my_team$type[i]=='gd'){
          calc_damage$guest[k] <<- calc_damage$guest[k] + row$inspi.val
        }
        else if(my_team$type[i]=='sp'){
          calc_sp$guest[k] <<- calc_sp$guest[k] + row$inspi.val
        }
        else if(my_team$type[i]=='sk'){
          calc_skill$guest[k] <<- calc_skill$guest[k] + row$inspi.val
        }
      }
    }
  }
}

apply.insp <- function(){
  calc_appeal$insp <<- 0
  calc_skill$insp <<- 0
  calc_skill$inspstrat <<- 0
  calc_critval$insp <<- 0
  
  calc_sp$inspstrat <<- 0
  calc_damage$inspstrat <<- 0
  calc_volt$inspstrat <<- 0
  
  
  #i is the card whose inspiration skills are being evaluated
  for(i in 1:9){
    #32 - 35 are the columns for the inspiration skills in the my_team table.
    for(j in 32:35){
      #The row of the inspiration skill being evaluated.
      row <- inspi.skills[which(inspi.skills$inspi.name==as.character(my_team[i,j])),]
      if(nrow(row)!=0){
        if(identical(row$inspi.dur,"Permanent")){
          #k are the cards that need to be checked to see if the inspiration skills applies to them.
          for(k in 1:9){
            if(row$inspi.target == 'all'){
              inspi.determine.stat(row,k,i)
            }
            if(row$inspi.target == 'allies'){
              if(i != k){
                inspi.determine.stat(row,k,i)
              }
            }
            if(row$inspi.target == 'attribute'){
              if(my_team$element[i] == my_team$element[k]){
                inspi.determine.stat(row,k,i)
              }
            }
            if(row$inspi.target == 'school'){
              match <- left_join(my_team,idol.data,by=c('idol'='idol'))
              if(identical(match$school[i],match$school[k])){
                inspi.determine.stat(row,k,i)
              }
            }
            #Type bonus says that it affects self when it really affects the same strategy.
            #Or at least I think it does...
            if(row$inspi.target == 'self' && row$inspi.effect != 'Type bonus'){
              if(i == k){
                inspi.determine.stat(row,k,i)
              }
            }
            if(row$inspi.target == 'strategy' || row$inspi.effect == 'Type bonus'){
              if(my_team$card.strategy[i] == my_team$card.strategy[k]){
                inspi.determine.stat(row,k,i)
              }
            }
            if(row$inspi.target == 'type'){
              if(my_team$type[i] == my_team$type[k]){
                inspi.determine.stat(row,k,i)
              }
            }
            if(row$inspi.target == 'year'){
              match <- left_join(my_team,idol.data,by=c('idol'='idol'))
              if(identical(match$year[i],match$year[k])){
                inspi.determine.stat(row,k,i)
              }
            }
          }
        }
      }
    }
  }
}

#sets the strategy bonuses for each card based on their types
apply.strategy <- function() {
  calc_volt$strat <- 0
  calc_damage$strat <- 0
  calc_sp$strat <- 0
  calc_skill$strat <- 0
  
  #I have to make the change locally using <- and then push it globally with <<- at the end.
  #Why can't I just push it globally to begin with? Excellent question. No idea.
  #global assignment works fine literally everywhere else.
  for(i in 1:9){
    for(j in 1:9){
      if(identical(my_team$card.strategy[i],my_team$card.strategy[j])){
        if(my_team$type[i]=='vo'){
          calc_volt$strat[j] <- calc_volt$strat[j] + .05
          calc_damage$strat[j] <- calc_damage$strat[j] - .05
        }
        else if(my_team$type[i]=='gd'){
          calc_damage$strat[j] <- calc_damage$strat[j] + .05
          calc_sp$strat[j] <- calc_sp$strat[j] - .05
        }
        else if(my_team$type[i]=='sp'){
          calc_sp$strat[j] <- calc_sp$strat[j] + .05
          calc_skill$strat[j] <- calc_skill$strat[j] - .05
        }
        else if(my_team$type[i]=='sk'){
          calc_skill$strat[j] <- calc_skill$strat[j] + .05
          calc_volt$strat[j] <- calc_volt$strat[j] - .05
        }
      }
    }
  }
  calc_sp <<- calc_sp
  calc_skill <<- calc_skill
  calc_volt <<- calc_volt
  calc_damage <<- calc_damage
}

#num is the row number of the accessory in the my_accessories table
team.add.accessory <- function(num,slot,save=F){
  team.accessory.checks(num,slot)
  
  #Remove the old accessory if there is one before adding a new one.
  if(!(my_team$acc.appeal[slot]==0)){
    team.remove.accessory(slot)
  }
  
  my_team$acc.num[slot] <<- my_accessories$acc.num[num]
  my_team$acc.name[slot] <<- my_accessories$acc.name[num]
  my_team$acc.element[slot] <<- my_accessories$acc.element[num]
  my_team$acc.appeal[slot] <<- my_accessories$acc.appeal[num]
  my_team$acc.stam[slot] <<- my_accessories$acc.stam[num]
  my_team$acc.tech[slot] <<- my_accessories$acc.tech[num]
  my_team$acc.skill[slot] <<- my_accessories$acc.skill[num]
  my_team$acc.skill.cond[slot] <<- my_accessories$acc.skill.cond[num]
  my_team$acc.skill.val[slot] <<- my_accessories$acc.skill.val[num]
  my_team$acc.skill.rate[slot] <<- my_accessories$acc.skill.rate[num]
  my_team$acc.skill.dur[slot] <<- my_accessories$acc.skill.dur[num]
  
  team.acc.stat.calc(slot)
  
  if(save){
    save_team() 
  }
}

#Checks if the accessory that's trying to be added has already been added to your team.
team.accessory.checks <- function(num,slot){
  if(!exists('my_accessories')){
    tryCatch({load_accessories()},
             error <- function(e){print("Please load the functions in the SIFASmyaccessories.R file.")
               stop(call. = F)})
  }
  
  if(!(slot %in% seq(1,9))){
    print("Invalid team slot entered. Please enter an integer between 1 and 9.")
    stop(call. = F)
  }
  
  if(!(num %in% seq(1,nrow(my_accessories)))){
    print("This row number can not be found in your accessory list.")
    stop(call. = F)
  }
  
  if(!exists('accessories.on.team')){
    accessories.on.team <<- rep(0,9)
  }
  
  if(num %in% accessories.on.team){
    print("This accessory has already been added.")
    stop(call. = F)
  }
  
  accessories.on.team[slot] <<- num
}


#Removes an accessory from a selected card.
team.remove.accessory <- function(slot){
  if(!(slot %in% seq(1,9))){
    print("Invalid team slot entered. Please enter an integer between 1 and 9.")
    stop(call. = F)
  }
  
  accessories.on.team[slot] <<- 0
  
  team.rem.acc.stat(slot)
  
  my_team$acc.num[slot] <<- NA
  my_team$acc.name[slot] <<- NA
  my_team$acc.element[slot] <<- NA
  my_team$acc.appeal[slot] <<- 0
  my_team$acc.stam[slot] <<- 0
  my_team$acc.tech[slot] <<- 0
  my_team$acc.skill[slot] <<- NA
  my_team$acc.skill.cond[slot] <<- NA
  my_team$acc.skill.val[slot] <<- NA
  my_team$acc.skill.rate[slot] <<- NA
  my_team$acc.skill.dur[slot] <<- NA
  
  save_team()
}

#Determines what stats the accessory should give to each affected card. Matching element gives a 10% bonus.
team.acc.stat.calc <- function(pos) {
  for(i in 1:9){
    if(identical(my_team$card.strategy[pos],my_team$card.strategy[i])){
      if(identical(tolower(my_team$acc.element[pos]),my_team$element[i])){
        my_team$appeal[i] <<- my_team$appeal[i] + floor(my_team$acc.appeal[pos]*1.1)
        my_team$stam[i] <<- my_team$stam[i] + floor(my_team$acc.stam[pos]*1.1)
        my_team$tech[i] <<- my_team$tech[i] + floor(my_team$acc.tech[pos]*1.1)
      }
      else{
        my_team$appeal[i] <<- my_team$appeal[i] + my_team$acc.appeal[pos]
        my_team$stam[i] <<- my_team$stam[i] + my_team$acc.stam[pos]
        my_team$tech[i] <<- my_team$tech[i] + my_team$acc.tech[pos]
      }
    }
  }
}

#Have to remove an accessory's stats when removing the accessory.
team.rem.acc.stat <- function(pos){
  for(i in 1:9){
    if(identical(my_team$card.strategy[pos],my_team$card.strategy[i])){
      if(identical(tolower(my_team$acc.element[pos]),my_team$element[i])){
        my_team$appeal[i] <<- my_team$appeal[i] - floor(my_team$acc.appeal[pos]*1.1)
        my_team$stam[i] <<- my_team$stam[i] - floor(my_team$acc.stam[pos]*1.1)
        my_team$tech[i] <<- my_team$tech[i] - floor(my_team$acc.tech[pos]*1.1)
      }
      else{
        my_team$appeal[i] <<- my_team$appeal[i] - my_team$acc.appeal[pos]
        my_team$stam[i] <<- my_team$stam[i] - my_team$acc.stam[pos]
        my_team$tech[i] <<- my_team$tech[i] - my_team$acc.tech[pos]
      }
    }
  }
}

#Verifies that no two cards in the same strategy have the same order. This is run both after adding
#a new card and switching cards over to a different strategy.
verify.strat.order <- function(pos){
  red <- my_team[which(my_team$card.strategy=='red'),c('pos','card.strategy','strategy.order')]
  blue <- my_team[which(my_team$card.strategy=='blue'),c('pos','card.strategy','strategy.order')]
  green <- my_team[which(my_team$card.strategy=='green'),c('pos','card.strategy','strategy.order')]
  
  strats <- list(red,blue,green)
  
  #Iterates over the 3 strategies
  for(i in 1:3){
    #Counts how many cards in that strategy have the order "1", "2", or "3"
    one <- strats[[i]] %>% filter(strategy.order==1) %>% nrow() %>% unlist() %>% as.integer()
    two <- strats[[i]] %>% filter(strategy.order==2) %>% nrow() %>% unlist() %>% as.integer()
    three <- strats[[i]] %>% filter(strategy.order==3) %>% nrow() %>% unlist() %>% as.integer()
    
    #If any of the counts are more than 1, that's a problem and a suitable replacement needs to be identified.
    if(one > 1){
      if(strats[[i]]$pos[1]==pos){
        to.change <- strats[[i]]$pos[2]
      }
      else{
        to.change <- strats[[i]]$pos[1]
      }
      
      if(two == 0){
        my_team$strategy.order[to.change] <<- 2
      }
      else{
        my_team$strategy.order[to.change] <<- 3
      }
    }
    if(two > 1){
      if(strats[[i]]$pos[1]==pos){
        to.change <- strats[[i]]$pos[2]
      }
      else{
        to.change <- strats[[i]]$pos[1]
      }
      
      if(two == 0){
        my_team$strategy.order[to.change] <<- 1
      }
      else{
        my_team$strategy.order[to.change] <<- 3
      }
    }
    if(three > 1){
      if(strats[[i]]$pos[1]==pos){
        to.change <- strats[[i]]$pos[2]
      }
      else{
        to.change <- strats[[i]]$pos[1]
      }
      
      if(two == 0){
        my_team$strategy.order[to.change] <<- 1
      }
      else{
        my_team$strategy.order[to.change] <<- 2
      }
    }
  }
}

#Swaps the ordering of two cards already in the same strategy.
swap.strategy.order <- function(pos1,pos2){
  if(my_team$card.strategy[pos1] != my_team$card.strategy[pos2]){
    print("Can only swap appeal order among two cards already in the same strategy. If you would like to swap cards in different strategies, use the swap.strategies function.")
    stop(call. = F)
  }
  
  order1 <- my_team$strategy.order[pos1]
  order2 <- my_team$strategy.order[pos2]
  
  my_team$strategy.order[pos1] <<- order2
  my_team$strategy.order[pos2] <<- order1
  
  save_team()
  
  print("Order adjusted.")
}

#This will need to be updated when guests are added to the stat calculations.
swap.pos <- function(pos1,pos2){
  #-1 selects all columns except for the first, which is position.
  row1 <- my_team[pos1,-1]
  row2 <- my_team[pos2,-1]
  
  my_team[pos1,-1] <<- row2
  my_team[pos2,-1] <<- row1
  
  #There has to be a more efficient way to swap rows than this.
  
  row1 <- accessories.on.team[pos1]
  row2 <- accessories.on.team[pos2]
  accessories.on.team[pos1] <<- row2
  accessories.on.team[pos2] <<- row1
  
  row1 <- calc_appeal[pos1,-1]
  row2 <- calc_appeal[pos2,-1]
  calc_appeal[pos1,-1] <<- row2
  calc_appeal[pos2,-1] <<- row1
  
  row1 <- calc_critrate[pos1,-1]
  row2 <- calc_critrate[pos2,-1]
  calc_critrate[pos1,-1] <<- row2
  calc_critrate[pos2,-1] <<- row1
  
  row1 <- calc_critval[pos1,-1]
  row2 <- calc_critval[pos2,-1]
  calc_critval[pos1,-1] <<- row2
  calc_critval[pos2,-1] <<- row1
  
  row1 <- calc_damage[pos1,-1]
  row2 <- calc_damage[pos2,-1]
  calc_damage[pos1,-1] <<- row2
  calc_damage[pos2,-1] <<- row1
  
  row1 <- calc_skill[pos1,-1]
  row2 <- calc_skill[pos2,-1]
  calc_skill[pos1,-1] <<- row2
  calc_skill[pos2,-1] <<- row1
  
  row1 <- calc_sp[pos1,-1]
  row2 <- calc_sp[pos2,-1]
  calc_sp[pos1,-1] <<- row2
  calc_sp[pos2,-1] <<- row1
  
  row1 <- calc_stam[pos1,-1]
  row2 <- calc_stam[pos2,-1]
  calc_stam[pos1,-1] <<- row2
  calc_stam[pos2,-1] <<- row1
  
  row1 <- calc_tech[pos1,-1]
  row2 <- calc_tech[pos2,-1]
  calc_tech[pos1,-1] <<- row2
  calc_tech[pos2,-1] <<- row1
  
  row1 <- calc_volt[pos1,-1]
  row2 <- calc_volt[pos2,-1]
  calc_volt[pos1,-1] <<- row2
  calc_volt[pos2,-1] <<- row1
  
  #If the center is changed, then the guest's bonuses must be reapplied as well.
  if(1 %in% c(pos1,pos2)){
    guest.passive.apply()
    guest.insp.apply()  
  }
  
  
  save_team()
}

#Guests take their passives and inspiration skills and essentially "give" them to your team's center.
#For the sake of staying consistent with the game, guest stats will be applied when the song starts and not at this level.
add.team.guest <- function(table.num){
  
  my_team_guest <<- my_guests[table.num,!names(my_guests) %in% c('rarity','uncaps'),]
  
  guest.passive.apply()
  guest.insp.apply()
}

#Initializes the relevant columns pertaining the passive, live, and inspiration skills.
init_team_guest <- function(){
  my_team_guest <<- tibble('guest.name'="",'card.num' = NA,'card.name' = NA,
                          'pass.target' = NA, 'pass.stat' = NA, 'pass.val' = NA,
                          'live.cond' = NA, 'live.chance' = NA,
                          'live.effect' = NA, 'live.val' = NA, 'live.stat' = NA,
                          'live.dur' = NA, 'live.target' = NA, 'insp.slots' = NA,
                          'insp1' = NA, 'insp2' = NA, 'insp3' = NA, 'insp4' = NA)
  
  save.team.guest()
}

save.team.guest <- function(){
  db = dbConnect(SQLite(), dbname="C:/Users/Mike/Documents/R/SIFASteambuilder/SIFAS.sqlite")
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  dbWriteTable(db,"my_team_guest",my_team_guest,overwrite=T)
  dbDisconnect(db)
}

#Applies the passive skill from the guest. Works exactly the same way that the apply.passive function does.
guest.passive.apply <- function(){
  
  calc_appeal$pass.g <<- 0
  calc_stam$pass.g <<- 0
  calc_tech$pass.g <<- 0
  
  for(i in 1:9){
    if(my_team_guest$pass.target[1] == 'all'){
      update.stat(1,i,guest=T)
    }
    else if(my_team_guest$pass.target[1] == 'allies'){
      if(i != 1){
        update.stat(1,i,guest=T) 
      }
    }
    else if(my_team_guest$pass.target[1] == 'character'){
      if(my_team$idol[1] == my_team$idol[i]){
        update.stat(1,i,guest=T) 
      }
    }
    else if(my_team_guest$pass.target[1] == 'same element'){
      if(my_team$element[1] == my_team$element[i]){
        update.stat(1,i,guest=T) 
      }
    }
    else if(my_team_guest$pass.target[1] == 'same school'){
      match <- left_join(my_team,idol.data,by=c('idol'='idol'))
      if(match$school[1] == match$school[i]){
        update.stat(1,i,guest=T) 
      }
    }
    else if(my_team_guest$pass.target[1] == 'same strategy'){
      if(my_team$card.strategy[1] == my_team$card.strategy[i]){
        update.stat(1,i,guest=T) 
      }
    }
    else if(my_team_guest$pass.target[1] == 'same type'){
      if(my_team$type[1] == my_team$type[i]){
        update.stat(1,i,guest=T) 
      }
    }
    else if(my_team_guest$pass.target[1] == 'self'){
      if(i == 1){
        update.stat(1,i,guest=T) 
      }
    }
    else if(my_team_guest$pass.target[1] == 'same subunit'){
      match <- left_join(my_team,idol.data,by=c('idol'='idol'))
      if(match$subunit[1] == match$subunit[i]){
        update.stat(1,i,guest=T) 
      }
    }
    else if(my_team_guest$pass.target[1] == 'year'){
      match <- left_join(my_team,idol.data,by=c('idol'='idol'))
      if(match$year[1] == match$year[i]){
        update.stat(1,i,guest=T) 
      }
    }
  }
}

#Applies the guest's inspiration skills. Works exactly the same as the inspiration stat calculation for regular team members.
guest.insp.apply <- function(){
  calc_appeal$insp.g <<- 0
  calc_skill$guest <<- 0
  calc_skill$guest <<- 0
  calc_critval$guest <<- 0
  calc_sp$guest <<- 0
  calc_damage$guest <<- 0
  calc_volt$guest <<- 0
  
  #15 - 18 are the columns for the inspiration skills in the my_team_guest table.
  for(i in 15:18){
    row <- inspi.skills[which(inspi.skills$inspi.name==as.character(my_team_guest[1,i])),]
    if(nrow(row) != 0){
      if(identical(row$inspi.dur,"Permanent")){
        for(j in 1:9){
          if(row$inspi.target == 'all'){
            inspi.determine.stat(row,j,1,guest=T)
          }
          if(row$inspi.target == 'allies'){
            if(j == 1){
              inspi.determine.stat(row,j,1,guest=T)
            }
          }
          if(row$inspi.target == 'attribute'){
            if(my_team$element[1] == my_team$element[j]){
              inspi.determine.stat(row,j,1,guest=T)
            }
          }
          if(row$inspi.target == 'school'){
            match <- left_join(my_team,idol.data,by=c('idol'='idol'))
            if(identical(match$school[1],match$school[j])){
              inspi.determine.stat(row,j,1,guest=T)
            }
          }
          if(row$inspi.target == 'self' && row$inspi.effect != 'Type bonus'){
            if(j == 1){
              inspi.determine.stat(row,j,1,guest=T)
            }
          }
          if(row$inspi.target == 'strategy' || row$inspi.effect == 'Type bonus'){
            if(my_team$card.strategy[1] == my_team$card.strategy[j]){
              inspi.determine.stat(row,j,1,guest=T)
            }
          }
          if(row$inspi.target == 'type'){
            if(my_team$type[1] == my_team$type[j]){
              inspi.determine.stat(row,j,1,guest=T)
            }
          }
          if(row$inspi.target == 'type'){
            match <- left_join(my_team,idol.data,by=c('idol'='idol'))
            if(identical(match$year[i],match$year[k])){
              inspi.determine.stat(row,j,1,guest=T)
            }
          }
        }
      }
    }
  }
}


#Active2
add.member(226,"red",1,2)
add.member(52,"red",2,1)
add.member(146,"red",3,3)
add.member(48,"green",4,1)
add.member(109,"blue",5,2)
add.member(75,"blue",6,3)
add.member(12,"green",7,2)
add.member(178,"green",8,3)
add.member(4,"blue",9,1)
team.add.accessory(1,1)
team.add.accessory(2,2)
team.add.accessory(7,3)
team.add.accessory(4,4)
team.add.accessory(8,7)
team.add.accessory(13,8)
team.add.accessory(30,5)
team.add.accessory(3,6)
team.add.accessory(11,9,T)

View(my_team)
View(my_guests)
View(my_team_guest)

sum(my_team$stam)
swap.strategies(4,5)

init_team_guest()
add.team.guest(11)

View(calc_appeal)
