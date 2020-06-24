url.in <- "http://kachagain.com/sifas/inspiskills.php"

inspi <- read_html(url.in,encoding='UTF-8') %>%
  html_nodes(xpath='//*[@id="skills"]') %>%
  html_nodes("div.skill-table") %>%
  html_text()


inspi.num <- rep(NA,length(inspi))
inspi.name <- rep(NA,length(inspi))
inspi.effect <- rep(NA,length(inspi))
inspi.val <- rep(NA,length(inspi))
inspi.dur <- rep(NA,length(inspi))
inspi.target <- rep(NA,length(inspi))
inspi.cond <- rep(NA,length(inspi))
inspi.chance <- rep(NA,length(inspi))

#The part of the string where all of the skill info is.
inspi.skill.part <- str_split(inspi,"(Incr|If)",simplify=T)[,2]

for(i in 1:length(inspi)) {
  inspi.num[i] <- i
  inspi.name[i] <- get.inspi.name(inspi[i])
  inspi.effect[i] <- get.inspi.effect(inspi.skill.part[i])
  inspi.val[i] <- get.inspi.val(inspi.skill.part[i])
  #Kach's site has a typo here and wrote ".5" instead of ".5%"
  if(i == 208) {
    inspi.val[i] <- inspi.val[i]/100
  }
  inspi.dur[i] <- get.inspi.dur(inspi.skill.part[i])
  inspi.target[i] <- get.inspi.target(inspi.skill.part[i])
  inspi.cond[i] <- get.inspi.cond(inspi.skill.part[i])
  inspi.chance[i] <- get.inspi.chance(inspi.skill.part[i])
  
  inspi.skills <- tibble(inspi.num,inspi.name,inspi.effect,inspi.val,inspi.dur,inspi.target,
                         inspi.cond,inspi.chance)
}

get.inspi.name <- function(inspi) {
  inspi %>%
    str_extract("([:upper:]{1}|SP[:blank:])[:lower:]([:graph:]|[:blank:])+](:[:blank:])?([:digit:]+%)?([:upper:]{2})?([:blank:]|/|[:lower:])*") %>%
    return()
}


#"Appeal+" - "Appeal stat"
#"Critical value" - "Critical value"
#"Skill activation rate" - "Skill activation"
#"Type bonus" - "Type bonus"
#"SP Skill Up" - "Voltage boost of the next SP"
#       "Voltage boost" - "Voltage boost"
#"Stamina recovery" - "recovers"
#"Shield" - "shield"
#"Combo Count UP" - "combo count"
#"Voltage Up" - "Voltage gain"
#"Damage reduction" - "reduces damage"
#"Critical rate" - "Critical rate"
get.inspi.effect <- function(inspi) {
  if(str_detect(inspi,"(Appeal stat|Appeal by)")) {
    return("Appeal+")
  }
  else if(str_detect(inspi,"Critical value")) {
    return("Critical value")
  }
  else if(str_detect(inspi,"skill activation")) {
    return("Skill activation rate")
  }
  else if(str_detect(inspi,"type bonus")) {
    return("Type bonus")
  }
  else if(str_detect(inspi,"boost of the next SP")) {
    return("SP Skill Up")
  }
  else if(str_detect(inspi,"Voltage boost")) {
    return("Voltage boost")
  }
  else if(str_detect(inspi,"recovers")) {
    return("Stamina recovery")
  }
  else if(str_detect(inspi,"shield")) {
    return("Shield")
  }
  else if(str_detect(inspi,"combo count")) {
    return("Combo Count Up")
  }
  else if(str_detect(inspi,"Voltage gain")) {
    return("Voltage Up")
  }
  else if(str_detect(inspi,"reduces damage")) {
    return("Damage reduction")
  }
  else if(str_detect(inspi,"Critical rate")) {
    return("Critical rate")
  }
}

get.inspi.val <- function(inspi) {
  to.return <- inspi %>%
    str_extract("(by |equal to |recovers )[:digit:]+(.)?([:digit:])*(%)?") %>% 
    str_extract("[:digit:]+(.)?([:digit:])*(%)?")
  if(str_detect(to.return,"%")) {
    to.return %>%
      str_extract("[^%]+") %>%
      as.numeric() %>%
      divide_by(100) %>%
      return()
  }
  else{
    to.return %>%
      as.numeric() %>%
      return()
  }
}


#5 notes = "5 notes"
#Remainder = "remainder of the song"
#Permanent = "eases" - comes from "Increases"
#else return NA
get.inspi.dur <- function(inspi) {
  if(str_detect(inspi,"5 notes")) {
    return("5 notes")
  }
  #"game" and "song" are both used at the end because ???
  else if(str_detect(inspi,"remainder of the")) {
    return("Remainder")
  }
  else if(str_detect(inspi,"^eases ")) {
    return("Permanent")
  }
  else {
    return(NA)
  }
}

get.inspi.target <- function(inspi) {
  if(str_detect(inspi,"same attribute")) {
    return("attribute")
  }
  else if(str_detect(inspi,"same type")) {
    return("type")
  }
  else if(str_detect(inspi,"same school")) {
    return("school")
  }
  else if(str_detect(inspi,"(s own|on self)")) {
    return("self")
  }
  else if(str_detect(inspi,"same subunit")) {
    return("strategy")
  }
  else if(str_detect(inspi,"all members")) {
    return("all")
  }
  else if(str_detect(inspi,"same year")) {
    return("year")
  }
  else if(str_detect(inspi,"all other")) {
    return("allies")
  }
  else {
    return(NA)
  }
}

get.inspi.cond <- function(inspi) {
  if(str_detect(inspi,"beginning of Appeal")) {
    return("AC Start")
  }
  else if(str_detect(inspi,"target Voltage")) {
    return("30% Target")
  }
  else if(str_detect(inspi,"under 80%")) {
    return("80% Stam")
  }
  else if(str_detect(inspi,"beginning of the song")) {
    return("Song Start")
  }
  else if(str_detect(inspi,"completing Appeal")) {
    return("AC Clear")
  }
  else{
    return("Permanent")
  }
}

get.inspi.chance <- function(inspi){
  if(!str_detect(inspi,"\\([:digit:]+%")) {
    return(1)
  }
  else{
    inspi %>%
      str_extract("\\([:digit:]+%") %>% 
      str_remove("\\(") %>% 
      str_remove("%") %>% 
      as.numeric() %>% 
      divide_by(100) %>%
      return()
  }
}

#Some of the inspiration skills use a weird plus sign that makes searching for skills difficult.
replace.plus <- function(){
  # U+FF0B is the unicode character of the full-width plus sign. For some reason,
  #trying to locate the character without using the unicode value doesn't work.
  # U+002B is the unicode character for a normal plus sign.
  for(i in 1:nrow(inspi.skills)){
    if(str_detect(inspi.skills$inspi.name[i],"\uFF0B")) {
      inspi.skills$inspi.name[i] <<- str_replace(inspi.skills$inspi.name[i],"\uFF0B","\u002B")
    }
    if(str_detect(inspi.skills$inspi.effect[i],"\uFF0B")) {
      inspi.skills$inspi.effect[i] <<- str_replace(inspi.skills$inspi.effect[i],"\uFF0B","\u002B")
    }
  }
  
  db = dbConnect(SQLite(), dbname="SIFAS.sqlite")
  dbWriteTable(db,"inspi_skills",inspi.skills,overwrite=T)
  dbDisconnect(db)
}

replace.plus()

