library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

translation = c("Numero_observation", 
                "Code_postal_etablissement", 
                "Espece", 
                "Nombre_individus", 
                "Humidite_sol",
                "Difficulte_enfoncer_crayon",
                "Environnement")
input = c("Zon", "Zip", "Esp", "Ind", "Hum", "Dif", "Env")

EquivalenceVar <- data.frame(translation, input)

# Function for operation ----
mean2 <- function(x) {
  mean(x, na.rm = TRUE)
}

sd2 <- function(x) {
  sd(x, na.rm = TRUE)
}

sum2 <- function(x) {
  sum(x, na.rm = TRUE)
}

lengthSupZero <- function(x) {
  length(x[x>0])
}


translation = c("mean2", 
                "sd2", 
                "sum2",
                "length",
                "lengthSupZero")
input = c("Mo", "Ec", "So","Co","No")

EquivalenceFun <- data.frame(translation, input)

# import data for  the game
getDataInitial <- function(directory = "data/"){
  # Upload complete data Earth Worm
  jeuDeDonnees <- fread(paste0(directory,"VersDeTerre.csv"))
  # add placette (to keep the number of row)
  Placette <- rep(c(1,2,3),nrow(jeuDeDonnees)/3)
  jeuDeDonnees$Placette <- Placette
  # Reduce columns nomber and add juveniles and adults
  jeuDeDonneesReduction <- jeuDeDonnees %>%
    rename(Numero_observation = numero_observation, 
           Code_postal = code_postal_etablissement,
           Nombre_individus = nb_ind,
           Humidite_sol = humidite_sol,
           Environnement = environnement,
           Difficulte_enfoncer_crayon = difficulte_enfoncer_crayon) %>%
    mutate(Espece = str_remove_all(sp, fixed(" (juvénile)")))  %>%
    mutate(Espece = str_remove_all(Espece, fixed("s"))) %>%
    group_by(Numero_observation,
             Code_postal,
             Espece,
             Humidite_sol,
             Difficulte_enfoncer_crayon,
             Environnement,
             Placette) %>%
    summarise(Nombre_individus = sum2(Nombre_individus))
  
  #Remplacer NA par 0
  jeuDeDonneesReduction[is.na(jeuDeDonneesReduction[ , "Nombre_individus"]) , "Nombre_individus"] <- 0
  
  # pour l'ordre dans les futurs graphiques
  #Turn your 'treatment' column into a character vector
  jeuDeDonneesReduction$Environnement <- as.character(jeuDeDonneesReduction$Environnement)
  #Then turn it back into a factor with the levels in the correct order
  jeuDeDonneesReduction$Environnement <- factor(jeuDeDonneesReduction$Environnement, levels=c("Rural", "Péri-urbain", "Urbain"))
  
  return(jeuDeDonneesReduction)
}

# functions for the game ----

# [deprecated]
# get the information for the tool separated
separateTools <- function (code, sep = ":"){
  # get tool names and operation as separated string
  whereToCut <- gregexpr("([A-Z])(?![a-z])", code, perl = TRUE)
  tools <-  c()
  if (length(whereToCut[[1]]) >  1){
    for (i in 1:(length(whereToCut[[1]])-1)) {
      tools[i] <- substr(code, whereToCut[[1]][[i]], whereToCut[[1]][[i+1]]-1)
    }
    tools[length(whereToCut[[1]])] <- substr(code, whereToCut[[1]][[length(whereToCut[[1]])]], nchar(code))
    tools
  } else {
    code
  }
}

# get the 'arguments' of each tool
separateParametersTreatment <- function (code){
  #remove tool id
  code <- substr(code, 2, nchar(code))
  # separate input, operation and output
  whereToCut <- gregexpr("([A-Z])(?![a-z][a-z])", code, perl = TRUE)
  inputCol <- substr(code, 1, whereToCut[[1]][[1]]-1)
  operation <- substr(code, whereToCut[[1]][[1]], whereToCut[[1]][[1]]+1)
  outputCol <- substr(code, whereToCut[[1]][[1]]+2, nchar(code))
  # separate input
  inputColTranslated <- regmatches(inputCol, gregexpr("([A-Z][a-z][a-z])", inputCol,perl = TRUE))
  outputCol
  informationR <- list(inputColTranslated, outputCol, operation)
  informationR
}

# Treatments
randomAll <- function(df) {
  df2 <- df %>%
    mutate_all(as.character)
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      df2[i,j] <- df[sample(nrow(df), size = 1), sample(ncol(df), size = 1)]
    }
  }
  df2
}

correspond <- function (input, reference){
  inputDf <-  data.frame(input)
  colnames(inputDf) <- "input"
  inputColumns <- inner_join(inputDf, reference,by="input")
  as.character(inputColumns$translation)
}

# [deprecated?]
printPlot <- function (x) {
  plot(1, 1, type = "n", axes = FALSE, ann = FALSE)
  text(1, 1, x)
}

# Function for handling shiny tags ----

#' @title uniteTags
#' 
#' @description takes a list of HTML tags (type: list; class: shiny.tags)
#' and return a single shiny tag concatening all others
uniteTags <- function(tags.list){
  tagString <- ""
  sapply(tags.list, function(tag){
    tagString <<- HTML(as.character(tagString),
                      as.character(tag))
  })
  # browser()
  tagString <- tagList(tagString)
  return(tagString)
}

