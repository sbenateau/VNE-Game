library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(tidyr)
library(sf)
library(tmap)
library(rlang)

# load correspondance between code and variables and functions
EquivalenceVar <- read.csv("data/EquivalenceVar.csv", sep = ",", row.names = 1)
EquivalenceFun <- read.csv("data/EquivalenceFun.csv", sep = ",", row.names = 1)


# Function for operation ----
#' @title mean2
#' 
mean2 <- function(x) {
  mean(x, na.rm = TRUE)
}

#' @title sd2
#' 
sd2 <- function(x) {
  sd(x, na.rm = TRUE)
}

#' @title sum2
#' 
sum2 <- function(x) {
  sum(x, na.rm = TRUE)
}

#' @title median2
#' 
median2 <- function(x) {
  median(x, na.rm = TRUE)
}

#' @title lengthSupZero
#' 
lengthSupZero <- function(x) {
  length(x[x>0])
}

#' @title se
#' 
se <- function(x){
  x2 <- na.omit(x)
  sd(x2)/sqrt(length(x2))
}


# import data for  the game
getDataInitial <- function(directory = "data/", observatory){
  if (observatory == "Vdt"){
    # Upload complete data Earth Worm
    jeuDeDonnees <- data.table::fread(paste0(directory,"VersDeTerre.csv"))
    
    # add placette (to keep the number of row)
    Placette <- rep(c("1","2","3"),nrow(jeuDeDonnees)/3)
    jeuDeDonnees$Placette <- Placette
    # Reduce columns nomber and add juveniles and adults
    jeuDeDonneesReduction <- jeuDeDonnees %>%
      dplyr::rename(Numero_observation = numero_observation,
                    Code_postal = code_postal_etablissement,
                    Longitude = longitude,
                    Latitude = latitude,
                    Nombre_individus = nb_ind,
                    Humidite_sol = humidite_sol,
                    Environnement = environnement,
                    Difficulte_enfoncer_crayon = difficulte_enfoncer_crayon,
                    Temperature = temperature_durant_obs) %>%
      dplyr::mutate(Espece = stringr::str_remove_all(sp, fixed(" (juvénile)")))  %>%
      dplyr::mutate(Espece = stringr::str_remove_all(Espece, fixed("s"))) %>%
      dplyr::mutate(Departement = as.factor(substr(Code_postal, 0, 2))) %>%
      dplyr::group_by(Numero_observation,
                      Placette,
                      Espece,
                      Environnement,
                      Humidite_sol,
                      Difficulte_enfoncer_crayon,
                      Temperature,
                      Code_postal,
                      Departement,
                      Longitude,
                      Latitude) %>%
      dplyr::summarise(Nombre_individus = sum2(Nombre_individus))
    
    
    # reorder columns
    colnamesDf <- colnames(jeuDeDonneesReduction) 
    jeuDeDonneesReduction <- jeuDeDonneesReduction[c(colnamesDf[1:3],colnamesDf[length(colnamesDf)],colnamesDf[-c(1:3, length(colnamesDf))])]
    
    #Remplacer NA par 0
    jeuDeDonneesReduction[is.na(jeuDeDonneesReduction[ , "Nombre_individus"]) , "Nombre_individus"] <- 0
    
    # pour l'ordre dans les futurs graphiques
    #Turn your 'treatment' column into a character vector
    #Then turn it back into a factor with the levels in the correct order
    jeuDeDonneesReduction$Environnement <- factor(as.character(jeuDeDonneesReduction$Environnement), levels=c("Rural", "Péri-urbain", "Urbain"))
    jeuDeDonneesReduction$Humidite_sol <- factor(as.character(jeuDeDonneesReduction$Humidite_sol), levels=c("engorge", "humide", "sec", ""))
    levels(jeuDeDonneesReduction$Humidite_sol)[4] <- "Donnees_manquantes"
    jeuDeDonneesReduction$Difficulte_enfoncer_crayon <- factor(as.character(jeuDeDonneesReduction$Difficulte_enfoncer_crayon), levels=c("tres_facile", "facile", "peu_difficile", "difficile", ""))
    levels(jeuDeDonneesReduction$Difficulte_enfoncer_crayon)[5] <- "Donnees_manquantes"
    return(jeuDeDonneesReduction)
  }
  else if (observatory == "Ois") {
    # Upload complete data Earth Worm
    jeuDeDonnees <- data.table::fread(paste0(directory,"Oiseaux2.csv"))
    jeuDeDonneesReduction <- jeuDeDonnees %>%
      dplyr::select(Numero_observation = numero_observation,
                    Espece = sp,
                    Nombre_individus = nb_ind,
                    Date=date_obs,
                    Code_postal = code_postal_etablissement,
                    Environnement = environnement,
                    #Longitude = longitude,
                    #Latitude = latitude,
                    #Humidite_sol = humidite_sol,
                    Surface_zone = surface_zone,
                    Distance_bois = distance_bois,
                    Distance_prarie = distance_prairie,
                    Distance_champs = distance_champ
                    #Difficulte_enfoncer_crayon = difficulte_enfoncer_crayon,
                    #Temperature = temperature_durant_obs
      )%>%
      dplyr::mutate(Departement = as.factor(substr(Code_postal, 0, 2)))
    jeuDeDonneesReduction <- jeuDeDonneesReduction[ , -5]
    jeuDeDonneesReduction
  } else if (observatory == "Esc"){
    jeuDeDonnees <- data.table::fread(paste0(directory,"Escargot.csv"))
    jeuDeDonneesReduction <- jeuDeDonnees %>%
      dplyr::select(Numero_observation = numero_observation,
                    Numero_planche = num_Planche,
                    Espece = sp,
                    Nombre_individus = nb_ind,
                    Date=date_obs,
                    Code_postal = code_postal_etablissement,
                    Environnement = environnement,
                    #Longitude = longitude,
                    #Latitude = latitude,
                    #Humidite_sol = humidite_sol,
                    Surface_zone = Surface,
                    Distance_bois = distance_bois,
                    Distance_prarie = distance_prairie,
                    Distance_champs = distance_champ
                    #Difficulte_enfoncer_crayon = difficulte_enfoncer_crayon,
                    #Temperature = temperature_durant_obs
      )%>%
      dplyr::mutate(Departement = as.factor(substr(Code_postal, 0, 2)))
    jeuDeDonneesReduction <- jeuDeDonneesReduction[ , -5]
    jeuDeDonneesReduction
  }
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

#' @title separateParametersTreatment
#' 
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

#' @title randomAll
#' 
#' @description randomize all columns of a data.frame
#' @param df An input data.frame
randomAll <- function(df) {
  df2 <- df %>%
    dplyr::mutate_all(as.character)
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      df2[i,j] <- df[sample(nrow(df), size = 1), sample(ncol(df), size = 1)]
    }
  }
  df2
}


#' @title correspond
#' 
#' @description translate codes to variables or function
#' @param input A vector of the input code
#' @param reference A data.frame table that contains the correspondance between the code and the variable or function
#' 
correspond <- function (input, reference){
  inputDf <-  data.frame(input)
  colnames(inputDf) <- "input"
  inputColumns <- dplyr::inner_join(inputDf, reference, by="input")
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


#' @title codeInformation
#' 
#' @description parse the input code and gives information on the tools,
#' the variables and the function used
#' @param fullCode a string containing the input code from the user
codeInformation <- function (fullCode){
  AllTools <- unlist(strsplit(fullCode, ":"))
  AllToolsNames <- unlist(lapply(AllTools, function(x) str_sub(x,1,1)))
  toolUsed <- AllToolsNames[!sapply(AllToolsNames, function (x) x == "S")]
  # variables utilisées
  varUsed <-unlist(stringr::str_extract_all(AllTools, "[A-Z][a-z][a-z]"))
  # fonctions utilisées
  funUsed <-unlist(stringr::str_extract_all(AllTools, "[A-Z][a-z][A-Z]"))
  funUsed <- str_sub(funUsed, start = 1, end = 2)
  
  informations <- list(toolUsed, varUsed, funUsed)
  names(informations) <- c("toolUsed", "varUsed", "funUsed")
  informations
}