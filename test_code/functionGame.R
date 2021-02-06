library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(tidyr)
library(sf)
library(tmap)
library(rlang)
library(purrr)

# load correspondance between code and variables and functions
# If you want to add functions or variable, you have to fill these documents
EquivalenceVar <- read.csv("data/EquivalenceVar.csv", sep = ",", row.names = 1)
EquivalenceFun <- read.csv("data/EquivalenceFun.csv", sep = ",", row.names = 1)


# Function for operation ----

# Modification of the functions to handle NA values and round to one digit
#' @title mean2
#'
#' @param x a numeric vector
mean2 <- function(x) {
  round(mean(x, na.rm = TRUE), 1)
}

#' @title sd2
#'
#' @param x a numeric vector
sd2 <- function(x) {
  sd(x, na.rm = TRUE)
}

#' @title sum2
#'
#' @param x a numeric vector
sum2 <- function(x) {
  sum(x, na.rm = TRUE)
}

#' @title median2
#'
#' @param x a numeric vector
median2 <- function(x) {
  median(x, na.rm = TRUE)
}

p <- c(0.25, 0.5, 0.75)
p_names <- map_chr(p, ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
  set_names(nm = p_names)



#' @title lengthSupZero
#'
#' @param x a numeric vector
lengthSupZero <- function(x) {
  length(x[x>0])
}

length_unique <- function(x) {
  length(unique(na.omit(x)))
}

#' @title se
#'
#' @param x a numeric vector
confidence_interval <- function(x) {
  x2 <- na.omit(x)
  1.96*sd(x2)/sqrt(length(x2))
}


# import data for  the game
getDataInitial <- function(data_values, observatory) {
  if (observatory == "Vdt") {
    jeuDeDonnees <- data_values[["vdt"]]
    
    #   # reorder columns
    #   colnamesDf <- colnames(jeuDeDonneesReduction)
    #   jeuDeDonneesReduction <- jeuDeDonneesReduction[c(colnamesDf[1:3],colnamesDf[length(colnamesDf)],colnamesDf[-c(1:3, length(colnamesDf))])]
    #   
    #   # pour l'ordre dans les futurs graphiques
    #   #Turn your 'treatment' column into a character vector
    #   #Then turn it back into a factor with the levels in the correct order
    #   jeuDeDonneesReduction$Environnement <- factor(as.character(jeuDeDonneesReduction$Environnement), levels=c("Rural", "Péri-urbain", "Urbain"))
    #   jeuDeDonneesReduction$Humidite_sol <- factor(as.character(jeuDeDonneesReduction$Humidite_sol), levels=c("engorge", "humide", "sec", ""))
    #   levels(jeuDeDonneesReduction$Humidite_sol)[4] <- "Donnees_manquantes"
    #   jeuDeDonneesReduction$Difficulte_enfoncer_crayon <- factor(as.character(jeuDeDonneesReduction$Difficulte_enfoncer_crayon), levels=c("tres_facile", "facile", "peu_difficile", "difficile", ""))
    #   levels(jeuDeDonneesReduction$Difficulte_enfoncer_crayon)[5] <- "Donnees_manquantes"
    #   return(jeuDeDonneesReduction)
    jeuDeDonnees
  }
  else if (observatory == "Ois") {
    jeuDeDonnees <- data_values[["oiseaux"]]
    
    #   # Upload complete data for  birds
    #   jeuDeDonnees <- data.table::fread(paste0(directory,"Oiseaux2.csv"))
    #   jeuDeDonneesReduction <- jeuDeDonnees %>%
    #     dplyr::select(Numero_observation = numero_observation,
    #                   Espece = sp,
    #                   Nombre_individus = nb_ind,
    #                   Date=date_obs,
    #                   Code_postal = code_postal_etablissement,
    #                   Environnement = environnement,
    #                   #Longitude = longitude,
    #                   #Latitude = latitude,
    #                   #Humidite_sol = humidite_sol,
    #                   Surface_zone = surface_zone,
    #                   Distance_bois = distance_bois,
    #                   Distance_prarie = distance_prairie,
    #                   Distance_champs = distance_champ
    #                   #Difficulte_enfoncer_crayon = difficulte_enfoncer_crayon,
    #                   #Temperature = temperature_durant_obs
    #     ) %>%
    #     dplyr::mutate(Departement = as.factor(substr(Code_postal, 0, 2)))
    #   jeuDeDonneesReduction <- jeuDeDonneesReduction[ , -5]
    #   jeuDeDonneesReduction$Environnement   <- factor(as.character(jeuDeDonneesReduction$Environnement), levels=c("Rural", "Péri-urbain", "Urbain"))
    #   jeuDeDonneesReduction$Distance_bois   <- factor(as.character(jeuDeDonneesReduction$Distance_bois), levels=c("moins de 50 m", "50 à 500 m", "501 à 1000 m", "1001 m à 2000 m", "au-delà de 2 km", "Non renseigné", ""))
    #   jeuDeDonneesReduction$Distance_bois[jeuDeDonneesReduction$Distance_bois ==""] <- "Non renseigné"
    #   jeuDeDonneesReduction$Distance_prarie <- factor(as.character(jeuDeDonneesReduction$Distance_prarie), levels=c("moins de 50 m", "50 à 500 m", "501 à 1000 m", "1001 m à 2000 m", "au-delà de 2 km", "Non renseigné", ""))
    #   jeuDeDonneesReduction$Distance_prarie[jeuDeDonneesReduction$Distance_prarie ==""] <- "Non renseigné"
    #   jeuDeDonneesReduction$Distance_champs <- factor(as.character(jeuDeDonneesReduction$Distance_champs), levels=c("moins de 50 m", "50 à 500 m", "501 à 1000 m", "1001 m à 2000 m", "au-delà de 2 km", "Non renseigné", ""))
    #   jeuDeDonneesReduction$Distance_champs[jeuDeDonneesReduction$Distance_champs ==""] <- "Non renseigné"
    #   levels(jeuDeDonneesReduction$Departement)[levels(jeuDeDonneesReduction$Departement) =="  "] <- "Non renseigné"
    #   jeuDeDonneesReduction
    jeuDeDonnees
  } else if (observatory == "Esc") {
    jeuDeDonnees <- data_values[["escargots"]]
    #   jeuDeDonnees <- data.table::fread(paste0(directory,"Escargot.csv"))
    #   jeuDeDonneesReduction <- jeuDeDonnees %>%
    #     dplyr::select(Numero_observation = numero_observation,
    #                   Numero_planche = num_Planche,
    #                   Espece = sp,
    #                   Nombre_individus = nb_ind,
    #                   Date=date_obs,
    #                   Code_postal = code_postal_etablissement,
    #                   Environnement = environnement,
    #                   #Longitude = longitude,
    #                   #Latitude = latitude,
    #                   #Humidite_sol = humidite_sol,
    #                   Surface_zone = Surface,
    #                   Distance_bois = distance_bois,
    #                   Distance_prarie = distance_prairie,
    #                   Distance_champs = distance_champ
    #                   #Difficulte_enfoncer_crayon = difficulte_enfoncer_crayon,
    #                   #Temperature = temperature_durant_obs
    #     ) %>%
    #     dplyr::mutate(Departement = as.factor(substr(Code_postal, 0, 2)))
    #   jeuDeDonneesReduction <- jeuDeDonneesReduction[ , -5]
    #   jeuDeDonneesReduction
  } else if (observatory == "Sau") {
    jeuDeDonnees <- data_values[["sauvages"]]
  }
}


# remove 01_ from factors but keeps order
removeBeginingCategories <- function (input, Column){
  input <- data.frame(input)
  if (Column != "None"){
    Column <- as.numeric(Column)
    if(sapply(input[Column], class) == "factor" | sapply(input[Column], class) == "character"){
      if(is.character(sapply(input[Column], class))){
        input[ , Column] <- as.factor(input[ , Column])
      }
      levelsColumn <- levels(input[ , Column])
      if(any(grepl(pattern = "^[0-9][0-9]_", levelsColumn))){
        # lock order
        LevelToChange = grep(pattern = "^[0-9][0-9]_", levelsColumn)
        levelsColumn[LevelToChange] = substr(levelsColumn[LevelToChange], 4, nchar(levelsColumn[LevelToChange]))
        levels(input[ , Column]) <- levelsColumn
      }
    }
  }
  input
}

nice_column_names <- function(x) {
  switch (x,
          Espece = "Espèce",
          Nombre_individus = "Nombre d'individus",
          Region = "Région",
          Academie = "Académie",
          Annee = "Année",
          Type_de_milieu = "Type de milieu",
          Surface_zone = "Surface de la zone",
          Distance_bois = "Distance au bois le plus proche",
          Distance_prairie = "Distance à la prairie la plus proche",
          Distance_champ = "Distance au champ le plus proche",
          diversite_moyenne = "Diversité moyenne",
          nombre_moyen_individus = "Nombre moyen d'individus",
          Humidite_sol_lors_observation = "Humidite du sol lors de l'observation",
          Difficulte_enfoncer_crayon = "Difficulté à enfoncer un crayon",
          Taupinieres = "Taupinières",
          Surface_zone = "Surface de la zone",
          Longueur_rue = "Longueur de la rue",
          nombre_observations = "Nombre d'observations",
          x
  )
}


# functions for the game ----

# [deprecated]
# get the information for the tool separated
separateTools <- function (code, sep = ":") {
  # get tool names and operation as separated string
  whereToCut <- gregexpr("([A-Z])(?![a-z])", code, perl = TRUE)
  tools <-  c()
  if (length(whereToCut[[1]]) >  1) {
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
separateParametersTreatment <- function (code) {
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
  for (i in 1:nrow(df)) {
    for (j in 1:ncol(df)) {
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
correspond <- function (input, reference) {
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
#' and return a single shiny tag concatenating all others
uniteTags <- function(tags.list) {
  tagString <- ""
  sapply(tags.list, function(tag) {
    tagString <<- HTML(as.character(tagString),
                       as.character(tag))
  })
  # browser()
  tagString <- tagList(tagString)
  return(tagString)
}

# Make abundance according to one variable

abundanceCard <- function (dataset, group_variable = character(0)) {
  
  if (identical(group_variable, character(0))) {
    res <- dataset %>%
      dplyr::group_by(Numero_observation) %>%
      dplyr::summarise(
        abondance = sum2(Nombre_individus)) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(
        somme_abondance = sum2(abondance),
        nombre_participation = length(abondance),
        nombre_moyen_individus = mean2(abondance),
        intervalle_de_confiance = confidence_interval(abondance))
    
    resCalc = res[ , c("nombre_moyen_individus", "intervalle_de_confiance")]
    resUI = res[ , c("somme_abondance", "nombre_participation", "nombre_moyen_individus")]
    resUI$nombre_moyen_individus <- paste("<b>",resUI$nombre_moyen_individus,"</b>")
    colnames(resUI) = c("Somme de l'abondance", "Nombre de protocoles réalisés", "Nombre moyen d'individus")
  } else if (group_variable %in% c("Latitude", "Longitude", "Longueur_rue")) {
    res <- dataset %>%
      dplyr::group_by_at(c("Numero_observation", group_variable)) %>%
      dplyr::summarise(abondance = sum2(Nombre_individus)) %>%
      dplyr::ungroup()
    
    resCalc = res[ , c(group_variable, "abondance")]
    resUI = res[ , c(group_variable, "abondance")]
    resUI$abondance <- paste("<b>",resUI$abondance,"</b>")
    colnames(resUI) = c(group_variable, "Abondance")
  } else {
    res <- dataset %>%
      dplyr::group_by_at(c("Numero_observation", group_variable)) %>%
      dplyr::summarise(abondance = sum2(Nombre_individus)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by_at(group_variable) %>%
      dplyr::summarise(
        somme_abondance = sum2(abondance),
        nombre_participation = length(abondance),
        nombre_moyen_individus = mean2(abondance),
        intervalle_de_confiance = confidence_interval(abondance)) %>%
      ungroup()
    
    resCalc = res[ , c(group_variable, "nombre_moyen_individus", "intervalle_de_confiance")]
    resUI = res[ , c(group_variable, "somme_abondance", "nombre_participation", "nombre_moyen_individus")]
    resUI$nombre_moyen_individus <- paste("<b>",resUI$nombre_moyen_individus,"</b>")
    colnames(resUI) = c(group_variable, "Somme de l'abondance", "Nombre de protocoles réalisés", "Nombre moyen d'individus")
  }
  result_list <- list(resCalc, resUI)
  return(result_list)
}

diversityCard <- function (dataset, group_variable = character(0)) {
  if ("Epigé" %in% unique(dataset$Espece)){
    dataset = subset(dataset, select = -c(Num_quadrat))
    dataset <- dataset %>% 
      group_by(.dots = names(dataset)[-grep("Nombre_individus", names(dataset))]) %>%
      summarise(Nombre_individus = sum2(Nombre_individus))
  }
  
  if ("Plantain majeur" %in% unique(dataset$Espece)){
    protocole = "sauvages"
  } else {
    protocole = "defaut"
  }
  
  if (identical(group_variable, character(0))) {
    if (protocole == "sauvages"){
      res <- dataset %>%
        dplyr::group_by(Numero_observation) %>%
        dplyr::summarise(Diversite = length_unique(Espece))
    } else {
      res <- dataset %>%
        dplyr::group_by(Numero_observation) %>%
        dplyr::summarise(Diversite = lengthSupZero(Nombre_individus))
    }
    
    res <- res %>%
      dplyr::ungroup() %>%
      dplyr::summarise(
        somme_diversite = sum(Diversite, na.rm = TRUE),
        nombre_participation = length(Diversite),
        diversite_moyenne = mean2(Diversite),
        intervalle_de_confiance = confidence_interval(Diversite))
    
    resCalc = res[ , c("diversite_moyenne", "intervalle_de_confiance")]
    resUI = res[ , c("somme_diversite", "nombre_participation", "diversite_moyenne")]
    resUI$diversite_moyenne <- paste("<b>",resUI$diversite_moyenne,"</b>")
    colnames(resUI) = c("Somme des espèces observées", "Nombre de protocoles réalisés", "Diversité Moyenne")
  } else if (group_variable %in% c("Latitude", "Longitude", "Longueur_rue")) {
    if (protocole == "sauvages"){
      res <- dataset %>%
        dplyr::group_by_at(c("Numero_observation", group_variable)) %>%
        dplyr::summarise(Diversite = length_unique(Espece)) %>%
        dplyr::ungroup()
    } else {
      res <- dataset %>%
        dplyr::group_by_at(c("Numero_observation", group_variable)) %>%
        dplyr::summarise(Diversite = lengthSupZero(Nombre_individus)) %>%
        dplyr::ungroup()
    }
    
    resCalc = res[ , c(group_variable, "Diversite")]
    resUI = res[ , c(group_variable, "Diversite")]
    resUI$abondance <- paste("<b>",resUI$Diversite,"</b>")
    colnames(resUI) = c(group_variable, "Diversite")
  } else {
    if (protocole == "sauvages"){
      res <- dataset %>%
        dplyr::group_by_at(c("Numero_observation", group_variable)) %>%
        dplyr::summarise(Diversite = length_unique(Espece))
    } else {
      res <- dataset %>%
        dplyr::group_by_at(c("Numero_observation", group_variable)) %>%
        dplyr::summarise(Diversite = lengthSupZero(Nombre_individus))
    }
    
    res <- res %>%
      dplyr::group_by_at(group_variable) %>%
      dplyr::summarise(
        somme_diversite = sum(Diversite, na.rm = TRUE),
        nombre_participation = length(Diversite),
        diversite_moyenne = mean2(Diversite),
        intervalle_de_confiance = confidence_interval(Diversite))
    
    resCalc = res[ , c(group_variable, "diversite_moyenne", "intervalle_de_confiance")]
    
    resUI = res[ , c(group_variable, "somme_diversite", "nombre_participation", "diversite_moyenne")]
    resUI$diversite_moyenne <- paste("<b>",resUI$diversite_moyenne,"</b>")
    colnames(resUI) = c(group_variable, "Somme des espèces observées", "Nombre de protocoles réalisés", "Diversité Moyenne")
    
  }
  result_list <- list(resCalc, resUI)
  return(result_list)
}

observationCard <- function (dataset, group_variable = character(0)) {
  if (identical(group_variable, character(0))) {
    res <- dataset %>%
      select(Numero_observation) %>%
      distinct() %>%
      summarise(NombreDObservations = length(Numero_observation))
    resUI$nombre_observations <- paste("<b>",resUI$nombre_observations,"</b>")
    colnames(resUI) = c("Nombre de protocoles réalisés")
  } else {
    res <- dataset %>%
      select_at(c("Numero_observation", group_variable)) %>%
      distinct() %>%
      dplyr::group_by_at(group_variable) %>%
      summarise(nombre_observations = length(Numero_observation))
    resCalc = res
    resUI = res[ , c(group_variable, "nombre_observations")]
    resUI$nombre_observations <- paste("<b>",resUI$nombre_observations,"</b>")
    colnames(resUI) = c(group_variable, "Nombre de protocoles réalisés")
    
  }
  result_list <- list(resCalc, resUI)
  return(result_list)
}

total_species_card <- function (dataset) {
  res <- dataset %>%
    filter(Nombre_individus > 0) %>%
    group_by(Espece) %>%
    summarise(nombre_observations = n())
  
  resCalc = res
  resUI = res
  resUI$nombre_observations <- paste("<b>",resUI$nombre_observations,"</b>")
  colnames(resUI) = c("Espèces", "Nombre d'observations de l'espèce")
  
  
  result_list <- list(resCalc, resUI)
  return(result_list)
}

makeGraphEasy <- function (dataset) {
  if ("Annee" %in% colnames(dataset)){
    dataset <- filter(dataset, Annee > 2000 | Annee < 2023)
  }
  
  columnsNames <- names(dataset)
  x <- columnsNames[1]
  y <- columnsNames[2]
  
  # change names to something nice
  x_label = nice_column_names(x)
  y_label = nice_column_names(y)
  
  # remove label order 
  dataset <- removeBeginingCategories(dataset, 1)
  dataset <- removeBeginingCategories(dataset, 2)
  
  graph <- ggplot2::ggplot(dataset, ggplot2::aes(x = !!ensym(x), y = !!ensym(y))) +
    xlab(x_label) +
    ylab(y_label)
  
  
  
  if ("Mois" %in% colnames(dataset) | "Annee" %in% colnames(dataset)){
    
    graph <- graph + 
      ggplot2::geom_point(size = 3, col = 2) +
      ggplot2::geom_line(size = 2, col = 2)
    
  } else if ("Latitude" %in% colnames(dataset) | "Longitude" %in% colnames(dataset)){
    graph <- graph + ggplot2::geom_point() +
      geom_smooth(method="lm", se=TRUE)
  } else {
    graph <- graph + ggplot2::geom_col(ggplot2::aes(fill = !!ensym(x)))
  }
  
  if ("intervalle_de_confiance" %in% colnames(dataset)) {
    dataset <- dataset %>%
      mutate(errorPlus = !!ensym(y) + intervalle_de_confiance,
             errorMoins = !!ensym(y) - intervalle_de_confiance)
    graph <- graph + ggplot2::geom_errorbar(data = dataset, ggplot2::aes(ymax = errorPlus, ymin = errorMoins),
                                            width = .2, size = 1.2)
  }
  
  graph <- graph +
    expand_limits(y = 0) +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.text=element_text(size=20),
                   axis.title=element_text(size=24),
                   strip.text.x = element_text(size = 20),
                   legend.position = "none")
  
  if ("Region" %in% colnames(dataset)){
    graph <- graph + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  }
  graph
}


makeMapEasy <- function(dataset, depart_map, region_map) {
  # change parameters according to the scale
  if ("Departement" %in% colnames(dataset)) {
    map_to_plot <- depart_map
    join_by = c("NOM_DEPT" = "Departement")
    dataset$Departement <- toupper(dataset$Departement)
  } else if ("Region" %in% colnames(dataset)) {
    map_to_plot <- region_map
    map_to_plot$Region <- c("Grand Est", "Nouvelle-Aquitaine", "Auvergne-Rhône-Alpes",
                            "Bourgogne-Franche-Comté", "Bretagne", "Centre-Val de Loire",
                            "Corse", "Île-de-France", "Occitanie"  ,
                            "Hauts-de-France","Normandie", "Pays de la Loire",
                            "Provence-Alpes-Côte d'Azur")
    join_by = "Region"
  }
  
  # join with the data
  geoData = dplyr::left_join(map_to_plot, dataset, by = join_by) %>%
    sf::st_transform(2154)
  
  # make map
  tmap::tm_shape(geoData) +
    tmap::tm_borders() +
    tmap::tm_fill(col = colnames(dataset)[2])
}

getSpeciesNumber <- function(dataset) {
  dataset %>%
    filter(Nombre_individus > 0) %>%
    group_by(Espece) %>%
    summarise(Nombre = n())
}

makeTop <- function (dataset, topLength = 20) {
  res <- dataset[order(data.frame(dataset)[ , 2], decreasing = TRUE), ]
  #resUI$nombre_observations <- paste("<b>",resUI$nombre_observations,"</b>")
  head(res, topLength)
}
