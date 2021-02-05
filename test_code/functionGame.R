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


#' @title codeInformation
#'
#' @description parse the input code and gives information on the tools,
#' the variables and the function used
#' @param fullCode a string containing the input code from the user
codeInformation <- function (fullCode) {
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


# All the card that make calculations functions will generate a list : the table for calculations and the table for UI

#' @title makeGraph
#'
#' @description make graph from the result of the previous tool
#' @param results the results from the previous tools
#' @param tools is the list of the tools used
#' @param i the number of the step
makeGraph <- function(tools, results, i) {
  # get parameters (improve by locating the graph within the code)
  Parameters <- separateParametersTreatment(tools[i])
  # get the name of the column to check few thing
  colNamesData <- colnames(results[[i-1]])
  # Add errors if the columns are not in the code
  # if sp is in the dataset, separate by species (if species as columns then change)
  #TODO Add
  #if ("Espece" %in% colNamesData & Parameters[[1]] != "Esp" & Parameters[[1]] != "Esp") facet = ggplot2::facet_wrap(.~Espece) else facet = NULL
  # if data not summarised plot points else plot barplot
  if (nrow(results[[i-1]]) < 30) representation <- ggplot2::geom_col(ggplot2::aes_string(fill = correspond(Parameters[[1]], EquivalenceVar))) else representation <- geom_jitter(aes_string(col = correspond(Parameters[[1]], EquivalenceVar)))
  # graph is too specific right now
  ggplot2::ggplot(results[[i-1]], ggplot2::aes_string(x = correspond(Parameters[[1]], EquivalenceVar), y = correspond(Parameters[[2]], EquivalenceVar)), environment = environment()) +
    representation +
    #facet +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.text=element_text(size=12),
                   axis.title=element_text(size=16),
                   strip.text.x = element_text(size = 14))
}

#' @title makeSummary
#'
#' @description make graph from the result of the previous tool
#' @param results the results from the previous tools
#' @param i the number of the step
makeSummary <- function (tools, results, i) {
  Parameters <- separateParametersTreatment(tools[i])
  # calculate se for error bars later
  if (Parameters[[3]] == "Mo") {
    res <- results[[i-1]] %>%
      dplyr::group_by_at(correspond(Parameters[[1]], EquivalenceVar)) %>%
      dplyr::summarise_at(.vars = correspond(Parameters[[2]], EquivalenceVar), .funs = c("mean","confidence_interval"))
    # rename with the right name
    colnames(res)[which(colnames(res) == "mean")] <- correspond(Parameters[[2]], EquivalenceVar)
  } else {
    res <- results[[i-1]] %>%
      dplyr::group_by_at(correspond(Parameters[[1]], EquivalenceVar)) %>%
      dplyr::summarise_at(.vars = correspond(Parameters[[2]], EquivalenceVar), .funs = correspond(Parameters[[3]], EquivalenceFun))
  }
  return(res)
}

#' @title makeErrorBars
#'
#' @description add ErrorBars to a graph
#' @param results the results from the previous tools
#' @param i the number of the step
makeErrorBars <- function(tools, results, i) {
  Parameters <- separateParametersTreatment(tools[i-1])
  results[[i-1]]$data <- results[[i-1]]$data %>%
    dplyr::mutate(ymin = Nombre_individus + confidence_interval,
                  ymax = Nombre_individus - confidence_interval)
  return(results[[i-1]] + geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2))
}


# not functionning
#' @title makeMap
#'
#' @description make a map with the departements
#' @param results the results from the previous tools
#' @param i the number of the step
makeMap <- function(tools, results, i) {
  Parameters <- separateParametersTreatment(tools[i])
  if (!Parameters[[2]] == "Dep") {
    extraWD = "data"
    if (!file.exists(file.path(extraWD, "departement.zip"))) {
      githubURL <- "https://github.com/statnmap/blog_tips/raw/master/2018-07-14-introduction-to-mapping-with-sf-and-co/data/departement.zip"
      download.file(githubURL, file.path(extraWD, "departement.zip"))
      unzip(file.path(extraWD, "departement.zip"), exdir = extraWD)
    }
    departements_L93 <- sf::st_read(dsn = extraWD, layer = "DEPARTEMENT",
                                    quiet = TRUE) %>%
      dplyr::rename(Departement = CODE_DEPT) %>%
      sf::st_transform(2154)
    
    departements_L93 <- mapToPlot()
    geoData = dplyr::left_join(departements_L93, results[[i-1]], by = 'Departement') %>%
      sf::st_transform(2154)
    
    tmap::tm_shape(geoData) +
      tmap::tm_borders() +
      tmap::tm_fill(col = correspond(Parameters[[2]], EquivalenceVar))
  }
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
  
  graph <- ggplot2::ggplot(dataset, ggplot2::aes(x = !!ensym(x), y = !!ensym(y)))
  
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
