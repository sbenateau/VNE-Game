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

# Modification of the functions to handle NA values
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

#' @title se
#'
#' @param x a numeric vector
confidence_interval <- function(x){
  x2 <- na.omit(x)
  1.96*sd(x2)/sqrt(length(x2))
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
    # Upload complete data for  birds
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
    jeuDeDonneesReduction$Environnement   <- factor(as.character(jeuDeDonneesReduction$Environnement), levels=c("Rural", "Péri-urbain", "Urbain"))
    jeuDeDonneesReduction$Distance_bois   <- factor(as.character(jeuDeDonneesReduction$Distance_bois), levels=c("moins de 50 m", "50 à 500 m", "501 à 1000 m", "1001 m à 2000 m", "au-delà de 2 km", "Non renseigné", ""))
    jeuDeDonneesReduction$Distance_bois[jeuDeDonneesReduction$Distance_bois ==""] <- "Non renseigné"
    jeuDeDonneesReduction$Distance_prarie <- factor(as.character(jeuDeDonneesReduction$Distance_prarie), levels=c("moins de 50 m", "50 à 500 m", "501 à 1000 m", "1001 m à 2000 m", "au-delà de 2 km", "Non renseigné", ""))
    jeuDeDonneesReduction$Distance_prarie[jeuDeDonneesReduction$Distance_prarie ==""] <- "Non renseigné"
    jeuDeDonneesReduction$Distance_champs <- factor(as.character(jeuDeDonneesReduction$Distance_champs), levels=c("moins de 50 m", "50 à 500 m", "501 à 1000 m", "1001 m à 2000 m", "au-delà de 2 km", "Non renseigné", ""))
    jeuDeDonneesReduction$Distance_champs[jeuDeDonneesReduction$Distance_champs ==""] <- "Non renseigné"
    levels(jeuDeDonneesReduction$Departement)[levels(jeuDeDonneesReduction$Departement) =="  "] <- "Non renseigné"
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
  if (Parameters[[3]] == "Mo"){
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
makeErrorBars <- function(tools, results, i){
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
makeMap <- function(tools, results, i){
  Parameters <- separateParametersTreatment(tools[i])
  if (!Parameters[[2]] == "Dep"){
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

abundanceCard <- function (dataset, groupVariable = character(0)) {
  if (identical(groupVariable, character(0))){
    res <- dataset %>%
      dplyr::group_by(Numero_observation) %>%
      dplyr::summarise(Abondance = sum2(Nombre_individus)) %>%
      dplyr::ungroup()%>%
      dplyr::summarise(AbondanceMoyenne = mean2(Abondance),
                       IntervalleDeConfiance = confidence_interval(Abondance))
  } else {
    res <- dataset %>%
      dplyr::group_by_at(c("Numero_observation", groupVariable)) %>%
      dplyr::summarise(Abondance = sum2(Nombre_individus)) %>%
      dplyr::ungroup()%>%
      dplyr::group_by_at(groupVariable) %>%
      dplyr::summarise(AbondanceMoyenne = mean2(Abondance),
                       IntervalleDeConfiance = confidence_interval(Abondance))

  }
  return(res)
}

diversityCard <- function (dataset, groupVariable = character(0)) {
  if (identical(groupVariable, character(0))){
    res <- dataset %>%
      dplyr::group_by(Numero_observation) %>%
      dplyr::summarise(Diversite = lengthSupZero(Nombre_individus)) %>%
      dplyr::ungroup()%>%
      dplyr::summarise(DiversiteMoyenne = mean2(Diversite),
                       IntervalleDeConfiance = confidence_interval(Diversite))
  } else {
    res <- dataset %>%
      dplyr::group_by_at(c("Numero_observation", groupVariable)) %>%
      dplyr::summarise(Diversite = lengthSupZero(Nombre_individus)) %>%
      dplyr::ungroup()%>%
      dplyr::group_by_at(groupVariable) %>%
      dplyr::summarise(DiversiteMoyenne = mean2(Diversite),
                       IntervalleDeConfiance = confidence_interval(Diversite))

  }
  return(res)
}

observationCard <- function (dataset, groupVariable = character(0)) {
  if (identical(groupVariable, character(0))){
    res <- dataset %>%
      select(Numero_observation)%>%
      distinct()%>%
      summarise(NombreDObservations = length(Numero_observation))
  } else {
    res <- dataset %>%
      select_at(c("Numero_observation", groupVariable)) %>%
      distinct() %>%
      dplyr::group_by_at(groupVariable) %>%
      summarise(NombreDObservations = length(Numero_observation))
  }
  return(res)
}


#' @title makeGraph
#'
#' @description make graph from the result of the previous tool
#' @param results the results from the previous tools
#' @param tools is the list of the tools used
#' @param i the number of the step
makeGraph <- function(tools, results, i) {
  # get parameters (improve by locating the graph within the code)
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

makeGraphEasy <- function (dataset){
  columnsNames <- names(dataset)
  x <- columnsNames[1]
  y <- columnsNames[2]

  if ("IntervalleDeConfiance" %in% colnames(dataset)){
    dataset <- dataset %>%
      mutate(errorPlus = !!ensym(y) + IntervalleDeConfiance,
             errorMoins = !!ensym(y) - IntervalleDeConfiance)
  }

  ggplot2::ggplot(dataset, ggplot2::aes(x = !!ensym(x), y = !!ensym(y)))+
    ggplot2::geom_col(ggplot2::aes(fill = !!ensym(x)))+
    ggplot2::geom_errorbar(ggplot2::aes(ymax = errorPlus, ymin = errorMoins),
                           width = .2)+
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.text=element_text(size=12),
                   axis.title=element_text(size=16),
                   strip.text.x = element_text(size = 14),
                   legend.position = "none")
}


makeMapEasy <- function(dataset) {
  if (dataset[ , 1] == "Dep"){
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
      tmap::tm_fill(col = correspond(dataset[ , 2], EquivalenceVar))
  }
}

getSpeciesNumber <- function(dataset) {
 dataset %>%
    filter(Nombre_individus > 0) %>%
    group_by(Espece) %>%
    summarise(Nombre = n())
}

makeTop <- function (dataset, topLength = 20){
  res <- dataset[order(data.frame(dataset)[ , 2], decreasing = TRUE), ]
  head(res, topLength)
}
