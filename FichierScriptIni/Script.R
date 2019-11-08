source('../functionGame.R')
# Get the data TODO allow different possibilities
Data <- data.frame(getDataInitial())
#Remplacer NA par 0
Data[is.na(Data[ , "nb_ind"]) , "nb_ind"] <- 0


# calcul de la diversité
DataLessRaw <- Data %>%
  filter(nb_ind > 0) %>%
  group_by(numero_observation,environnement) %>%
  count()

DataLessSumm <- DataLessRaw %>%
  group_by(environnement) %>%
  summarise(Moyenne_Diversite = mean(n, na.rm = TRUE))

ggplot(DataLess, aes(x = environnement, y = Moyenne_Diversite)) +
  geom_col()

summary(aov(DataLessRaw$n~DataLessRaw$environnement))

#separate code to get clear instructions
tools <- separateTools(code)

# create a list with size = nomber of tools
Results <- vector(mode = "list", length = length(tools))

#loop to execute all the steps
for (i in 1:length(tools)){
  # case tool = data
  if (tools[i] == "D"){
    Results[[i]] <- Data
  } else if (tools[i] == "M") {
    Results[[i]] = randomAll(Results[[i-1]])
    # case tool = Graph
  } else if (tools[i] == "G") {
    # Case if first tool = Plot
    if (i == 1){
      print(ggplot(NULL))
    } else {
      # get the name of the column to check few thing
      colNamesData <- colnames(Results[[i-1]])
      # if sp is in the dataset, separate by  species
      if ("Espece" %in% colNamesData) facet = facet_wrap(.~Espece) else facet = NULL
      # if data not summarised plot points else plot barplot
      if (nrow(Results[[i-1]]) < 30) representation <- geom_col(aes(fill = environnement)) else representation <- geom_jitter(aes(fill = environnement))
      # graph is too specific right now
      YourPlot <- ggplot(Results[[i-1]], aes(x = environnement, y = nb_ind), environment = environment()) +
        representation +
        facet +
        theme_bw()
      print(YourPlot)
    }
    # case if tool = group columns
  } else if (substring(tools[i], 1, 1) == "R"){
    if (i == 1){
      printPlot("Il n'y a pas de données à résumer, \navez-vous inclus le code des données ?")
    } else {
      Parameters <- separateParametersR(tools[i])
      Results[[i]] <- Results[[i-1]] %>%
        group_by_at(correspond(Parameters[[1]], EquivalenceVar)) %>%
        summarise_at(.vars = correspond(Parameters[[2]], EquivalenceVar), .funs = correspond(Parameters[[3]], EquivalenceFun))
    }
  } else {
    printPlot("Il doit y avoir une erreur dans le code que vous avez écrit")
  }
}
} else {
  printPlot("Tapez un code dans la case prévue")
}


