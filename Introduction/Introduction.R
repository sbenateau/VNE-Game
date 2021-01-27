# script pour intervention avec les eleves de secondes

# importer les donnees
source('~/GitHub/VNE-Game/functionGame.R')
Data <- data.frame(getDataInitial())
DataExemple <- Data %>%
  filter(Numero_observation %in% c("94994", "95002", "94983","95208","95352"))
write.table(DataExemple, "ExempleDeDonnées.csv", sep = "\t", dec = ",")

# stat descriptives

# Abondance totale par placettes
AbondanceParPlacette <- DataExemple %>%
  group_by(Numero_observation, Placette, Environnement) %>%
  summarise(Abondance = sum(Nombre_individus)) %>%
  ungroup()

AbondanceParPlacette

MoyenneAbondance <- AbondanceParPlacette %>%
  group_by(Numero_observation, Environnement) %>%
  summarise(MoyenneAbondance = mean(Abondance))
  

AbondanceParPlacette

ggplot(MoyenneAbondance, aes(x = Environnement, y = MoyenneAbondance, group = as.factor(Numero_observation))) +
  geom_point(aes(color = as.factor(Numero_observation))) +
  ylim(0, NA) +
  labs(color = "Numero observation")+
  theme_bw()