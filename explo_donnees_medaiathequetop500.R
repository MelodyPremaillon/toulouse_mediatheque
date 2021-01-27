library(ggplot2)
library(dplyr)


movies <- as_tibble(read.csv("DATA/top-500-des-films-les-plus-empruntes-a-la-bibliotheque-de-toulouse.csv",
                             sep=";"), fileEncoding = "utf-8")

sub <- movies[which(movies$ANNEE == 2015),]

ggplot(movies, aes(ANNEE, nb_prets)) + geom_bar(stat="identity") + theme_light() + 
  geom_bar(data=sub, aes(ANNEE, nb_prets), stat="identity", fill="yellow")

total_prets <- movies %>%
                group_by(TITRE) %>%
                summarise(
                  n=n(),
                  sum_prets = sum(nb_prets, na.rm=T)
                )

total_prets <- total_prets %>%
  arrange(desc(sum_prets)) %>%
  slice_max(sum_prets,n=10)
  
total_prets$row <- seq(1:1500)

ggplot(total_prets[1:10, ], aes(reorder(TITRE,sum_prets), sum_prets)) +  
  geom_bar(stat="identity",width=0.5) + coord_flip() +
  theme_light()

movies %>% group_by(AUTEUR) %>% summarise(nb_pret=sum(nb_prets)) %>% arrange(desc(nb_pret)) %>% mutate(prct=nb_pret/sum(nb_pret)*100)
