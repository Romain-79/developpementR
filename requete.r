#M1
paste("La moyenne des parts de vote abstention est de ", round(moyenneAbstention, digits = 2),"%")

#M2
VoteJadot2040 <- resultats_pres2022 [resultats_pres2022$Part_Jadot > 20 & resultats_pres2022$Part_LePen < 40 , ]
View(VoteJadot2040)

#M3
write.csv(voteSaintes, "C:\\Users\\romai\\OneDrive - Université de La Rochelle\\DONNEES_SIG\\HUBERT\\INITIATION_R\\RESULTAT\\voteSaintes.csv")

#M4
voteSaintes_Candidats1_3 <- voteSaintes [ , c("Part_Macron", "Part_LePen", "Part_Melenchon")]
View(voteSaintes_Candidats1_3) 


#M5
voteSaintes_Candidats1_3$colonneTest <- NA
voteSaintes_Candidats1_3$colonneTest <- ifelse(voteSaintes_Candidats1_3$Part_Macron > 30, "MacronSup30", NA)
View(voteSaintes_Candidats1_3)
test_count <- count(voteSaintes_Candidats1_3, colonneTest,)

#M6
tapply(resultats_pres2022$Part_Arthaud, resultats_pres2022$Libelle_dep, max)

#M7
# On définit les noms des candidats dans une variable nommée « col1 »
voteCandidats_LaRochelle <- resultats_pres2022[resultats_pres2022$Libelle == "La Rochelle",]
col1 <- c("Hidalgo", "Melenchon", "Jadot")

# On reprend les % de vote issus de l'extraction précédente, dans une variable nommée « col2 »
col2 <- c(mean(voteCandidats_LaRochelle$Part_Hidalgo), mean(voteCandidats_LaRochelle$Part_Melenchon),
+           mean(voteCandidats_LaRochelle$Part_Jadot))

# On crée le tableau de données du graphique en spécifiant que les valeurs du graph correspondent à « col2 »
data <- data.frame(group=col1, value=col2)

# On crée le graphique :
ggplot(data, aes(x="", y=value , fill=group)) +
     geom_bar(stat="identity", width=1) +
     geom_col() +
    coord_polar("y", start=0) +
    
    # On ajoute les valeurs de % de vote sur le graph et on personnalise les couleurs
    geom_text(aes(label = round(value, digits=2)), position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157"))

#M8


#M9
