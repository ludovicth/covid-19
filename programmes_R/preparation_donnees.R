
# Etape 0 : Préparation des données de mobilité inter-clusters

# Etape 0.1 : Chargement des données de mobilité du recensement
setwd("C:/Users/nicol/Desktop/Données recensement")
mob_dom_trav <- read.table("FD_MOBPRO_2015.txt", sep = ";", header = TRUE, stringsAsFactors = FALSE)
# https://www.insee.fr/fr/statistiques/3566008?sommaire=3558417
names(mob_dom_trav) <- tolower(names(mob_dom_trav))

# Remplacement des communes par l'arrondissement dans les communes Paris, Lyon, Marseille
mob_dom_trav$commune[mob_dom_trav$commune == "75056"] <- mob_dom_trav$arm[mob_dom_trav$commune == "75056"] 
mob_dom_trav$commune[mob_dom_trav$commune == "69123"] <- mob_dom_trav$arm[mob_dom_trav$commune == "69123"] 
mob_dom_trav$commune[mob_dom_trav$commune == "13055"] <- mob_dom_trav$arm[mob_dom_trav$commune == "13055"] 

# Suppression des déplacements domicile-travail à l'étranger : les individus travaillant à l'étranger sont supposés travailler dans leur commune
mob_dom_trav$dclt[mob_dom_trav$dclt == "99999"] <- mob_dom_trav$commune[mob_dom_trav$dclt == "99999"] 

# Suppression des déplacements sans objet : ils restent dans la commune
mob_dom_trav$dclt[mob_dom_trav$dclt == "YYYYY"] <- mob_dom_trav$commune[mob_dom_trav$dclt == "YYYYY"] 

# Suppression des déplacements dans des communes qui n'existent pas dans les communes "domicile" dans le recensement (peu nombreuses)
summary(! mob_dom_trav$dclt %in% mob_dom_trav$commune)
# table(mob_dom_trav$dclt[! mob_dom_trav$dclt %in% mob_dom_trav$commune])
# Il s'agit essentiellement de déplacements vers les DOM / vers l'étranger
dclt_pas_ds_com <- !mob_dom_trav$dclt %in% mob_dom_trav$commune
mob_dom_trav$dclt[dclt_pas_ds_com] <- mob_dom_trav$commune[dclt_pas_ds_com]

# Suppression des déplacements vers l'outremer
mob_dom_trav$dclt[substr(mob_dom_trav$dclt, 1, 2) == "97"] <- mob_dom_trav$commune[substr(mob_dom_trav$dclt, 1, 2) == "97"] 

# Allègement de la base
mob_dom_trav <- mob_dom_trav[mob_dom_trav$metrodom == "M" , c("commune", "dclt", "agerevq", "cs1", "trans", "ipondi")]


# Etape 0.2 : Chargement des données communales du recensement par âge et sexe
rp_age_sexe <- read.csv(file = "rp15_age_sexe.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
names(rp_age_sexe) <- tolower(names(rp_age_sexe))
# Suppression de Paris, Lyon, Marseille (on ne garde que les arrondissements) et des communes des DOM
rp_age_sexe <- rp_age_sexe[! rp_age_sexe$codgeo %in% c("75056", "69123", "13055"), ]
rp_age_sexe <- rp_age_sexe[substr(rp_age_sexe$codgeo, 1, 2) != "97", ]

# Etape 0.3 : Chargement des données communales du recensement par statut d'activité pour les plus de 15 ans
rp_stat_act <- read.csv(file = "rp15_statut_act.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
names(rp_stat_act) <- tolower(names(rp_stat_act))
# Suppression de Paris, Lyon, Marseille (on ne garde que les arrondissements) et des communes des DOM
rp_stat_act <- rp_stat_act[! rp_stat_act$codgeo %in% c("75056", "69123", "13055"), ]
rp_stat_act <- rp_stat_act[substr(rp_stat_act$codgeo, 1, 2) != "97", ]



# Etape 1 : Calcul des flux de population

# Etape 1.1 : Flux de mobilité intercommunale

# Etape 1.1.1 : Générer la variable qui identifie les différentes populations étudiées dans la table de mobilité

# Ici, on estime que toute la population mobile pour le domicile - travail est résistante (i.e. on regarde une seule population et l'autre population est vulnérable, il s'agit de personnes âgées non mobiles)
mob_dom_trav$cat_pop <- "résistant"


# Etape 1.1.2 : Calcul du flux de population entre communes pour les actifs occupés de 15 ans et plus

# Pour gagner en efficacité, on regroupe commune de domicile et commune de travail et population dans une seule variable
mob_dom_trav$dom_trav_pop <- paste0(mob_dom_trav$commune, "_", mob_dom_trav$dclt, "_", mob_dom_trav$cat_pop) 

# Nombre de personnes pour chaque flux domicile travail
mob_dom_trav_pop <- tapply(mob_dom_trav$ipondi, mob_dom_trav$dom_trav_pop, sum)
tab_mob_dom_trav_pop <- data.frame(dom = substr(names(mob_dom_trav_pop), 1, 5), trav = substr(names(mob_dom_trav_pop), 7, 11), pop = substr(names(mob_dom_trav_pop), 13, nchar(names(mob_dom_trav_pop))), flux = mob_dom_trav_pop)


# Etape 1.2 : Calcul du nombre de moins de 15 ans et de leur flux

# On considère que les moins de 15 ans sont une population résistante mais immobile donc ayant un flux intra-communal
nom_col_moins_15_ans <- c(paste0("sexe1", "_aged100", formatC(0:14, width = 3, flag = "0")), paste0("sexe2", "_aged100", formatC(0:14, width = 3, flag = "0")))
moins_15_ans <- setNames(apply(rp_age_sexe[ , nom_col_moins_15_ans], 1, sum), rp_age_sexe$codgeo)
tab_mob_dom_trav_moins_15 <- data.frame(dom = names(moins_15_ans), trav = names(moins_15_ans), pop = "résistant", flux = moins_15_ans)
row.names(tab_mob_dom_trav_moins_15) <- paste0(tab_mob_dom_trav_moins_15$dom, "_", tab_mob_dom_trav_moins_15$trav, "_", tab_mob_dom_trav_moins_15$pop)

# Etape 1.3 : Calcul du nombre de 15 ans à 64 ans qui ne sont pas actifs occupés
# Ici, c'est une population résistante mais immobile
actif_ou_age_65_plus <- c(colnames(rp_stat_act)[substr(colnames(rp_stat_act), 22, 23) == "11"], colnames(rp_stat_act)[substr(colnames(rp_stat_act), 13, 15) == "065"])

nb_15_64_non_act_occ <- setNames(
  apply(rp_stat_act[, ! names(rp_stat_act) %in% c("codgeo", "libgeo", actif_ou_age_65_plus)], 1, sum)
  , rp_stat_act$codgeo
)
tab_mob_dom_trav_15_64_non_act_occ <- data.frame(dom = names(nb_15_64_non_act_occ), trav = names(nb_15_64_non_act_occ), pop = "résistant", flux = nb_15_64_non_act_occ)
row.names(tab_mob_dom_trav_15_64_non_act_occ) <- paste0(tab_mob_dom_trav_15_64_non_act_occ$dom, "_", tab_mob_dom_trav_15_64_non_act_occ$trav, "_", tab_mob_dom_trav_15_64_non_act_occ$pop)


# Etape 1.4 : Calcul du nombre de 65 ans et plus qui ne sont pas actifs occupés
# Ici, c'est une population vulnérable et immobile
nb_65_plus_non_act_occ <- setNames(
  rp_stat_act$sexe1_ageq65065_tactr12 + rp_stat_act$sexe1_ageq65065_tactr21 + rp_stat_act$sexe1_ageq65065_tactr22 + rp_stat_act$sexe1_ageq65065_tactr24 + rp_stat_act$sexe1_ageq65065_tactr26 + rp_stat_act$sexe2_ageq65065_tactr12 + rp_stat_act$sexe2_ageq65065_tactr21 + rp_stat_act$sexe2_ageq65065_tactr22 + rp_stat_act$sexe2_ageq65065_tactr24 + rp_stat_act$sexe2_ageq65065_tactr26
  , rp_stat_act$codgeo
)
tab_mob_dom_trav_65_plus_non_act_occ <- data.frame(dom = names(nb_65_plus_non_act_occ), trav = names(nb_65_plus_non_act_occ), pop = "vulnérable", flux = nb_65_plus_non_act_occ)
row.names(tab_mob_dom_trav_65_plus_non_act_occ) <- paste0(tab_mob_dom_trav_65_plus_non_act_occ$dom, "_", tab_mob_dom_trav_65_plus_non_act_occ$trav, "_", tab_mob_dom_trav_65_plus_non_act_occ$pop)

# save.image("C:/Users/nicol/Desktop/Données recensement/prov.RData")

# Etape 1.5 : Flux de mobilité domicile - travail de chaque population et dans chaque commune

# On commence par sommer les flux par population autres que celles des actifs occupés
summary(row.names(tab_mob_dom_trav_15_64_non_act_occ) == row.names(tab_mob_dom_trav_moins_15))
tab_mob_intra_com_resist_non_act_occ <- setNames(tab_mob_dom_trav_15_64_non_act_occ$flux + tab_mob_dom_trav_moins_15$flux, row.names(tab_mob_dom_trav_15_64_non_act_occ))
tab_mob_intra_com_vuln <- setNames(tab_mob_dom_trav_65_plus_non_act_occ$flux, row.names(tab_mob_dom_trav_65_plus_non_act_occ))

mob_intra_com <- c(tab_mob_intra_com_resist_non_act_occ, tab_mob_intra_com_vuln)

# On supprime les 23 flux nuls
mob_intra_com <- mob_intra_com[mob_intra_com != 0]

# On ajoute les flux de population intra-communale qui existent déjà à la table de mobilité
summary(names(mob_intra_com) %in% row.names(tab_mob_dom_trav_pop))
# Pour les 33871 flux qui existent déjà dans la population active, on les somme
flux_a_sommer <- mob_intra_com[names(mob_intra_com) %in% row.names(tab_mob_dom_trav_pop)]

place_flux_intra <- match(names(flux_a_sommer), row.names(tab_mob_dom_trav_pop))
tab_mob_dom_trav_pop$flux[place_flux_intra] <- flux_a_sommer + tab_mob_dom_trav_pop$flux[place_flux_intra]

# On ajoute les flux de population intra-communale qui n'existaient pas dans la table de mobilité
flux_a_ajouter <- mob_intra_com[!names(mob_intra_com) %in% row.names(tab_mob_dom_trav_pop)]
flux_a_ajouter <- data.frame(dom = substr(names(flux_a_ajouter), 1, 5), trav = substr(names(flux_a_ajouter), 7, 11), pop = substr(names(flux_a_ajouter), 13, nchar(names(flux_a_ajouter))), flux = flux_a_ajouter)
tab_mob_dom_trav_pop <- rbind(tab_mob_dom_trav_pop, flux_a_ajouter)

# Sauvegarde des flux de population
# Vérification
summary(duplicated(tab_mob_dom_trav_pop[ , c("dom", "trav")]))
# Pas de doublon!
# saveRDS(object = tab_mob_dom_trav_pop, file = "flux_pop_jour.rds")
