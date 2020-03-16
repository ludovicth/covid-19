# Etape 0 : Chargement des données et des fonctions

setwd("C:/Users/nicol/Desktop/Données recensement")

# Chargement des fonctions
source("fonctions_elementaires.R")
source("fonctions_mobilite.R")

# Chargement des données
mob_dom_trav_clust <- readRDS("flux_bv_pop_jour.rds")
summary(mob_dom_trav_clust$trav %in% mob_dom_trav_clust$dom)
mob_dom_trav_clust$dom <- as.character(mob_dom_trav_clust$dom )
mob_dom_trav_clust$trav <- as.character(mob_dom_trav_clust$trav )
mob_dom_trav_clust$pop <- as.character(mob_dom_trav_clust$pop )


# Etape 1 : calcul de la saturation des hôpitaux

# Pas de saturation des hôpitaux ici
# On pourra complexifier quand on disposera du nombre de lits par département

liste_clust <- unique(as.character(mob_dom_trav_clust$dom))
v_satur_hop <- setNames(rep(0, length(liste_clust)), liste_clust)

# Etape 2 : Calcul du nombre de contacts par cluster avec chaque population

# Etape 2.1 : Contacts dans le cluster de travail

# Etape 2.1.1 : Calcul du nombre de personnes dans un cluster au travail
taille_clust_trav <- sapply(split(mob_dom_trav_clust$flux, mob_dom_trav_clust$trav), sum)

# Etape 2.1.2 : Calcul du nombre de contacts par individu de la population du cluster
# On construit une matrice de contacts par bassin de vie. De base, un individu vulnérable est âgé et sort moins : il a moins de contact avec l'extérieur. Il a a priori 20 contacts avec l'extérieur quand les autres individu en ont 50.
# Dans les clusters les plus denses la journée, le nombre de contacts peut être multiplié par 5.
n_contact_trav <- (taille_clust_trav^0.19/ min(taille_clust_trav^0.19)) * matrix(c(rep(20, length(taille_clust_trav)), rep(50, length(taille_clust_trav))) , nrow = length(taille_clust_trav), dimnames = list(names(taille_clust_trav), c("vulnérable", "résistant")))

# Etape 2.1.2 : Calcul de la liste de probabilité de contacts associée
# Proportion d'individus vulnérable dans chaque cluster
prop_vulnerable <- sapply(split(mob_dom_trav_clust$flux[mob_dom_trav_clust$pop == "vulnérable"], mob_dom_trav_clust$trav[mob_dom_trav_clust$pop == "vulnérable"]), sum) / taille_clust_trav

# Calcul de la proportion de contact de la population vulnérable entre elle : on suppose que c'est un groupe qui est 2 fois plus en contact entre eux
liste_prob_contact_trav <- list(
  vulnérable = cbind(sapply(2*prop_vulnerable, function(x) min(x, 0.99)),  1 - sapply(2*prop_vulnerable, function(x) min(x, 0.99)))
)
colnames(liste_prob_contact_trav[["vulnérable"]]) <- c("vulnérable", "résistant")

# Calcul de la proportion de contact de la population résistante entre elle : dépend exclusivement de ce pourcentage pour la population vulnérable et du nombre de contacts de chaque population précédemment fixé
taille_pop_vuln <- sapply(split(mob_dom_trav_clust$flux[mob_dom_trav_clust$pop == "vulnérable"], mob_dom_trav_clust$trav[mob_dom_trav_clust$pop == "vulnérable"]), sum) 
summary(row.names(liste_prob_contact_trav[["vulnérable"]]) == row.names(n_contact_trav))
summary(row.names(liste_prob_contact_trav[["vulnérable"]]) == names(taille_pop_vuln))

n_contact_vuln_resist <-  taille_pop_vuln * liste_prob_contact_trav[["vulnérable"]][ , "résistant"] * n_contact_trav[ , "vulnérable"]
taille_pop_resist <- sapply(split(mob_dom_trav_clust$flux[mob_dom_trav_clust$pop == "résistant"], mob_dom_trav_clust$trav[mob_dom_trav_clust$pop == "résistant"]), sum) 
summary(names(taille_pop_resist) == row.names(n_contact_trav))
n_contact_resist <- taille_pop_resist * n_contact_trav[ , "résistant"]
 
liste_prob_contact_trav[["résistant"]] <- cbind(n_contact_vuln_resist / n_contact_resist, 1 - n_contact_vuln_resist / n_contact_resist)
colnames(liste_prob_contact_trav[["résistant"]]) <- c("vulnérable", "résistant")

# Remarque : en théorie, il faudrait faire ces calculs dans la fonction mobilite_france, en tenant compte de qui est malade et ne peut pas se déplacer ou simplement entrer en contact avec quelqu'un d'autre
# C'est une amélioration potentielle
# En particulier, cela devient faut quand beaucoup de malades se déclarent

# Etape 2.2 : Contacts dans le cluster domicile

# Etape 2.2.1 : Calcul du nombre de personnes dans un cluster domicile
taille_clust_dom <- sapply(split(mob_dom_trav_clust$flux, mob_dom_trav_clust$dom), sum)

# Etape 2.2.2 : Calcul du nombre de contacts par individu de la population du cluster
# On construit une matrice de contacts par bassin de vie. Les individus sont peu en contact entre eux (ils ont des activités sociales en journée dans leur cluster de domail et sont en famille à domicile).
n_contact_dom <- matrix(c(rep(5, length(taille_clust_dom)), rep(5, length(taille_clust_dom))) , nrow = length(taille_clust_dom), dimnames = list(names(taille_clust_dom), c("vulnérable", "résistant")))

# Etape 2.2.2 : Calcul de la liste de probabilité de contacts associée
# Proportion d'individus vulnérable dans chaque cluster
prop_vulnerable <- sapply(split(mob_dom_trav_clust$flux[mob_dom_trav_clust$pop == "vulnérable"], mob_dom_trav_clust$dom[mob_dom_trav_clust$pop == "vulnérable"]), sum) / taille_clust_dom

# Calcul de la proportion de contact de la population vulnérable entre elle : on suppose que c'est un groupe qui est 2 fois plus en contact entre eux
liste_prob_contact_dom <- list(
  vulnérable = cbind(sapply(3*prop_vulnerable, function(x) min(x, 0.99)),  1 - sapply(3*prop_vulnerable, function(x) min(x, 0.99)))
)
colnames(liste_prob_contact_dom[["vulnérable"]]) <- c("vulnérable", "résistant")

# Calcul de la proportion de contact de la population résistante entre elle : dépend exclusivement de ce pourcentage pour la population vulnérable et du nombre de contacts de chaque population précédemment fixé
taille_pop_vuln <- sapply(split(mob_dom_trav_clust$flux[mob_dom_trav_clust$pop == "vulnérable"], mob_dom_trav_clust$dom[mob_dom_trav_clust$pop == "vulnérable"]), sum) 
summary(row.names(liste_prob_contact_dom[["vulnérable"]]) == row.names(n_contact_dom))
summary(row.names(liste_prob_contact_dom[["vulnérable"]]) == names(taille_pop_vuln))

n_contact_vuln_resist <-  taille_pop_vuln * liste_prob_contact_dom[["vulnérable"]][ , "résistant"] * n_contact_dom[ , "vulnérable"]
taille_pop_resist <- sapply(split(mob_dom_trav_clust$flux[mob_dom_trav_clust$pop == "résistant"], mob_dom_trav_clust$dom[mob_dom_trav_clust$pop == "résistant"]), sum) 
summary(names(taille_pop_resist) == row.names(n_contact_dom))
n_contact_resist <- taille_pop_resist * n_contact_dom[ , "résistant"]

liste_prob_contact_dom[["résistant"]] <- cbind(n_contact_vuln_resist / n_contact_resist, 1 - n_contact_vuln_resist / n_contact_resist)
colnames(liste_prob_contact_dom[["résistant"]]) <- c("vulnérable", "résistant")


# On a donc construit liste_prob_contact_dom, liste_prob_contact_trav, n_contact_dom
# et n_contact_trav dont on aura besoin


# Etape 3 : Initialisation du pays

# Taille des populations du cluster
taille_pop_resist <- sapply(split(mob_dom_trav_clust$flux[mob_dom_trav_clust$pop == "résistant"], mob_dom_trav_clust$dom[mob_dom_trav_clust$pop == "résistant"]), sum) 
taille_pop_vuln <- sapply(split(mob_dom_trav_clust$flux[mob_dom_trav_clust$pop == "vulnérable"], mob_dom_trav_clust$dom[mob_dom_trav_clust$pop == "vulnérable"]), sum) 

taille_pop <- lapply(setNames(names(taille_pop_resist), names(taille_pop_resist)), function(nom_clust){
  c(résistant = unname(taille_pop_resist[nom_clust]), vulnérable = unname(taille_pop_vuln[nom_clust]))
})

pays_init <- lapply(taille_pop, function(taille_pop_clust){
  lapply(taille_pop_clust, function(pop){
    return(list(
      non_conta = pop*0.7
      , sains = setNames(pop * c(0, 0.02, 0.015, 0.01, 0.005, 0.01, rep(0.005, 8)), c(0:13))
      , non_decl = setNames(c(0, pop * 0.02, pop * 0.015, pop * 0.01, pop*0.005, rep(0, 9)), c(0:13))
      , decl = setNames(c(rep(0, 5), pop * 0.01, pop * 0.005, pop * 0.005, pop * 0.005, pop * 0.005, pop * 0.005, pop * 0.005, pop * 0.005, pop * 0.005), c(0:13))
      , grav = setNames(c(rep(0, 8), pop * c(0.005, 0.004, 0.003, 0.003, 0.003, 0.002)) , c(0:13))
      , mort = pop * 0.02
      , immu = pop * 0.06
    ))
  })
})


# Etape 4 : fonction de modélisation d'une journée

modele_journee <- function(pays, v_clust, v_flux_clust, contag, v_prob_sain, liste_prob_contact_dom, liste_prob_contact_trav, mat_contact_trav, mat_contact_dom, d, g, v_prob_grav, v_satur_hop, mat_tx_mort){
  
  # pays = lapply(setNames(liste_clust, liste_clust), function(x) cluster_test); v_clust = liste_clust; v_flux_clust = mob_dom_trav_clust; contag = 0.005;  v_prob_sain = setNames(c(0.9, 0.99), c("vulnérable", "résistant")); liste_prob_contact_dom = liste_prob_contact_dom ; liste_prob_contact_trav = liste_prob_contact_trav ; mat_contact_trav = n_contact_trav ; mat_contact_dom = n_contact_dom ; d = 4; g = 7; v_prob_grav = setNames(c(0.4, 0.1), c("vulnérable", "résistant")); v_satur_hop = v_satur_hop; mat_tx_mort = rbind(c(0.9, 0.2), c(0.1, 0.01)); colnames(mat_tx_mort) = c("satur", "normal"); row.names(mat_tx_mort) = c("vulnérable", "résistant")  
  # pays = lapply(setNames(liste_clust, liste_clust), function(x) list(vulnérable = pop_test_2, résistant = pop_test_2) ); v_clust = liste_clust; v_flux_clust = mob_dom_trav_clust; contag = 0.005;  v_prob_sain = setNames(c(0.9, 0.99), c("vulnérable", "résistant")); liste_prob_contact_dom = liste_prob_contact_dom ; liste_prob_contact_trav = liste_prob_contact_trav ; mat_contact_trav = n_contact_trav ; mat_contact_dom = n_contact_dom ; d = 4; g = 7; v_prob_grav = setNames(c(0.4, 0.1), c("vulnérable", "résistant")); v_satur_hop = v_satur_hop; mat_tx_mort = rbind(c(0.9, 0.2), c(0.1, 0.01)); colnames(mat_tx_mort) = c("satur", "normal"); row.names(mat_tx_mort) = c("vulnérable", "résistant")  
  # pays = pays_init; v_clust = liste_clust; v_flux_clust = mob_dom_trav_clust; contag = 0.005;  v_prob_sain = setNames(c(0.9, 0.99), c("vulnérable", "résistant")); liste_prob_contact_dom = liste_prob_contact_dom ; liste_prob_contact_trav = liste_prob_contact_trav ; mat_contact_trav = n_contact_trav ; mat_contact_dom = n_contact_dom ; d = 4; g = 7; v_prob_grav = setNames(c(0.4, 0.1), c("vulnérable", "résistant")); v_satur_hop = v_satur_hop; mat_tx_mort = rbind(c(0.9, 0.2), c(0.1, 0.01)); colnames(mat_tx_mort) = c("satur", "normal"); row.names(mat_tx_mort) = c("vulnérable", "résistant")  
  
  # Etape 4.1 : Vérifications
  if(ncol(v_flux_clust) != 4 | any( !names(v_flux_clust) %in% c("dom", "trav", "pop", "flux"))) stop("La structure de l'objet v_flux_clust n'est pas celle attendue.")
  if(length(pays) != length(v_clust)) stop("Il y a des clusters en trop dans l'objet pays, ou bien il en manque")
  if(any(! names(pays) %in% v_clust)) stop("Certains clusters dans l'objet pays ne sont pas dans la liste d'identifiants")
  liste_pop <- unique(v_flux_clust$pop)
  # if(any(!liste_pop %in% names(v_prob_sain)) | any(! liste_pop %in% row.names(mat_tx_mort)) | any(! liste_pop %in% colnames(mat_contact))) stop("Certains paramètres ne sont pas définies pour quelques populations de v_flux_clust")
  
  # Etape 4.2 : départ vers un cluster pour les non contaminés, les malades non déclarés et les porteurs sains
  
  # On commence par calculer les pourcentages de déplacement de chaque population d'un cluster domicile vers les différents clusters de travail
  tx_flux_pop_dom_vers_trav <- lapply(setNames(names(pays), names(pays)), function(nom_clust){
    # nom_clust = names(pays)[[2]]
    v_flux_clust_dom <-  v_flux_clust[v_flux_clust$dom == nom_clust, ]
    type_pop_clust_dom <- unique(v_flux_clust_dom$pop)
    
    lapply(setNames(type_pop_clust_dom, type_pop_clust_dom), function(nom_pop){
      flux_pop_vers_trav_clust_dom <- setNames(v_flux_clust_dom$flux[v_flux_clust_dom$pop == nom_pop], v_flux_clust_dom$trav[v_flux_clust_dom$pop == nom_pop])
      flux_pop_vers_trav_clust_dom / sum(flux_pop_vers_trav_clust_dom)
    })
  })
  
  # On calcule les différents cluster de travail associés au cluster domicile
  liste_clust_trav_dom <- lapply(setNames(v_clust, v_clust), function(nom_clust){
    unique(v_flux_clust$trav[v_flux_clust$dom == nom_clust])
  })
  
  # On stocke les différents croisement cluster de travail x population pour chaque cluster domicile
  liste_pop_dom_vers_trav <- lapply(setNames(names(pays), names(pays)), function(nom_clust_dom){
    v_flux_clust[v_flux_clust$dom == nom_clust_dom, c("trav", "pop")]
  })
  
  # On calcule les flux de voyageurs de chaque population du cluster domicile se rendant dans les clusters de travail
  depart_trav <- lapply(setNames(names(pays), names(pays)), function(nom_clust_dom){
    # nom_clust_dom = names(pays)[1]
    lapply(setNames(liste_clust_trav_dom[[nom_clust_dom]], liste_clust_trav_dom[[nom_clust_dom]]), function(nom_clust_trav){
      # nom_clust_trav = liste_clust_trav_dom[[nom_clust_dom]][1]
      type_pop <- liste_pop_dom_vers_trav[[nom_clust_dom]]$pop[liste_pop_dom_vers_trav[[nom_clust_dom]]$trav == nom_clust_trav]
      lapply(setNames(type_pop, type_pop), function(nom_pop){
        # nom_pop = type_pop[1]
        tx_depla <- unname(tx_flux_pop_dom_vers_trav[[nom_clust_dom]][[nom_pop]][nom_clust_trav])
        
        cluster_pop_depla <- pays[[nom_clust_dom]][[nom_pop]]
        cluster_pop_depla[["non_conta"]] <- cluster_pop_depla[["non_conta"]] * tx_depla
        cluster_pop_depla[["sains"]] <- cluster_pop_depla[["sains"]] * tx_depla
        cluster_pop_depla[["non_decl"]] <- cluster_pop_depla[["non_decl"]] * tx_depla
        cluster_pop_depla[["immu"]] <- cluster_pop_depla[["immu"]] * tx_depla
        
        if(nom_clust_dom != nom_clust_trav){
          cluster_pop_depla[["decl"]] <- setNames(rep(0, length(cluster_pop_depla[["decl"]])),names(cluster_pop_depla[["decl"]]))
          cluster_pop_depla[["grav"]] <- setNames(rep(0, length(cluster_pop_depla[["grav"]])),names(cluster_pop_depla[["decl"]]))
          cluster_pop_depla[["mort"]] <- 0
        }
        
        return(cluster_pop_depla)
        
      })
      
    })
  })
  
  
  # Etape 4.3 : Arrivée au travail

  # On calcule les différents cluster de domicile arrivant dans chaque cluster de travail
  liste_clust_dom_trav <- lapply(setNames(v_clust, v_clust), function(nom_clust){
    unique(v_flux_clust$dom[v_flux_clust$trav == nom_clust])
  })
  
  # On calcule la composition du cluster de travail avec l'information sur les clusters d'origine
  arrivee_trav <- lapply(setNames(names(pays), names(pays)), function(nom_clust_trav){
    # nom_clust_trav <- names(pays)[1]
    liste_clust_dom <- liste_clust_dom_trav[[nom_clust_trav]]
    arrivee_trav_clust_trav <- lapply(setNames(liste_clust_dom, liste_clust_dom), function(nom_clust_dom){
      return(depart_trav[[nom_clust_dom]][[nom_clust_trav]])
    })    
  })
  
  # Etape 4.4 : Composition du cluster par population durant la journée de travail, sans distinguer le cluster d'origine
  compo_clust_trav <- lapply(arrivee_trav, function(clust_trav){
    # clust_trav <- arrivee_trav[[1]]
    
    # On repère les différentes populations dans le cluster d'arrivée
    type_pop <- unique(do.call(c, lapply(clust_trav, names)))
    
    # On calcule la composition du cluster en chaque population
    compo_cluster_pop <- 
      lapply(setNames(type_pop, type_pop), function(pop){
        # pop = "vulnérable"
        # pop = "résistant"
        # On va chercher pour chaque population les groupes correspondant dans les différents cluster d'origine
        groupes <- c("non_conta", "sains", "non_decl", "decl", "grav", "mort", "immu")
        compo_pop <- lapply(setNames(groupes, groupes), function(groupe){
          # groupe = groupes[1]
          # groupe = groupes[2]
          groupe_clust_dom <- lapply(clust_trav, function(clust_dom){
            # clust_dom = clust_trav[[1]]
            clust_dom[[pop]][[groupe]]
          })
          groupe_clust_dom <- groupe_clust_dom[!sapply(groupe_clust_dom, is.null)]
          groupe_clust_dom <- do.call(rbind, groupe_clust_dom)
          groupe_clust_dom <- apply(groupe_clust_dom, 2, sum)
        })

        
      })
    
  })
  
  # Etape 4.5 : Contamination des clusters durant la journée de travail
  mat_prob_contact_trav_clust <- lapply(setNames(names(pays), names(pays)), function(nom_clust){
    do.call(rbind, lapply(liste_prob_contact_trav, function(pop) pop[nom_clust, ]))
  })
  
  clust_trav_contam <- lapply(setNames(names(compo_clust_trav), names(compo_clust_trav)), function(nom_clust){
    contamination_cluster(cluster = compo_clust_trav[[nom_clust]], contag = contag, v_prob_sain = v_prob_sain, mat_prob_contact = mat_prob_contact_trav_clust[[nom_clust]], v_contact = mat_contact_trav[nom_clust, ])
  })
  
  # Etape 4.6 : Départ vers le domicile
  
  # On doit maintenant décomposer la population contaminée de manière à l'affecter aux différents clusters d'origine
  # On calcule le nombre de porteurs sains et de malades non déclarés contaminés dans la journée, et la diminution du nombre de contaminés
  flux_contam_clust <- lapply(setNames(names(clust_trav_contam), names(clust_trav_contam)), function(nom_clust){
    # nom_clust = names(clust_trav_contam)[1] 
      do.call(cbind, lapply(setNames(names(clust_trav_contam[[nom_clust]]), names(clust_trav_contam[[nom_clust]])), function(nom_pop){
        # nom_pop = names(clust_trav_contam[[nom_clust]])[1]
      c(non_conta = clust_trav_contam[[nom_clust]][[nom_pop]][["non_conta"]] - compo_clust_trav[[nom_clust]][[nom_pop]][["non_conta"]]
        , sains = unname(clust_trav_contam[[nom_clust]][[nom_pop]][["sains"]]["0"] - compo_clust_trav[[nom_clust]][[nom_pop]][["sains"]]["0"])
        , non_decl = unname(clust_trav_contam[[nom_clust]][[nom_pop]][["non_decl"]]["0"] - compo_clust_trav[[nom_clust]][[nom_pop]][["non_decl"]]["0"]))
    }))
  
  })
  
  # On va maintenant décomposer cette population contaminée dans la journée d'un cluster de travail en les différents clusters domicile
  # qui sont venus travailler
  
  # On commence par calculer par cluster de départ le pourcentage de personnes non contaminées venant de chaque cluster de domicile
  tx_arriv_pers_non_conta <- lapply(arrivee_trav, function(arrivee_trav_clust){
    # arrivee_trav_clust = arrivee_trav[[2]]
    type_pop <- unique(do.call(c, lapply(arrivee_trav_clust, names)))
    
    lapply(setNames(type_pop, type_pop), function(pop){
      # pop = type_pop[1]
      arriv_clust_pop_non_conta <- 
        do.call(c, lapply(arrivee_trav_clust, function(arrivee_trav_clust_dom){
          unname(arrivee_trav_clust_dom[[pop]][["non_conta"]])
        }))
      
      return(arriv_clust_pop_non_conta / sum(arriv_clust_pop_non_conta))
    })
    
  })
  
  # On peut maintenant ventiler les populations contaminées d'un cluster de travail vers les clusters domicile
  retour_trav_contam <- lapply(setNames(names(arrivee_trav), names(arrivee_trav)), function(nom_clust_trav){
    # nom_clust_trav = names(arrivee_trav)[2]
    lapply(setNames(names(arrivee_trav[[nom_clust_trav]]), names(arrivee_trav[[nom_clust_trav]])), function(nom_clust_dom){
      # nom_clust_dom = names(arrivee_trav[[nom_clust_trav]])[1]
      lapply(setNames(names(arrivee_trav[[nom_clust_trav]][[nom_clust_dom]]), names(arrivee_trav[[nom_clust_trav]][[nom_clust_dom]])), function(nom_pop){
        # nom_pop = names(arrivee_trav[[nom_clust_trav]][[nom_clust_dom]])[1]
        pop_clust_dom <- arrivee_trav[[nom_clust_trav]][[nom_clust_dom]][[nom_pop]]
        pop_clust_dom[["non_conta"]] <- unname(pop_clust_dom[["non_conta"]] + flux_contam_clust[[nom_clust_trav]]["non_conta", nom_pop] * tx_arriv_pers_non_conta[[nom_clust_trav]][[nom_pop]][nom_clust_dom])
        pop_clust_dom[["sains"]]["0"] <- pop_clust_dom[["sains"]]["0"] + flux_contam_clust[[nom_clust_trav]]["sains", nom_pop] * tx_arriv_pers_non_conta[[nom_clust_trav]][[nom_pop]][nom_clust_dom]
        pop_clust_dom[["non_decl"]]["0"] <- pop_clust_dom[["non_decl"]]["0"] + flux_contam_clust[[nom_clust_trav]]["non_decl", nom_pop] * tx_arriv_pers_non_conta[[nom_clust_trav]][[nom_pop]][nom_clust_dom]
        return(pop_clust_dom)
      })
    } )
  })
  
  
  # Etape 4.7 : Arrivée au domicile
  
  liste_clust_trav_dom <- lapply(setNames(v_clust, v_clust), function(nom_clust){
    unique(v_flux_clust$trav[v_flux_clust$dom == nom_clust])
  })
  
  arrivee_dom <- lapply(setNames(names(pays), names(pays)), function(nom_clust_dom){
    # nom_clust_dom = names(pays)[1]
    
    liste_clust_trav <- liste_clust_trav_dom[[nom_clust_dom]]

    compo_clust_dom <- lapply(setNames(liste_clust_trav, liste_clust_trav), function(nom_clust_trav){
      # nom_clust_trav = liste_clust_trav[2]
      retour_trav_contam[[nom_clust_trav]][[nom_clust_dom]]
    })

  })
  
  # Etape 4.8 : Agrégation des populations revenant du travail pour donner la composition du cluster
  
  compo_clust_dom <- lapply(arrivee_dom, function(clust_dom){
    # clust_dom <- arrivee_dom[[1]]
    
    # On repère les différentes populations dans le cluster d'arrivée
    type_pop <- unique(do.call(c, lapply(clust_dom, names)))
    
    # On calcule la composition du cluster en chaque population
    compo_cluster_pop <- 
      lapply(setNames(type_pop, type_pop), function(pop){
        # pop = "vulnérable"
        # pop = "résistant"
        # On va chercher pour chaque population les groupes correspondant dans les différents cluster d'origine
        groupes <- c("non_conta", "sains", "non_decl", "decl", "grav", "mort", "immu")
        compo_pop <- lapply(setNames(groupes, groupes), function(groupe){
          # groupe = groupes[1]
          # groupe = groupes[2]
          groupe_clust_trav <- lapply(clust_dom, function(clust_trav){
            # clust_trav = clust_dom[[1]]
            clust_trav[[pop]][[groupe]]
          })
          groupe_clust_trav <- groupe_clust_trav[!sapply(groupe_clust_trav, is.null)]
          groupe_clust_trav <- do.call(rbind, groupe_clust_trav)
          groupe_clust_trav <- apply(groupe_clust_trav, 2, sum)
        })
        
        
      })
    
  })
  
  # Etape 4.9 : Contamination dans le cluster domicile
  mat_prob_contact_dom_clust <- lapply(setNames(names(pays), names(pays)), function(nom_clust){
    do.call(rbind, lapply(liste_prob_contact_dom, function(pop) pop[nom_clust, ]))
  })
  
  clust_dom_contam <- lapply(setNames(names(compo_clust_dom), names(compo_clust_dom)), function(nom_clust){
    contamination_cluster(cluster = compo_clust_dom[[nom_clust]], contag = contag, v_prob_sain = v_prob_sain, mat_prob_contact = mat_prob_contact_dom_clust[[nom_clust]], v_contact = mat_contact_dom[nom_clust, ])
  }) 
  
  
  # Etape 4.10 : Passage à J+1 : application de la fonction évolution_cluster
  
  pays_j_plus_1 <- lapply(setNames(names(clust_dom_contam), names(clust_dom_contam)), function(nom_clust){
    # nom_clust = names(clust_dom_contam)[2]
    # nom_clust = names(clust_dom_contam)[5]
    evolutions_cluster(cluster = clust_dom_contam[[nom_clust]], d = d, g = g, v_prob_grav = v_prob_grav, satur_hop = v_satur_hop[nom_clust], mat_tx_mort = mat_tx_mort)
  })
  
  
}

# Remarque :
# On peut modéliser le confinement intra-communal en diminuant les flux flux_clust à partir d'une certaine journée
# On peut modéliser chaque jour la saturation des hôpitaux en chargeant des données sur les hôpitaux et en les comparant au nombre de malades graves dans le cluster ou à un niveau agrégé
# On peut modéliser chaque jour l'effet du confinement de la population en faisant varier mat_contact
# On peut modéliser chaque jour l'effet du confinement d'une partie de la population en faisant varier mat_prob_contact et mat_contact
# On peut modéliser le nombre de rencontres par jour par cluster mat_contact en jouant sur la densité de poplation
# Inconvénient du modèle mat_prob_contact devrait différer d'un cluster à l'autre (pas la même structure démographique dans tous les clusters). Ce n'est pas le cas.
