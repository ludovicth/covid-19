# Programme des fonctions contamination et évolution

# Données de test
pop_test_1_1 <- list(
  non_conta = 1000
  , sains = setNames(rep(0,14), c(0:13))
  , non_decl = setNames(rep(0,14), c(0:13))
  , decl = setNames(rep(0,14), c(0:13))
  , grav = setNames(rep(0,14), c(0:13))
  , mort = 0
  , immu = 0
)

pop_test_1_2 <- list(
  non_conta = 1000
  , sains = setNames(c(0, 2, rep(0,12)), c(0:13))
  , non_decl = setNames(c(0, 5, 2, 1, rep(0,10)), c(0:13))
  , decl = setNames(rep(0,14), c(0:13))
  , grav = setNames(rep(0,14), c(0:13))
  , mort = 0
  , immu = 0
)

pop_test_2 <- list(
    non_conta = 1000
    , sains = setNames(c(60, 45, 40, 45, 20, 27, 30, 15, 13, 13, 11, 9, 4, 2), c(0:13))
    , non_decl = setNames(c(50, 45, 30, 20, 15, rep(0,9)), c(0:13))
    , decl = setNames(c(rep(0,5), c(12, 14, 8, 9, 4, 6, 3, 4, 1)) , c(0:13))
    , grav = setNames(c(rep(0,8), c(10, 4, 7, 5, 3, 2)) , c(0:13))
    , mort = 5
    , immu = 8
  )


cluster_test <- setNames(list(pop_test_1_1, pop_test_1_2) , c("résistant", "vulnérable"))

# Fonction contamination
contamination <- function(pop, cluster, contag, prob_sain, prob_contact, n_contact){
  
  # pop <- "vulnérable"; cluster = cluster_test; prob_sain = 0.9; prob_contact = setNames(c(0.8, 0.2), c("vulnérable", "résistant")); n_contact = 20; contag = 0.005
  # pop <- "résistant"; cluster = cluster_test; prob_sain = 0.99; prob_contact = setNames(c(0.2, 0.8), c("vulnérable", "résistant")); n_contact = 100; contag = 0.005
  
  
  # Etape 1 : Vérifications
  if(typeof(cluster) != "list") stop("Le cluster doit être une liste de populations")
  if(is.null(cluster[[pop]])) stop ("Cette population n'existe pas dans le cluster")
  if(is.null(names(prob_contact))) stop ("La probabilité de contact doit être un vecteur nommé")
  if(any(!names(prob_contact) %in% names(cluster))) stop("La probabilité de contact doit être un vecteur nommé dont les noms sont les populations du cluster")
  
  
  # Etape 2 : Calcul de la probabilité pour un individu d'être contaminé
  
  # Nombre de contacts avec des individus de chaque population
  n_contact_pop <- n_contact * prob_contact
  
  # Nombre de contacts avec des individus contaminés par individu de la population
  n_contact_contam <- sum(
    sapply(names(n_contact_pop), function(nom_pop){
      popul <- cluster[[nom_pop]]
      (sum(popul[["sains"]]) + sum(popul[["non_decl"]])) / (popul[["non_conta"]] + sum(popul[["sains"]]) + sum(popul[["non_decl"]]) + popul[["immu"]] ) * n_contact_pop[nom_pop]
    })
  )
  
  # Probabilité d'être contaminé pour un individu
  prob_contam <- 1 - (1 - contag)^n_contact_contam
  
  # Etape 3 : Nombre d'individus contaminés dans la population et de chaque type
  
  # Nombre d'individus contaminés
  population <- cluster[[pop]]
  n_contam <- population[["non_conta"]] * prob_contam
  
  # Nombre d'individus devenant porteurs sains
  population[["non_conta"]] <- population[["non_conta"]] - n_contam
  population[["sains"]]["0"] <- n_contam * prob_sain
  population[["non_decl"]]["0"] <- n_contam * (1 - prob_sain)
  
  return(population)
}


evolution <- function(pop, d, g, prob_grav, satur_hop, tx_mort){
  
  # pop <- pop_test_2; d = 4; g = 7; prob_grav = 0.4 ; tx_mort = setNames(c(0.9, 0.2), c("satur", "normal")) ; satur_hop = 0
  
  taille_pop <- sum(sapply(pop, sum))
  
  # Etape 1 : Vérifications
  
  if(length(pop) != 7) stop("L'objet pop doit contenir 7 éléments décrivant la population")
  if(d >= g) stop("La durée d avant que la maladie se déclare doit être strictement inférieure à la durée g à laquelle elle devient grave")
  if(length(tx_mort) != 2) stop ("Le taux de mortalité doit contenir 2 éléments : 1 en cas de saturation des hôpitaux, l'autre en cas de fonctionnement normal")
  if(any(!names(tx_mort) %in% c("satur", "normal"))) stop("Les noms de l'objet taux de mortalité sont incorrects")
  if(! satur_hop %in% c(0, 1)) stop("La saturation des hôpitaux doit être une indicatrice")
  
  # Etape 2 : Evolution des porteurs sains
  pop[["immu"]] <- pop[["immu"]] + pop[["sains"]][length(pop[["sains"]])]
  pop[["sains"]] <- setNames(
    c(0, pop[["sains"]][1 : ( length(pop[["sains"]]) - 1 ) ])
    , names(pop[["sains"]])
  )
  
  # Etape 3 : Evolution des malades graves
  if(satur_hop == 0) tx_mort_hop <- tx_mort["normal"]
  else tx_mort_hop <- tx_mort["satur"]
  nb_morts <- tx_mort_hop * pop[["grav"]][length(pop[["grav"]])]
  pop[["immu"]] <- pop[["immu"]] + pop[["grav"]][length(pop[["grav"]])] - nb_morts
  pop[["mort"]] <- pop[["mort"]] + nb_morts
  pop[["grav"]] <- setNames(c(0, pop[["grav"]][1 : (length(pop[["grav"]]) - 1)])
                            , names(pop[["grav"]]))
  pop[["grav"]][as.character(g+1)] <- setNames(pop[["decl"]][as.character(g)] * prob_grav, as.character(g+1))
  
  
  # Etape 4 : Evolution des malades déclarés
  pop[["immu"]] <- pop[["immu"]] + pop[["decl"]][length(pop[["decl"]])]
  pop[["decl"]] <- setNames(
    c(rep(0, d+1)
      , pop[["non_decl"]][as.character(d)]
      , pop[["decl"]][as.character((d+1):(g-1))]
      , pop[["decl"]][as.character(g)]* (1 - prob_grav)
      , pop[["decl"]][as.character((g+1) : (length(pop[["decl"]]) - 2))])
    ,   names(pop[["decl"]])
  )
  
  # Etape 5 : Evolution des malades non déclarés
  pop[["non_decl"]] <- setNames(c(0, pop[["non_decl"]][1 : d], rep(0, length(pop[["non_decl"]] ) - d - 1) ), names(pop[["non_decl"]]))
  
  # Etape 6 : mise en forme
  names(pop[["mort"]]) <- NULL
  names(pop[["immu"]]) <- NULL
  
  # Etape 7 : Vérification
  if(sum(sapply(pop, sum)) != taille_pop) stop ("La taille de la population en entrée est différente de celle en sortie")
  
  # Etape 8 : Sortie
  return(pop)
}

# library(microbenchmark)
# microbenchmark(contamination(pop  ="vulnérable", cluster = cluster_test, prob_sain = 0.9, prob_contact = setNames(c(0.8, 0.2), c("vulnérable", "résistant")), n_contact = 20, contag = 0.005), times = 10000)
# moyenne : environ 100 micro secondes => 100 000 : environ 10 secondes pour faire la contamination sur France entière, sur une population
# microbenchmark(evolution(pop = pop_test_2, d = 4, g = 7, prob_grav = 0.4 , tx_mort = setNames(c(0.9, 0.2), c("satur", "normal")) , satur_hop = 0), times = 10000)
# moyenne : environ 140 micro secondes => 40000 : environ 6 secondes pour faire la contamination sur France entière, sur une population


contamination_cluster <- function(cluster, contag, v_prob_sain, mat_prob_contact, v_contact){
  
  # cluster = cluster_test; contag = 0.005; v_prob_sain = setNames(c(0.9, 0.99), c("vulnérable", "résistant")); v_contact = setNames(c(20, 100), c("vulnérable", "résistant")); mat_prob_contact = rbind(c(0.5, 0.5), c(0.1, 0.9)); row.names(mat_prob_contact) = c("vulnérable", "résistant"); colnames(mat_prob_contact) = c("vulnérable", "résistant");
  # cluster = cluster_test; contag = 0.005; v_prob_sain = setNames(c(0.9, 0.99), c("vulnérable", "résistant")); v_contact = setNames(c(20, 100), c("vulnérable", "résistant")); mat_prob_contact = rbind(c(0.8, 0.2), c(0.2, 0.8)); row.names(mat_prob_contact) = c("vulnérable", "résistant"); colnames(mat_prob_contact) = c("vulnérable", "résistant");
  
  # Etape 1 : Vérifications
  
  if(is.null(names(cluster))) stop ("Les populations du cluster doivent être nommées")
  if(length(unique(length(v_prob_sain), length(v_contact), length(cluster), nrow(mat_prob_contact), ncol(mat_prob_contact))) != 1) stop("Les objets en entrée n'ont pas tous la même longueur : certaines populations n'ont pas de paramètres ou certains paramètres s'appliquent à des populations qui n'existent pas")
  if(any(! names(cluster) %in% names(v_contact)) | any(! names(cluster) %in% names(v_prob_sain)) | any(! names(cluster) %in% row.names(mat_prob_contact)) | any(! names(cluster) %in% colnames(mat_prob_contact))) stop("Les paramètres ne sont pas nommés comme la population")
  if(any(apply(mat_prob_contact, 1, sum) != 1)) stop("Les lignes de mat_prob_contact doivent sommer à 1")
  
  # Etape 2 : Application de la fonction contamination à chaque population
  cluster_contamine <- lapply(setNames(names(cluster), names(cluster)), function(nom_pop){
    # nom_pop = "vulnérable"
    population <- cluster[[nom_pop]]
    contamination(pop = nom_pop, cluster = cluster, contag = contag, prob_sain = v_prob_sain[nom_pop], prob_contact = mat_prob_contact[nom_pop, ], n_contact = v_contact[nom_pop])
  })
  
  return(cluster_contamine)
  
}

# mat_prob_contact <- rbind(c(0.5, 0.5), c(0.1, 0.9))
# row.names(mat_prob_contact) <- c("vulnérable", "résistant")
# colnames(mat_prob_contact) <- c("vulnérable", "résistant")

# microbenchmark(contamination_cluster(cluster = cluster_test, contag = 0.005, v_prob_sain = setNames(c(0.9, 0.99), c("vulnérable", "résistant")), v_contact = setNames(c(20, 100), c("vulnérable", "résistant")), mat_prob_contact = mat_prob_contact), times = 10000)
# moyenne : environ 240 micro secondes => 100 000 : environ 24 secondes pour faire la contamination sur France entière, sur une population



evolutions_cluster <- function(cluster, d, g, v_prob_grav, satur_hop, mat_tx_mort){
  
  # cluster = list(pop_test_2, pop_test_2); names(cluster) = c("vulnérable", "résistant"); d = 4; g = 7; satur_hop = 0; v_prob_grav = setNames(c(0.4, 0.1), c("vulnérable", "résistant")); mat_tx_mort = rbind(c(0.9, 0.2), c(0.1, 0.01)); colnames(mat_tx_mort) = c("satur", "normal"); row.names(mat_tx_mort) = c("vulnérable", "résistant")

  # Etape 1 : Vérifications
  
  if(is.null(names(cluster))) stop ("Les populations du cluster doivent être nommées")
  if(length(unique(length(v_prob_grav), length(cluster), nrow(mat_tx_mort))) != 1) stop("Les objets en entrée n'ont pas tous la même longueur : certaines populations n'ont pas de paramètres ou certains paramètres s'appliquent à des populations qui n'existent pas")
  if(any(! names(cluster) %in% names(v_prob_grav)) | any(! names(cluster) %in% row.names(mat_tx_mort)) ) stop("Les paramètres ne sont pas nommés comme la population")

  # Etape 2 : Application de la fonction contamination à chaque population
  cluster_j_plus_1 <- lapply(setNames(names(cluster), names(cluster)), function(nom_pop){
    # nom_pop = "vulnérable"
    evolution(pop = cluster[[nom_pop]], d = d, g = g, prob_grav = v_prob_grav[[nom_pop]], tx_mort = mat_tx_mort[nom_pop, ], satur_hop = satur_hop)
  })
  
  return(cluster_j_plus_1)
  
}
