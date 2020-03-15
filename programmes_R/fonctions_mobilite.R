
# Fonction de calcul de départ des flux depuis un cluster vers les autres clusters
depart_clust <- function(nom_cluster, cluster, flux_clust){
  
  # nom_cluster = names(pays)[1]; cluster = pays[[1]]; flux_clust = v_flux_clust[v_flux_clust$dom == nom_cluster, ]
  # nom_cluster = names(pays)[1976]; cluster = pays[[1976]]; flux_clust = v_flux_clust[v_flux_clust$dom == nom_cluster, ]
  
  
  # Etape 1 : Taux de déplacement dans un cluster d'arrivée par population
  for(i in unique(flux_clust$pop)){
    flux_clust$tx_mob_pop[flux_clust$pop == i] <- flux_clust$flux[flux_clust$pop == i] / sum(flux_clust$flux[flux_clust$pop == i])
  }
  
  # Etape 2 : Calculer les populations qui se déplacent d'un cluster à l'autre par cluster d'arrivée
  depla_vers_trav <- lapply(setNames(unique(flux_clust$trav), unique(flux_clust$trav)), function(clust_trav){
    # clust_trav = unique(flux_clust$trav)[1];
    # clust_trav = unique(flux_clust$trav)[64];
    type_pop_depla <- unique(flux_clust$pop[flux_clust$trav == clust_trav])
    
    # Etape 2.1 : identifier pour chaque cluster d'arrivée, les populations qui arrivent du cluster de départ "nom_cluster"
    depla_diff_pop_vers_clust_trav <- lapply(setNames(type_pop_depla, type_pop_depla), function(pop){
      # pop = "résistant"
      # Etape 2.2 : calculer le nombre de personnes qui de déplacent dans le cluster d'arrivée dans les groupes susceptibles de se déplacer
      tx_depla <- flux_clust$tx_mob_pop[flux_clust$trav == clust_trav & flux_clust$pop  == pop]
      cluster_pop_depla <- cluster[[pop]]
      cluster_pop_depla[["non_conta"]] <- cluster_pop_depla[["non_conta"]] * tx_depla
      cluster_pop_depla[["sains"]] <- cluster_pop_depla[["sains"]] * tx_depla
      cluster_pop_depla[["non_decl"]] <- cluster_pop_depla[["non_decl"]] * tx_depla
      cluster_pop_depla[["immu"]] <- cluster_pop_depla[["immu"]] * tx_depla
      
      # Etape 2.3 : on laisse dans le cluster de départ les personnes en incapacité de se déplacer
      if(clust_trav != nom_cluster){
        cluster_pop_depla[["decl"]] <- setNames(rep(0, length(cluster_pop_depla[["decl"]])),names(cluster_pop_depla[["decl"]]))
        cluster_pop_depla[["grav"]] <- setNames(rep(0, length(cluster_pop_depla[["grav"]])),names(cluster_pop_depla[["decl"]]))
        cluster_pop_depla[["mort"]] <- 0
      }
      return(cluster_pop_depla)
    })
    return(depla_diff_pop_vers_clust_trav)
  })
  
  return(depla_vers_trav)
  
}