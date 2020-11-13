# ----------------------------------------------------------------------------
#                                                                                                                      
#                                      88                       
#                                      ""                       
#                                                               
#              88       88 8b,dPPYba,  88 8b,dPPYba,   ,adPPYba,
#              88       88 88P'   `"8a 88 88P'   `"8a a8P_____88
#              88       88 88       88 88 88       88 8PP"""""""
#              "8a,   ,a88 88       88 88 88       88 "8b,   ,aa
#               `"YbbdP'Y8 88       88 88 88       88  `"Ybbd8"'
#                                                               
#
#     Laurent Donzé
#     Institut du management de l'information
#     Université de Neuchâtel
#     2000 Neuchâtel
#
# ------------------------------------------------------------------------------

# Définition de fonctions ad hoc
# ------------------------------

# Nbr. non NA

nmiss <- function(x){ sum(!is.na(x))}

# Standard error

se <- function(x){
    
    # Standard error de x (erreur-type)
    #
    # x : série x
    
    sd(x, na.rm=T)/sqrt(nmiss(x))
}

# r-ème moment centré

mur <- function(x, r){
    
    # r-ème moment centré
    #
    # x : série x
    # r : r-eme moment
    
    meanx <- mean(x, na.rm=T)
    mur <- mean ( ( x - meanx )^r, na.rm=T)
    return(mur)
    
}
    
# Skewness

skew <- function(x){
    
    # Skewness : coefficient d'asymétrie d'une série x
    #
    # x : série x
    
    
    mur(x, 3) / ( mur(x, 2)^(3/2))
    
}
    
# Kurtosis

kurt <- function(x){
    
    # Kurtosis : coefficient d'aplatissement d'une série x
    #
    # x : série x
    
    ( mur(x, 4) / mur(x, 2)^2 ) - 3

}

# Jarque-Bera

jb <- function(x){
    
    # Statistique de Jarque-Bera
    #
    # x : série x
    
    (nmiss(x)/6)*( skew(x)^2 + ((kurt(x)^2) / 4) )
    
}

# My descriptive

mydescriptive <- function(x){
    
    # Tableau ad hoc de statistiques descriptives
    #
    # x : série x
    #
    
    des <- c("N non NA"=nmiss(x), 
             "Moyenne"=mean(x, na.rm=T), 
             "Stand. dev."=sd(x, na.rm=T), 
             "S.E."=se(x),
             "Skewness"=skew(x),
             "Kurtosis"=kurt(x),
             "Jarque-Bera"=jb(x),
             "p-value"=pchisq(jb(x), 2, lower.tail=F)
             )
    return(des)
}
