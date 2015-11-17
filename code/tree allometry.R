# --> generic volume calculation
data$volume_gen1 <- (data$d130/100)**2*data$h # including branches etc
data$volume_gen2 <- 0.42*pi*((data$d130/100)/2)**2*data$h # excluding branches etc
data$vol <- NA # the final variable that will be used
data$vol_alt <- NA
data$incl <- NA


# -> general species-specific equations to calculate aboveground stem-volume of trees in m³
# Acer pseudoplatanus, Acer platanoides, Acer sp. (Acer pseudoplatanus - Dagnelie, 1999)
countries <- levels(droplevels(data$id_area[(data$id_species==26 | data$id_species==36 | data$id_species==93)  & !is.na(data$id_species)]))
l <- length(data$id_point[(data$id_species==26 | data$id_species==36 | data$id_species==93)  & !is.na(data$id_species)])
d <- data$d130[(data$id_species==26 | data$id_species==36 | data$id_species==93)  & !is.na(data$id_species)]
h <- data$h[(data$id_species==26 | data$id_species==36 | data$id_species==93)  & !is.na(data$id_species)]
data$incl[(data$id_species==26 | data$id_species==36 | data$id_species==93)  & !is.na(data$id_species)] <- 0
data$vol[(data$id_species==26 | data$id_species==36 | data$id_species==93)  & !is.na(data$id_species)] <- 0.010343-0.00450536*d+3.4070*10^(-4)*d^2-4.0472*10^(-6)*d^3+7.7115*10^(-4)*h+2.9836*10^(-5)*d^2*h

# Acer campestre (Maillet, 2014)
# mainly trees in Fr_N (22/24), so french equation used for this species is almost 100% justified
countries <- levels(droplevels(data$id_area[data$id_species==38 & !is.na(data$id_species)]))
l <- length(data$id_point[data$id_species==38 & !is.na(data$id_species)])
d <- data$d130[data$id_species==38 & !is.na(data$id_species)]
h <- data$h[data$id_species==38 & !is.na(data$id_species)]
data$incl[data$id_species==38 & !is.na(data$id_species)] <- 1
data$vol[data$id_species==38 & !is.na(data$id_species)] <- (h*d**2)/(4*pi*(1-1.3/h)**2)*(0.534 + 0.661*(sqrt(d)/h) - 0.002*(h/d))

#Alnus glutinosa (Dik, 1984)
countries <- levels(droplevels(data$id_area[data$id_species==1 & !is.na(data$id_species)]))
l <- length(data$id_point[data$id_species==1 & !is.na(data$id_species)])
d <- data$d130[data$id_species==1 & !is.na(data$id_species)]
h <- data$h[data$id_species==1 & !is.na(data$id_species)]
data$incl[data$id_species==1 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==1 & !is.na(data$id_species)] <- (d^1.85749*h^0.88675*exp(-2.5222))/1000
#Alnus glutinosa, Sweden (Eriksson, 1973)
countries <- levels(droplevels(data$id_area[(data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O") & data$id_species==1 & !is.na(data$id_species)]))
l <- length(data$id_point[(data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O") & data$id_species==1 & !is.na(data$id_species)])
d <- data$d130[(data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O") & data$id_species==1 & !is.na(data$id_species)]
h <- data$h[(data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O") & data$id_species==1 & !is.na(data$id_species)]
data$incl[(data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O") & data$id_species==1 & !is.na(data$id_species)] <- 0
data$vol[(data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O") & data$id_species==1 & !is.na(data$id_species)] <- (0.1926*d^2+0.01631*d^2*h+0.003755*d*h^2-0.02756*d*h+0.000499*d^2+h^2)/1000
data$vol_alt[(data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O") & data$id_species==1 & !is.na(data$id_species)] <- (0.05437*d^1.94505*h^0.92947)/1000

# Alnus incana (Braastad, 1966)
countries <- levels(droplevels(data$id_area[data$id_species==31 & !is.na(data$id_species)]))
l <- length(data$id_point[data$id_species==31 & !is.na(data$id_species)])
d <- data$d130[data$id_species==31 & !is.na(data$id_species)]
h <- data$h[data$id_species==31 & !is.na(data$id_species)]
data$incl[data$id_species==31 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==31 & !is.na(data$id_species)] <- (-1.86827+0.21461*d^2+0.01283*d^2*h+0.0138*h^2*d-0.06311*h^2)/1000

# Betula pendula (Dik, 1984)
d <- data$d130[data$id_species==3 & !is.na(data$id_species)]
h <- data$h[data$id_species==3 & !is.na(data$id_species)]
data$incl[data$id_species==3 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==3 & !is.na(data$id_species)] <- (d^1.8906*h^0.26595*exp(-1.07055))/1000

# Betula sp., Betula pubescens (Betula sp. - Dagnelie, 1999)
d <- data$d130[data$id_species==12 & !is.na(data$id_species) | data$id_species==96 & !is.na(data$id_species)]
h <- data$h[data$id_species==12 & !is.na(data$id_species) | data$id_species==96 & !is.na(data$id_species)]
data$incl[data$id_species==12 & !is.na(data$id_species) | data$id_species==96 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==12 & !is.na(data$id_species) | data$id_species==96 & !is.na(data$id_species)] <- -0.011392-0.00031447*d+0.000279211*d^2-5.7966*10^(-6)*d^3-5.9573*10^(-4)*h+3.0409*10^(-5)*d^2*h

# Betula sp. , Betula pendula, Betula pubescens (Betula sp. - Näslund, 1947)
d <- data$d130[(data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O") & data$id_species==12 & !is.na(data$id_species) | data$id_species==96 & !is.na(data$id_species) | data$id_species==3 & !is.na(data$id_species)]
h <- data$h[(data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O") & data$id_species==12 & !is.na(data$id_species) | data$id_species==96 & !is.na(data$id_species) | data$id_species==3 & !is.na(data$id_species)]
data$incl[(data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O") & data$id_species==12 & !is.na(data$id_species) | data$id_species==96 & !is.na(data$id_species) | data$id_species==3 & !is.na(data$id_species)] <- 0
data$vol[(data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O") & data$id_species==12 & !is.na(data$id_species) | data$id_species==96 & !is.na(data$id_species) | data$id_species==3 & !is.na(data$id_species)] <- (0.1305*d^2+0.01338*d^2*h+0.01757*d*h^2-0.05606*h^2)/1000

# Carpinus betulus (Carpinus sp. - Scheelhas, 2002)
d <- data$d130[data$id_species==2 & !is.na(data$id_species)]
h <- data$h[data$id_species==2 & !is.na(data$id_species)]
data$incl[data$id_species==2 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==2 & !is.na(data$id_species)] <- (0.00021491*(d*10)^(2.258957614+0.001411006)*h^0.60291075)/1000

# Castanea sativa (Quercus robur - Dik, 1984)
d <- data$d130[data$id_species==42 & !is.na(data$id_species)]
h <- data$h[data$id_species==42 & !is.na(data$id_species)]
data$incl[data$id_species==42 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==42 & !is.na(data$id_species)] <- (d^2.00333*h^0.85925*exp(-2.86353))/1000

# Cornus mas (generic)
d <- data$d130[data$id_species==39 & !is.na(data$id_species)]
h <- data$h[data$id_species==39 & !is.na(data$id_species)]
data$incl[data$id_species==39 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==39 & !is.na(data$id_species)] <- 0.42*pi*((d/100)/2)**2*h

# Corylus avellana (Braasted, 1966)
d <- data$d130[data$id_species==8 & !is.na(data$id_species)]
h <- data$h[data$id_species==8 & !is.na(data$id_species)]
data$incl[data$id_species==8 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==8 & !is.na(data$id_species)] <- (-1.86827+0.21461*d^2+0.01283*d^2*h+0.0138*d*h^2-0.06311*h^2)/1000

# Crataegus laevigata, Crataegus monogyna, Crataegus sp. (generic)
d <- data$d130[data$id_species==23 & !is.na(data$id_species) | data$id_species==30 & !is.na(data$id_species) | data$id_species==99 & !is.na(data$id_species)]
h <- data$h[data$id_species==23 & !is.na(data$id_species) | data$id_species==30 & !is.na(data$id_species) | data$id_species==99 & !is.na(data$id_species)]
data$incl[data$id_species==23 & !is.na(data$id_species) | data$id_species==30 & !is.na(data$id_species) | data$id_species==99 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==23 & !is.na(data$id_species) | data$id_species==30 & !is.na(data$id_species) | data$id_species==99 & !is.na(data$id_species)] <- 0.42*pi*((d/100)/2)**2*h

# Cydonia oblonga, Malus sylvestris (generic)
d <- data$d130[data$id_species==48 & !is.na(data$id_species) | data$id_species==58 & !is.na(data$id_species)]
h <- data$h[data$id_species==48 & !is.na(data$id_species) | data$id_species==58 & !is.na(data$id_species)]
data$incl[data$id_species==48 & !is.na(data$id_species) | data$id_species==58 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==48 & !is.na(data$id_species) | data$id_species==58 & !is.na(data$id_species)] <- 0.42*pi*((d/100)/2)**2*h

# Fagus sylvatica (Dagnelie, 1999)
d <- data$d130[data$id_species==4 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Fr_S_B" | data$id_area=="Fr_S_O")]
h <- data$h[data$id_species==4 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Fr_S_B" | data$id_area=="Fr_S_O")]
data$incl[data$id_species==4 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Fr_S_B" | data$id_area=="Fr_S_O")] <- 0
data$vol[data$id_species==4 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Fr_S_B" | data$id_area=="Fr_S_O")] <- -0.015572 + 0.00290013*d - 7.0476*10^(-6)*d^2 + 2.393*10^(-6)*d^3 - 0.0013528*h + 3.9837*10^(-5)*d^2*h
# Fagus sylvatica (Pellinen, 1986)
d <- data$d130[data$id_species==4 & !is.na(data$id_species) & (data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O" | data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O")]
h <- data$h[data$id_species==4 & !is.na(data$id_species) & (data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O" | data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O")]
data$incl[data$id_species==4 & !is.na(data$id_species) & (data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O" | data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O")] <- 0
data$vol[data$id_species==4 & !is.na(data$id_species) & (data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O" | data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O")] <- 15.589*10^(-3)+0.01696*10^(-3)*d*h^2+0.01883*10^(-3)*d^3

# Frangula alnus (Estonian forest inventory)
d <- data$d130[data$id_species==22 & !is.na(data$id_species)]
h <- data$h[data$id_species==22 & !is.na(data$id_species)]
data$incl[data$id_species==22 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==22 & !is.na(data$id_species)] <- 0.0000785*d**2*h*(0.4033 + 0/d + 1.586/h + 1.440/(d*h))

# Fraxinus excelsior, Fraxinus angustifolia (Dagnelie, 1999)
d <- data$d130[data$id_species==57 & !is.na(data$id_species) | data$id_species==13 & !is.na(data$id_species)]
h <- data$h[data$id_species==57 & !is.na(data$id_species) | data$id_species==13 & !is.na(data$id_species)]
data$incl[data$id_species==57 & !is.na(data$id_species) | data$id_species==13 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==57 & !is.na(data$id_species) | data$id_species==13 & !is.na(data$id_species)] <- -0.039836 + 0.006262765*d - 0.00015937*d^2 - 1.9902*10^(-7)*d^3 - 0.0009834*h + 3.7872*10^(-5)*d^2*h

# Ilex aquifolium (generic)
d <- data$d130[data$id_species==18 & !is.na(data$id_species)]
h <- data$h[data$id_species==18 & !is.na(data$id_species)]
data$incl[data$id_species==18 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==18 & !is.na(data$id_species)] <- 0.42*pi*((d/100)/2)**2*h

# Juniperus communis (generic)
d <- data$d130[data$id_species==61 & !is.na(data$id_species)]
h <- data$h[data$id_species==61 & !is.na(data$id_species)]
data$incl[data$id_species==61 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==61 & !is.na(data$id_species)] <- 0.42*pi*((d/100)/2)**2*h

# Larix decidua (Dagnelie, 1999)
d <- data$d130[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O")]
h <- data$h[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O")]
data$incl[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O")] <- 0
data$vol[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O")] <- -0.03088 + 0.004676261*d - 4.8614*10^(-5)*d^2 - 3.8178*10^(-6)*d^3 - 0.0011638*h + 4.0597*10^(-5)*d^2*h
# Larix decidua (Pollanschütz, 1974)
d <- data$d130[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")]
h <- data$h[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")]
data$incl[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")] <- 0
data$vol[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")] <- ((pi/4)*(0.609443*(d/10)^2*(h*10)-0.0455748*(d/10)^2*(h*10)*log((d/10))^2-18.6631*(d/10)^2-0.248736*(d/10)*(h*10)+0.126594*(h*10)+36.9783*(d/10)-14.204))/1000
# Larix decidua (Øen, 2001)
d <- data$d130[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O")]
h <- data$h[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O")]
data$incl[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O")] <- 0
data$vol[data$id_species==10 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O")] <- (0.7761*h^3.6461*d^1.9166*(h-1.3)^(-2.3179)*(d+100)^(-0.8236))/1000

# Picea abies (Dagnelie, 1999)
d <- data$d130[data$id_species==7 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")]
h <- data$h[data$id_species==7 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")]
data$incl[data$id_species==7 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")] <- 0
data$vol[data$id_species==7 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")] <- -0.010929 + 0.004380951*d - 9.4713*10^(-5)*d^2* - 7.8024*10^(-6)*d^3 - 0.0027922*h + 4.8346*10^(-5)*d^2*h
# Picea abies (Näslund, 1947)
d <- data$d130[data$id_species==7 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Es_B" | data$id_area=="Es_O")]
h <- data$h[data$id_species==7 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Es_B" | data$id_area=="Es_O")]
data$incl[data$id_species==7 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Es_B" | data$id_area=="Es_O")] <- 0
data$vol[data$id_species==7 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Es_B" | data$id_area=="Es_O")] <- (0.115*d^2 + 0.01746*d^2*h + 0.02022*d*h^2 - 0.05618*h^2)/1000

# Pinus sylvestris (Dagnelie, 1999)
d <- data$d130[data$id_species==17 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")]
h <- data$h[data$id_species==17 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")]
data$incl[data$id_species==17 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")] <- 0
data$vol[data$id_species==17 & !is.na(data$id_species) & (data$id_area=="Be_B" | data$id_area=="Be_O" | data$id_area=="Fr_N_B" | data$id_area=="Fr_N_O" | data$id_area=="Ge_E_B" | data$id_area=="Ge_E_O" | data$id_area=="Ge_W_B" | data$id_area=="Ge_W_O")] <- -0.039836 + 4.871*10^(-3)*d - 6.1028*10^(-5)*d^2 + 1.4889*10^(-5)*d^3 + 7.3997*10^(-5)*h + 2.9221*10^(-5)*d^2*h
# Pinus sylvestris (Näslund, 1947)
d <- data$d130[data$id_species==17 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Es_B" | data$id_area=="Es_O")]
h <- data$h[data$id_species==17 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Es_B" | data$id_area=="Es_O")]
data$incl[data$id_species==17 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Es_B" | data$id_area=="Es_O")] <- 0
data$vol[data$id_species==17 & !is.na(data$id_species) & (data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O" | data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Es_B" | data$id_area=="Es_O")] <- (0.1028*d^2 + 0.02705*d^2*h + 0.005215*d*h^2 )/1000

# Populus x canadensis (tremula), Populus alba, Populus balsamifera, Populus sp. (Schelhaas, 2002)
d <- data$d130[data$id_species==5 & !is.na(data$id_species) | data$id_species==54 & !is.na(data$id_species) | data$id_species==53 & !is.na(data$id_species) | data$id_species==49 & !is.na(data$id_species) | data$id_species==98 & !is.na(data$id_species)]
h <- data$h[data$id_species==5 & !is.na(data$id_species) | data$id_species==54 & !is.na(data$id_species) | data$id_species==53 & !is.na(data$id_species) | data$id_species==49 & !is.na(data$id_species) | data$id_species==98 & !is.na(data$id_species)]
data$incl[data$id_species==5 & !is.na(data$id_species) | data$id_species==54 & !is.na(data$id_species) | data$id_species==53 & !is.na(data$id_species) | data$id_species==49 & !is.na(data$id_species) | data$id_species==98 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==5 & !is.na(data$id_species) | data$id_species==54 & !is.na(data$id_species) | data$id_species==53 & !is.na(data$id_species) | data$id_species==49 & !is.na(data$id_species) | data$id_species==98 & !is.na(data$id_species)] <- (0.0009507*(d*10)^(1.895629295 + 0.001650837)*h^0.8392146)/1000

# Populus tremula (Børset, 1954)
d <- data$d130[data$id_species==11 & !is.na(data$id_species)]
h <- data$h[data$id_species==11 & !is.na(data$id_species)]
data$incl[data$id_species==11 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==11 & !is.na(data$id_species)] <- (9.69 + 0.0365*d^2*h)/1000

# Prunus avium, Prunus padus, Prunus serotina, Prunus sp. (Dagnelie, 1999)
d <- data$d130[(data$id_species==15 | data$id_species==9 | data$id_species==41 | data$id_species==97) & !is.na(data$id_species)]
h <- data$h[(data$id_species==15 | data$id_species==9 | data$id_species==41 | data$id_species==97) & !is.na(data$id_species)]
s <- data$id_species[(data$id_species==15 | data$id_species==9 | data$id_species==41 | data$id_species==97) & !is.na(data$id_species)]
data$incl[(data$id_species==15 | data$id_species==9 | data$id_species==41 | data$id_species==97) & !is.na(data$id_species)] <- 0
data$vol[(data$id_species==15 | data$id_species==9 | data$id_species==41 | data$id_species==97) & !is.na(data$id_species)] <- -0.002311 - 0.00117728*d + 0.000149061*d^2 - 7.8058*10^(-6)*d^3 + 3.3282*10^(-4)*h + 3.1526*10^(-5)*d^2*h

# Pseudotsuga menziesii (Dagnelie, 1999)
d <- data$d130[data$id_species==24 & !is.na(data$id_species)]
h <- data$h[data$id_species==24 & !is.na(data$id_species)]
data$incl[data$id_species==24 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==24 & !is.na(data$id_species)] <- -0.019911 + 0.001871101*d + 0.000127328*d^2 - 5.7631*10^(-6)*d^3 + 0.00071591*h + 3.9371*10^(-5)*d^2*h

# Quercus petraea, Quercus robur (Dik, 1984)
d <- data$d130[(data$id_species==55 | data$id_species==6) & !is.na(data$id_species)]
h <- data$h[(data$id_species==55 | data$id_species==6) & !is.na(data$id_species)]
data$incl[(data$id_species==55 | data$id_species==6) & !is.na(data$id_species)] <- 0
data$vol[(data$id_species==55 | data$id_species==6) & !is.na(data$id_species)] <- (d^2.00333*h^0.85925*exp(-2.86353))/1000

# Quercus pubescens (Giurgiu, 1974)
d <- data$d130[data$id_species==56 & !is.na(data$id_species)]
h <- data$h[data$id_species==56 & !is.na(data$id_species)]
data$incl[data$id_species==56 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==56 & !is.na(data$id_species)] <- 0.00035164*10**(1.1119*log10(d) + 0.3108*log10(d)**2 + 0.5356*log10(h) + 0.2139*log10(h)**2)

# Quercus rubra (Dagnelie, 1999)
d <- data$d130[data$id_species==32 & !is.na(data$id_species)]
h <- data$h[data$id_species==32 & !is.na(data$id_species)]
data$incl[data$id_species==32 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==32 & !is.na(data$id_species)] <- -0.02149 + 0.002986681*d - 4.2506*10^(-5)*d^2 - 2.1806*10^(-6)*d^3 - 0.000743*h + 3.7473*10^(-5)*d^2*h

# Rhamnus cathartica (generic)
d <- data$d130[data$id_species==66 & !is.na(data$id_species)]
h <- data$h[data$id_species==66 & !is.na(data$id_species)]
data$incl[data$id_species==66 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==66 & !is.na(data$id_species)] <- 0.42*pi*((d/100)/2)**2*h

# Robinia pseudoacacia (generic)
d <- data$d130[data$id_species==47 & !is.na(data$id_species)]
h <- data$h[data$id_species==47 & !is.na(data$id_species)]
data$incl[data$id_species==47 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==47 & !is.na(data$id_species)] <- 0.42*pi*((d/100)/2)**2*h

# Salix alba, Salix cinerea (x fragilis), Salix fragilis, Salix petendra (Giurgiu, 1974)
d <- data$d130[data$id_species==21 & !is.na(data$id_species) | data$id_species==45 & !is.na(data$id_species) | data$id_species==65 & !is.na(data$id_species) | data$id_species==52 & !is.na(data$id_species) | data$id_species==43 & !is.na(data$id_species)]
h <- data$h[data$id_species==21 & !is.na(data$id_species) | data$id_species==45 & !is.na(data$id_species) | data$id_species==65 & !is.na(data$id_species) | data$id_species==52 & !is.na(data$id_species) | data$id_species==43 & !is.na(data$id_species)]
s <- data$id_species[data$id_species==21 & !is.na(data$id_species) | data$id_species==45 & !is.na(data$id_species) | data$id_species==65 & !is.na(data$id_species) | data$id_species==52 & !is.na(data$id_species) | data$id_species==43 & !is.na(data$id_species)]
data$incl[data$id_species==21 & !is.na(data$id_species) | data$id_species==45 & !is.na(data$id_species) | data$id_species==65 & !is.na(data$id_species) | data$id_species==52 & !is.na(data$id_species) | data$id_species==43 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==21 & !is.na(data$id_species) | data$id_species==45 & !is.na(data$id_species) | data$id_species==65 & !is.na(data$id_species) | data$id_species==52 & !is.na(data$id_species) | data$id_species==43 & !is.na(data$id_species)] <- 4.281*10^(-5)*10^(2.0766*log10(d) - 0.1296*log10(d)^2 + 0.6843*log10(h) + 0.2745*log10(h)^2)

# Salix caprea (Giurgiu, 1974)
d <- data$d130[(data$id_species==20 | data$id_species==64) & !is.na(data$id_species)]
h <- data$h[(data$id_species==20 | data$id_species==64) & !is.na(data$id_species)]
data$incl[(data$id_species==20 | data$id_species==64) & !is.na(data$id_species)] <- 0
data$vol[(data$id_species==20 | data$id_species==64) & !is.na(data$id_species)] <- 0.00011585*10^(1.6688*log10(d) + 0.109*log10(d)^2 + 0.7781*log10(h) + 0.0269*log10(h)^2)
# Salix caprea (Braastad, 1996)
d <- data$d130[(data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O") & (data$id_species==20 | data$id_species==64) & !is.na(data$id_species)]
h <- data$h[(data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O") & (data$id_species==20 | data$id_species==64) & !is.na(data$id_species)]
data$incl[(data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O") & (data$id_species==20 | data$id_species==64) & !is.na(data$id_species)] <- 0
data$vol[(data$id_area=="Sw_C_B" | data$id_area=="Sw_C_O" | data$id_area=="Sw_S_B" | data$id_area=="Sw_S_O") & (data$id_species==20 | data$id_species==64) & !is.na(data$id_species)] <- (-1.86827 + 0.21461*d^2 + 0.01283*d^2*h + 0.0138*h^2*d - 0.06311*h^2)/1000

# Sambucus nigra, Sambucus racemosa (generic)
d <- data$d130[data$id_species==25 & !is.na(data$id_species) | data$id_species==33 & !is.na(data$id_species)]
h <- data$h[data$id_species==25 & !is.na(data$id_species) | data$id_species==33 & !is.na(data$id_species)]
data$incl[data$id_species==25 & !is.na(data$id_species) | data$id_species==33 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==25 & !is.na(data$id_species) | data$id_species==33 & !is.na(data$id_species)] <- NA

# Sorbus aucuparia, Sorbus torminalis (Braastad, 1996)
d <- data$d130[data$id_species==14 & !is.na(data$id_species) | data$id_species==19 & !is.na(data$id_species)]
h <- data$h[data$id_species==14 & !is.na(data$id_species) | data$id_species==19 & !is.na(data$id_species)]
s <- data$id_species[data$id_species==14 & !is.na(data$id_species) | data$id_species==19 & !is.na(data$id_species)]
data$incl[data$id_species==14 & !is.na(data$id_species) | data$id_species==19 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==14 & !is.na(data$id_species) | data$id_species==19 & !is.na(data$id_species)] <- (-1.86827+0.21461*d^2+0.01283*d^2*h+0.0138*h^2*d-0.06311*h^2)/1000

# Tilia cordata, Tilia platyphyllos, Tilia x europea, Tilis sp. (Estonian forest inventory)
d <- data$d130[(data$id_species==16 | data$id_species==37 | data$id_species==95 | data$id_species==59) & !is.na(data$id_species)]
h <- data$h[(data$id_species==16 | data$id_species==37 | data$id_species==95 | data$id_species==59) & !is.na(data$id_species)]
s <- data$id_species[(data$id_species==16 | data$id_species==37 | data$id_species==95 | data$id_species==59) & !is.na(data$id_species)]
data$incl[(data$id_species==16 | data$id_species==37 | data$id_species==95 | data$id_species==59) & !is.na(data$id_species)] <- 0
data$vol[(data$id_species==16 | data$id_species==37 | data$id_species==95 | data$id_species==59) & !is.na(data$id_species)] <- 0.0000785*d**2*h*(0.4080 + 0.757/d + 0.801/h + -10.707/(d*h))

# Ulmus glabra, Ulmus minor (Dagnelie, 1999)
d <- data$d130[data$id_species==29 & !is.na(data$id_species) | data$id_species==40 & !is.na(data$id_species)]
h <- data$h[data$id_species==29 & !is.na(data$id_species) | data$id_species==40 & !is.na(data$id_species)]
data$incl[data$id_species==29 & !is.na(data$id_species) | data$id_species==40 & !is.na(data$id_species)] <- 0
data$vol[data$id_species==29 & !is.na(data$id_species) | data$id_species==40 & !is.na(data$id_species)] <- -0.034716 + 0.004268168*d - 0.00013227*d^2 - 1.7667*10^(-6)*d^3 + 0.00016516*h + 3.8311*10^(-5)*d^2*h


# --> country-specific equations
# Estonia (vol_alt <- 0.0000785*d130**2*h*(a + b/d130 + c/h + d/(d130/h)), with a, b, c, d as group-specific parameters)
# author unknown
# Pinus sylvestris, Larix sp., 
d <- data$d130[data$id_region=="Es" & (data$id_species==17 | data$id_species==10) & !is.na(data$id_species)]
h <- data$h[data$id_region=="Es" & (data$id_species==17 | data$id_species==10) & !is.na(data$id_species)]
data$incl[data$id_region=="Es" & (data$id_species==17 | data$id_species==10) & !is.na(data$id_species)] <- 0
data$vol_alt[data$id_region=="Es" & (data$id_species==17 | data$id_species==10) & !is.na(data$id_species)] <- data$vol[data$id_region=="Es" & (data$id_species==17 | data$id_species==10) & !is.na(data$id_species)]
data$vol[data$id_region=="Es" & (data$id_species==17 | data$id_species==10) & !is.na(data$id_species)] <- 0.0000785*d**2*h*(0.3571 + 0.660/d + 2.156/h + -8.312/(d*h))

# Picea abies, Pseudotsuga menziesii
d <- data$d130[data$id_region=="Es" & (data$id_species==7 | data$id_species==24) & !is.na(data$id_species)]
h <- data$h[data$id_region=="Es" & (data$id_species==7 | data$id_species==24) & !is.na(data$id_species)]
data$incl[data$id_region=="Es" & (data$id_species==7 | data$id_species==24) & !is.na(data$id_species)] <- 0
data$vol_alt[data$id_region=="Es" & (data$id_species==7 | data$id_species==24) & !is.na(data$id_species)] <- data$vol[data$id_region=="Es" & (data$id_species==7 | data$id_species==24) & !is.na(data$id_species)]
data$vol[data$id_region=="Es" & (data$id_species==7 | data$id_species==24) & !is.na(data$id_species)] <- 0.0000785*d**2*h*(0.4216 + 0.181/d + 1.190/h + -1.309/(d*h))

# Betula pendula, B. pubescens, Betula sp, Tilia cordata, T. platyphylos, T. sp., T. x europea
d <- data$d130[data$id_region=="Es" & (data$id_species==3 | data$id_species==12 | data$id_species==96 | data$id_species==16 | data$id_species==37 | data$id_species==95 | data$id_species==59) & !is.na(data$id_species)]
h <- data$h[data$id_region=="Es" & (data$id_species==3 | data$id_species==12 | data$id_species==96 | data$id_species==16 | data$id_species==37 | data$id_species==95 | data$id_species==59) & !is.na(data$id_species)]
data$incl[data$id_region=="Es" & (data$id_species==3 | data$id_species==12 | data$id_species==96 | data$id_species==16 | data$id_species==37 | data$id_species==95 | data$id_species==59) & !is.na(data$id_species)] <- 0
data$vol_alt[data$id_region=="Es" & (data$id_species==3 | data$id_species==12 | data$id_species==96 | data$id_species==16 | data$id_species==37 | data$id_species==95 | data$id_species==59) & !is.na(data$id_species)] <- data$vol[data$id_region=="Es" & (data$id_species==3 | data$id_species==12 | data$id_species==96 | data$id_species==16 | data$id_species==37 | data$id_species==95 | data$id_species==59) & !is.na(data$id_species)]
data$vol[data$id_region=="Es" & (data$id_species==3 | data$id_species==12 | data$id_species==96 | data$id_species==16 | data$id_species==37 | data$id_species==95 | data$id_species==59) & !is.na(data$id_species)] <- 0.0000785*d**2*h*(0.4080 + 0.757/d + 0.801/h + -10.707/(d*h))

# Populus alba, P. balsamifera, P. tremula, P. sp., P. x canadensis, P. canadensis x tremula, Alnus glutinosa, A. incana, Salix alba, S. caprea, S. caprea x fragilis, S. cinearea x fragilis, S. petrandra, S. sp.
d <- data$d130[data$id_region=="Es" & (data$id_species==53 | data$id_species==49 | data$id_species==98 | data$id_species==11 | data$id_species==5 | data$id_species==54 | data$id_species==1 | data$id_species==31 | data$id_species==21 | data$id_species==20 | data$id_species==64 | data$id_species==45 | data$id_species==65 | data$id_species==52 | data$id_species==43 | data$id_species==94) & !is.na(data$id_species)]
h <- data$h[data$id_region=="Es" & (data$id_species==53 | data$id_species==49 | data$id_species==98 | data$id_species==11 | data$id_species==5 | data$id_species==54 | data$id_species==1 | data$id_species==31 | data$id_species==21 | data$id_species==20 | data$id_species==64 | data$id_species==45 | data$id_species==65 | data$id_species==52 | data$id_species==43 | data$id_species==94) & !is.na(data$id_species)]
data$incl[data$id_region=="Es" & (data$id_species==53 | data$id_species==49 | data$id_species==98 | data$id_species==11 | data$id_species==5 | data$id_species==54 | data$id_species==1 | data$id_species==31 | data$id_species==21 | data$id_species==20 | data$id_species==64 | data$id_species==45 | data$id_species==65 | data$id_species==52 | data$id_species==43 | data$id_species==94) & !is.na(data$id_species)] <- 0
data$vol_alt[data$id_region=="Es" & (data$id_species==53 | data$id_species==49 | data$id_species==98 | data$id_species==11 | data$id_species==5 | data$id_species==54 | data$id_species==1 | data$id_species==31 | data$id_species==21 | data$id_species==20 | data$id_species==64 | data$id_species==45 | data$id_species==65 | data$id_species==52 | data$id_species==43 | data$id_species==94) & !is.na(data$id_species)] <- data$vol[data$id_region=="Es" & (data$id_species==53 | data$id_species==49 | data$id_species==98 | data$id_species==11 | data$id_species==5 | data$id_species==54 | data$id_species==1 | data$id_species==31 | data$id_species==21 | data$id_species==20 | data$id_species==64 | data$id_species==45 | data$id_species==65 | data$id_species==52 | data$id_species==43 | data$id_species==94) & !is.na(data$id_species)]
data$vol[data$id_region=="Es" & (data$id_species==53 | data$id_species==49 | data$id_species==98 | data$id_species==11 | data$id_species==5 | data$id_species==54 | data$id_species==1 | data$id_species==31 | data$id_species==21 | data$id_species==20 | data$id_species==64 | data$id_species==45 | data$id_species==65 | data$id_species==52 | data$id_species==43 | data$id_species==94) & !is.na(data$id_species)] <- 0.0000785*d**2*h*(0.4723 + -0.608/d + 0/h + 12.724/(d*h))

# Quercus petrea, Q. pubescens, Q. rubra, Q. robur, Q. sp., Fraxinus excelsior, F. angustifolia, Acer pseudoplatanus, A. platanoides, A. campestre, Prunus avium, P. padus, Corylus avellana, Sorbus aucuparia, Frangula alnus, Ulmus glabra
d <- data$d130[data$id_region=="Es" & (data$id_species==55 | data$id_species==56 | data$id_species==6 | data$id_species==32 | data$id_species==57 | data$id_species==13 | data$id_species==38 | data$id_species==36 | data$id_species==26 | data$id_species==106 | data$id_species==8 | data$id_species==9 | data$id_species==14 | data$id_species==15 | data$id_species==22 | data$id_species==29) & !is.na(data$id_species)]
h <- data$h[data$id_region=="Es" & (data$id_species==55 | data$id_species==56 | data$id_species==6 | data$id_species==32 | data$id_species==57 | data$id_species==13 | data$id_species==38 | data$id_species==36 | data$id_species==26 | data$id_species==106 | data$id_species==8 | data$id_species==9 | data$id_species==14 | data$id_species==15 | data$id_species==22 | data$id_species==29) & !is.na(data$id_species)]
data$incl[data$id_region=="Es" & (data$id_species==55 | data$id_species==56 | data$id_species==6 | data$id_species==32 | data$id_species==57 | data$id_species==13 | data$id_species==38 | data$id_species==36 | data$id_species==26 | data$id_species==106 | data$id_species==8 | data$id_species==9 | data$id_species==14 | data$id_species==15 | data$id_species==22 | data$id_species==29) & !is.na(data$id_species)] <- 0
data$vol_alt[data$id_region=="Es" & (data$id_species==55 | data$id_species==56 | data$id_species==6 | data$id_species==32 | data$id_species==57 | data$id_species==13 | data$id_species==38 | data$id_species==36 | data$id_species==26 | data$id_species==106 | data$id_species==8 | data$id_species==9 | data$id_species==14 | data$id_species==15 | data$id_species==22 | data$id_species==29) & !is.na(data$id_species)] <- data$vol[data$id_region=="Es" & (data$id_species==55 | data$id_species==56 | data$id_species==6 | data$id_species==32 | data$id_species==57 | data$id_species==13 | data$id_species==38 | data$id_species==36 | data$id_species==26 | data$id_species==106 | data$id_species==8 | data$id_species==9 | data$id_species==14 | data$id_species==15 | data$id_species==22 | data$id_species==29) & !is.na(data$id_species)]
data$vol[data$id_region=="Es" & (data$id_species==55 | data$id_species==56 | data$id_species==6 | data$id_species==32 | data$id_species==57 | data$id_species==13 | data$id_species==38 | data$id_species==36 | data$id_species==26 | data$id_species==106 | data$id_species==8 | data$id_species==9 | data$id_species==14 | data$id_species==15 | data$id_species==22 | data$id_species==29) & !is.na(data$id_species)] <- 0.0000785*d**2*h*(0.4033 + 0/d + 1.586/h + 1.440/(d*h))

# France (vol_alt <- (h*d130**2)/(4*pi*(1-1.3/h)**2)*(a + b*(sqrt(d130)/h) + c*(h/d130)) with a, b and c as species-specific parameters)
# Maillet et al. (2014)
# Acer campestre (just for completeness here, basically duplicate of above)
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==38 & !is.na(data$id_species)]
d <- d*pi # transformation since french equations are based on circumference instead of diameter
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==38 & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==38 & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==38 & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==38 & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==38 & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.534 + 0.661*(sqrt(d/100)/h) - 0.002*(h/(d/100)))

# Acer pseudoplatanus(, Acer platanoides, Acer sp.)
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==26 | data$id_species==36 | data$id_species==93)   & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==26 | data$id_species==36 | data$id_species==93)   & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==26 | data$id_species==36 | data$id_species==93)   & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==26 | data$id_species==36 | data$id_species==93)   & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==26 | data$id_species==36 | data$id_species==93)   & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==26 | data$id_species==36 | data$id_species==93)   & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.502 + 0.661*(sqrt(d/100)/h) - 0.002*(h/(d/100)))

# Betula pendula
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==3 & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==3 & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==3 & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==3 & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==3 & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==3 & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.493 + 0.661*(sqrt(d/100)/h) - 0.002*(h/(d/100)))

# Carpinus betulus
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==2 & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==2 & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==2 & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==2 & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==2 & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==2 & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.533 + 0.661*(sqrt(d/100)/h) - 0.001*(h/(d/100)))

# Fagus sylvatica
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==4 & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==4 & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==4 & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==4 & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==4 & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==4 & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.542 + 0.661*(sqrt(d/100)/h) - 0.002*(h/(d/100)))

# Fraxinus excelsior
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==13 & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==13 & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==13 & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==13 & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==13 & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==13 & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.509 + 0.661*(sqrt(d/100)/h) - 0.001*(h/(d/100)))

# Prunus avium
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==15 & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==15 & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==15 & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==15 & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==15 & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==15 & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.521 + 0.661*(sqrt(d/100)/h) - 0.002*(h/(d/100)))

# Quercus robur, Quercus petrea (also Castanea sativa)
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==6 | data$id_species==55 | data$id_species==42) & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==6 | data$id_species==55 | data$id_species==42) & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==6 | data$id_species==55 | data$id_species==42) & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==6 | data$id_species==55 | data$id_species==42) & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==6 | data$id_species==55 | data$id_species==42) & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & (data$id_species==6 | data$id_species==55 | data$id_species==42) & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.561 + 0.661*(sqrt(d/100)/h) - 0.002*(h/(d/100)))

# Quercus rubra
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==32 & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==32 & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==32 & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==32 & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==32 & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==32 & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.511 + 0.661*(sqrt(d/100)/h) - 0.002*(h/(d/100)))

# Larix decidua
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==10 & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==10 & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==10 & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==10 & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==10 & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==10 & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.377 + 1.756*(sqrt(d/100)/h) + 0.001*(h/(d/100)))

# Picea abies
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==7 & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==7 & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==7 & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==7 & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==7 & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==7 & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.303 + 1.756*(sqrt(d/100)/h) + 0.004*(h/(d/100)))

# Pinus sylvestris
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==17 & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==17 & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==17 & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==17 & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==17 & !is.na(data$id_species)]
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==17 & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.372 + 1.756*(sqrt(d/100)/h) + 0.001*(h/(d/100)))

# Pseudotsuga menziesii
d <- data$d130[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==24 & !is.na(data$id_species)]
d <- d*pi
h <- data$h[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==24 & !is.na(data$id_species)]
data$incl[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==24 & !is.na(data$id_species)] <- 1
data$vol_alt[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==24 & !is.na(data$id_species)] <- data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==24 & !is.na(data$id_species)]
data$vol[(data$id_region=="Fr_N" | data$id_region=="Fr_S") & data$id_species==24 & !is.na(data$id_species)] <- (h*(d/100)**2)/(4*pi*(1-1.3/h)**2)*(0.235 + 1.756*(sqrt(d/100)/h) + 0.004*(h/(d/100)))


# calculate deviation from generic estimation
data$dev1 <- data$vol/data$volume_gen1
data$dev2 <- data$vol/data$volume_gen2
data$dev3 <- data$vol_alt/data$volume_gen1
data$dev4 <- data$vol_alt/data$volume_gen2

# I am determining the bcef-value based on the total volume per stand, the woodtype and the question if volume has been calculated including branches, etc or not. I am determining such a value for each individual tree to multiply its volume by the bcef-value.
# I am determining thr root-shoot ratio based on the total biomass per stand and proceed like I did for bcef-values.
for(i in levels(data$id_point)){
  md_total_temp <- round(mean(data$dis[data$id_point==i], na.rm=T), 2)
  dens_total_temp <- round(1/md_total_temp**2*10000)
  n_ind_data_temp <- length(data$id_species[data$id_point==i][!is.na(data$id_species[data$id_point==i])])
  vol_total_temp <- round(sum(data$vol[data$id_point==i], na.rm=T)/n_ind_data_temp*dens_total_temp, 2)
  for(j in 1:length(data$vol[data$id_point==i & !is.na(data$vol[data$id_point==i])])){
    bcef_temp <- bcef$bcef[which(bcef$type==data$wt[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] & bcef$include==data$incl[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] & bcef$stock_lb <= vol_total_temp & bcef$stock_ub > vol_total_temp)]
    data$biomass_aboveground[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] <- data$vol[data$id_point==i & !is.na(data$vol[data$id_point==i])][j]*bcef_temp
  }
  biomass_total_temp <- round(sum(data$biomass_aboveground[data$id_point==i], na.rm=T)/n_ind_data_temp*dens_total_temp, 2)
  for(j in 1:length(data$vol[data$id_point==i & !is.na(data$vol[data$id_point==i])])){
    rs_ratio_temp <- root_shoot$ratio[which(root_shoot$rs_group==data$rs_group[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] & root_shoot$biomass_lb <= biomass_total_temp & root_shoot$biomass_ub > biomass_total_temp)]
    rs_ratio_lb_temp <- root_shoot$ratio_lb[which(root_shoot$rs_group==data$rs_group[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] & root_shoot$biomass_lb <= biomass_total_temp & root_shoot$biomass_ub > biomass_total_temp)]
    rs_ratio_ub_temp <- root_shoot$ratio_ub[which(root_shoot$rs_group==data$rs_group[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] & root_shoot$biomass_lb <= biomass_total_temp & root_shoot$biomass_ub > biomass_total_temp)]
    data$biomass_belowground[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] <- data$biomass_aboveground[data$id_point==i & !is.na(data$vol[data$id_point==i])][j]*rs_ratio_temp
    data$biomass_belowground_lb[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] <- data$biomass_aboveground[data$id_point==i & !is.na(data$vol[data$id_point==i])][j]*rs_ratio_lb_temp
    data$biomass_belowground_ub[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] <- data$biomass_aboveground[data$id_point==i & !is.na(data$vol[data$id_point==i])][j]*rs_ratio_ub_temp
    data$biomass_tree[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] <- data$biomass_aboveground[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] + data$biomass_belowground[data$id_point==i & !is.na(data$vol[data$id_point==i])][j]
    data$biomass_tree_lb[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] <- data$biomass_aboveground[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] + data$biomass_belowground_lb[data$id_point==i & !is.na(data$vol[data$id_point==i])][j]
    data$biomass_tree_ub[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] <- data$biomass_aboveground[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] + data$biomass_belowground_ub[data$id_point==i & !is.na(data$vol[data$id_point==i])][j]
    data$carbon[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] <- data$biomass_tree[data$id_point==i & !is.na(data$vol[data$id_point==i])][j]*0.5
    data$carbon_lb[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] <- data$biomass_tree_lb[data$id_point==i & !is.na(data$vol[data$id_point==i])][j]*0.5
    data$carbon_ub[data$id_point==i & !is.na(data$vol[data$id_point==i])][j] <- data$biomass_tree_ub[data$id_point==i & !is.na(data$vol[data$id_point==i])][j]*0.5
  }
}

# References:
# Børset, O. (1954): Kubering av osp på rot. Meddelelser fra det norske Skogforsøksvesen 12: 391–447.
# Braastad, H. (1966): Volumtabeller for bjørk. Meddelelser fra det Norske Skogforsøksvesen 21(1): 23–78.
# Dagnelie, P., Palm, R., Rondeux, J. & Thill, A. (1999): Tables de cubage des arbres et des peuple- ments forestiers. Les Presses Agronomiques de Gembloux, Gembloux. 126 p.
# Dik, E.J. (1984): Estimating the wood volume of standing trees in forestry practice. Rijksinstituut voor onderzoek in de bos en landschapsbouw de Dorschkamp, Wageningen. Uitvoerige verslagen 19(1): 1–114.
# Eriksson, H. (1973): Volymfunktioner för ståendeträd av ask, asp, klibbal och contorta-tall. Institutionen för Skogsproduktion, Royal College of Forestry, Stockholm. Research Notes 26: 1–26.
# Estonian forest inventory (ask for additional data from Jaan Liira)
# Giurgiu, V. (1974): O expresie matematica unica a relatiei diametru – înaltime – volum, pentru majori- tatea speciilor forestiere din Romania. Silvicultura si Exploatarea Padurilor 89(4): 173–178.
# Näslund, M. (1947): Funktioner och tabeller för kubering av stående träd. Meddelanden från Statens skogsforskningsinstitutet 36(3): 1–81.
# Øen, S., Bauger, E. & Øyen, B.-H. (2001): Functionar for volumberekning av framande treslag i Vest-Norge. Aktuelt fra Skogforsk 3/01: 18–19.
# Pellinen, P. (1986): Biomasseuntersuchungen im Kalkbuchenwald. University of Göttingen, Germany. 145 p.
# Schelhaas, M.J., Nabuurs, G.J., Jans, W.W.P., Moors, E.J., Sabaté, S. & Daamen, W.P. (2002): Converging estimates of the forest carbon sink. Alterra-rapport 631: 1–44.
# Zianis, D., Muukkonen, P., Mäkipää, R., Mencuccini, M. (2005): Biomass and stem volume equations for tree species in Europe. Silva Fennica Monographs 4
# -> BCEFs and carbon fraction
# Aalde et al. (2006) - IPCC Guidelines for National Greenhouse Gas Inventories (Tables 4.4 and 4.3)
# -> root:shoot ratio
# Mokany, Karel, Raison, R. John, Prokushkin, Anatoly S. (2006): Critical analysis of root shoot ratios in terrestrial biomes. Global Change Biology 12: 84–96.