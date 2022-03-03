############## Cours de statistique descriptive ##################

#################################################################


##### Packages R pour les graphiques du support ####



mypackages<-c("psych","tidyverse","corrplot")

check_fct<-function(pkg){
  if(!require(pkg,character.only = TRUE)){
    install.packages(pkg,dependencies = TRUE)
    library(pkg,character.only = TRUE)
  }
}

check_mypackages<-lapply(mypackages,check_fct)



#### Chapitre 2: Représentation graphique et tableaux ####

## Variable qualitative nominale##

# Création du vecteur en chaine des caractères

Data1 <- c('D', 'C', 'M', 'M', 'C', 'A', 'D', 'C', 'D', 'M', 'A', 'M', 'D', 'C', 'A', 'M',
           'D', 'A', 'M', 'C', 'A', 'M', 'A', 'M') 

Data1 <- as.factor(Data1) ## Création de la variable qualitative

## Rénommer les modalités

levels(Data1) <- c("Autres", "Célibataire", "Divorcé(e)", "Marié(e)") 


Tab1 = table(Data1) # Création de la table
Tab = as.data.frame(Tab1)
Tab$Freq_relat <- round((Tab$Freq/sum(Tab$Freq)) *100,0) # Fréquence relative

colnames(Tab) <- c("Etat_civil", "Effectifs", "Freq_relative")




Objet = c(Tab1) # Création de l'objet table1  


# Tableau statistique

## Création du Jeu de données

Data_fr1 <- data.frame(Effectif = Objet, Frequence = Objet/sum(Objet)) 

## Graphique Camembert

library(ggplot2)

couleurs <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")

ggplot(Tab, aes(x = "", y = Freq_relative, fill = Etat_civil)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = couleurs) +
  labs(title = "Graphique sur l'état civil")+
  geom_text(aes(label = paste(Freq_relative,"%")),
            position = position_stack(vjust = 0.5))+
  #geom_text(aes(y = Position_etiq , label = Freq_relative), color = "white")+
  theme_void()

## Graphique en Barres

ggplot(data = Tab, mapping = aes(x = Etat_civil, y = Effectifs , fill =  Etat_civil )) +
  geom_col() +                        
  labs(title = "Graphique sur l'état civil", fill = "État civil")+
  scale_fill_manual(values = couleurs)+
  geom_text(aes(label = Effectifs), vjust = 1.6, color = "white")


## Variable qualitative ordinale##

Niveau_sat <- c('Tr', 'Tr', 'Tr', 'Tr', 'Tr', 'Tr', 'Tr', 'Tr', 'Tr', 'Tr',
                'Tr', 'Tr', 'Tr', 'Tr', 'Tr', 'Tr', 'Tr', 'Tr', 'Pl', 'Pl',
                'Pl', 'Pl', 'Pl', 'Pl', 'Pl', 'Pl', 'Pl', 'Pl', 'Ni', 'Ni',
  'Ni', 'Pl.ins',  'Pl.ins', 'Pl.ins', 'Pl.ins', 'Pl.ins', 'Tr.ins',
  'Tr.ins', 'Tr.ins' , 'Tr.ins')

## Création de la variable qualitative

Niveau_sat <- factor(Niveau_sat, levels = c('Tr','Pl','Ni','Pl.ins',
                                            'Tr.ins'), ordered = TRUE)


Tab2 <- table(Niveau_sat) # Création de la table

# Création du jeu de données

Tab12 <- as.data.frame(Tab2) 
colnames(Tab12) <- c("Niveau_sat", "Effectifs")
Tab12$Effect_Cum <- cumsum(Tab12$Effectifs)
Tab12$Freq_relat <- round((Tab12$Effectifs/sum(Tab12$Effectifs)) *100,0)

Objet2 <- c(Tab2) # Création de l'objet table2  

# Tableau statistique

Data_fr2 <- data.frame(Effectif = Objet2, Eff_Cum = cumsum(Objet2), 
                       Frequence = Objet2/sum(Objet2), 
                       Freq_Cum = cumsum(Objet2/sum(Objet2))) ## Création du Jeu de données


## Graphique Camembert

library(ggplot2)

couleurs1 <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#00AFBB")

ggplot(Tab12, aes(x = "", y = Freq_relat, fill = Niveau_sat)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = couleurs1) +
  labs(title = "Graphique sur le niveau de satisfaction du produit")+
  geom_text(aes(label = paste(Freq_relat,"%")),
            position = position_stack(vjust = 0.5))+
  theme_void()



## Graphique en Barres
library(ggplot2)

ggplot(data = Tab12, mapping = aes(x = Niveau_sat, y = Effectifs , fill =  Niveau_sat )) +
  geom_col() +                        
  labs(title = "Graphique sur le niveau de satisfaction du produit",
       fill = "Niveau de satisfaction")+
  scale_fill_manual(values = couleurs1)+
  geom_text(aes(label = Effectifs), vjust = 1.6, color = "white")


ggplot(data = Tab12, mapping = aes(x = Niveau_sat, y = Effect_Cum , fill =  Niveau_sat )) +
  geom_col() +                        
  labs(title = "Graphique sur le niveau de satisfaction du produit cumulé",
       fill = "Niveau de satisfaction")+
  scale_fill_manual(values = couleurs1)+
  geom_text(aes(label = Effect_Cum), vjust = 1.6, color = "white")


## Variables quantitatives discrètes

Indiv <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3,
           3, 3, 3, 4, 4, 4, 4, 4, 4, 4,
           5, 5, 5, 6, 6, 7, 7, 7,  8, 8)


Tab3 = table(Indiv ) # Création de la table
Tab13 = as.data.frame(Tab3) # Création du jeu de données

colnames(Tab13) <- c("Nbre_indiv", "Effectifs")
Tab13$Effect_Cum <- cumsum(Tab13$Effectifs)

Objet3 = c(Tab3) # Création de l'objet table2  

# Tableau statistique

Data_fr3 <- data.frame(Effectif = Objet3, Eff_Cum = cumsum(Objet3), 
                       Frequence = Objet3/sum(Objet3), 
                       Freq_Cum = cumsum(Objet3/sum(Objet3))) ## Création du Jeu de données


## Graphique en batons

library(ggplot2)
  
  plot(Tab3,type="h",xlab="",ylab="",
       main="Graphique sur le nombre d'individus par ménage",frame=0,lwd=3)

  
## Histograme
  
  hist(Tab3)
  
## Variable continues

  
Poids <- c(22.28,	23.18,	23.47,	23.72,	24.09,	24.56,
              22.56,	23.23,	23.48,	23.48,	24.13,	24.63,
              22.57,	23.29,	23.48,	23.48,	24.32,	24.83,
              22.60,	23.30,	23.49,	23.49,	24.35,	24.94,
              22.69,	23.34,	23.51,	23.51,	24.36,	24.95,
              22.73,	23.35,	23.56,	23.56,	24.37,	25.00,
              22.78,	23.35,	23.57,	23.57,	24.41,	25.07,
              22.91,	23.37,	23.60,	23.60,	24.43,	25.16,
              23.05,	23.39,	23.61,	23.61,	24.43,	25.48,
              23.14,	23.47,	23.71,	23.71,	24.52,	25.74)

E = diff(range(Poids)) ## y(n) - y(1)

K = 1+3.3* log(60, base = 10) # Règle de Sturges pour le nombre des classes

I = E/K ## Intervelle des classes


## Construction de classe

## Couper les classes

Tab4 = table(cut(Poids,
                 breaks=c(22.2, 22.78, 23.28, 23.78, 24.28, 24.78, 25.28, 25.78)))
Tab5= c(Tab4)

# Tableau statistique

Data_fr4 <- data.frame(Eff=Tab5,EffCum=cumsum(Tab5),Freq=Tab5/sum(Tab5),
                       FreqCum=cumsum(Tab5/sum(Tab5)))

## Graphique histograme

hist(Poids, breaks = c(22.28, 22.78, 23.28, 23.78, 24.28, 24.78, 25.28, 25.78))

library(ggplot2)

df1 <- data.frame(Poids)

ggplot(df1, aes(x=Poids))+
  geom_histogram(color="darkblue", fill="lightblue", 
                 breaks = c(22.28, 22.78, 23.28, 23.78, 24.28, 24.78, 25.28, 25.78))+
  labs(title = "Distribution de la longueur totale de crâne (mm) de 60 souris sylvestres",
       x = "La longueur totale du crâne (mm) de souris sylvestres" , y = "Effectifs")

#### Chapitre 3: Statistique univariée ####

## Mediane quand n est impair

x= c(100, 800, 500, 350, 400, 200, 650, 900, 830)
x1 = sort(x)
median(x1)
plot(ecdf(x),xlab="Salaires",ylab="Proportion",
     main="Salaire médian des employés de l'entreprise Ferlon",frame=FALSE,yaxt = "n")
axis(2, c(0.0,0.25,0.50,0.75,1.00))
arrows(0, 0.50, 500, 0.50, length=0.14,col="blue")
arrows(500,0.50,500,0,length=0.14,col="blue")


## Graphique Box-plot pour le poids



df1 <- data.frame(Poids)

ggplot(df1, aes(x=Poids, y = ""))+
  geom_boxplot(color="darkblue", fill="lightblue")+
  labs(title = "Distribution du poids des serpents à la naissance.  ", 
       x = "Le poids des serpents nouveaux nés (en gramme)")

## Graphique Box-plot pour different vacci

Pfizer <- c(14, 56, 37, 93, 56, 37 , 90, 67, 87, 35, 89, 43)
Moderna <- c(53, 67, 83, 25, 78, 66, 35, 97, 19, 26, 36, 25)
Astra_Zeneca <- c(56, 27, 39, 68, 10, 67, 53, 36, 38,19, 25, 28)

Vaccin2 <- data.frame(Pfizer, Moderna , Astra_Zeneca) ## Création du Jeu de données

Vaccin1 <- pivot_longer(Vaccin2, cols = c('Pfizer', 'Moderna', 'Astra_Zeneca'),
                        names_to = 'Vaccin',
                        values_to = 'Nombre',
                        values_drop_na = TRUE) # Transformation de données large en longue 

head(Vaccin1) ## Visualisation
Vaccin1$Vaccin <- as.factor(Vaccin1$Vaccin)

boxplots <- ggplot(data = Vaccin1) + 
  geom_boxplot(mapping = aes(x = Vaccin, y = Nombre, fill = Vaccin)) +
  labs(
    x = "Vaccin",
    y = "Nombre de vaccinés",
    title = "Distribution du nombre de vaccinés"
  )



## Graphique du coefficient d'asymétrie

x1 <- seq(-3,3,.01)
y <- dnorm(x1, 0, 1)
y2 <- dnorm(x1, 2, 0.8)
y3 <- dnorm(x1, -2, 2/sqrt(pi))
leg.txt <- c("Aucune"," À gauche","À droite")
plot(x1, y, xlab="x", ylab="f(x)", main="Coefficient d'asymétrie",
       ylim=range(y, y2, y3), col="blue", type="l")
lines(x1,y2, lty=2, col = 3)
lines(x1,y3, lty=3, col = 2)

legend("topleft",leg=leg.txt, col= c("blue", "green", "red"), lty=1:3)


## Graphique du coefficient d'applatissement

x1 <- seq(-3,3,.01)
y <- dnorm(x1, 0, 0.5)
y2 <- dnorm(x1, 0, 1)
y3 <- dnorm(x1, 0, 0.3)
leg.txt <- c("Normal"," Pic","Aplati")
plot(x1, y, xlab="x", ylab="f(x)", main="Coefficient d'aplatissement",
     ylim=range(y, y2, y3), col="blue", type="l")
lines(x1,y2, lty=2, col = 3)
lines(x1,y3, lty=3, col = 2)

legend("topleft",leg=leg.txt, col= c("blue", "red", "green"), lty=1:3)

#### Chapitre 4: Statistique bivariées ####

## Graphique nuage des points

Taille <- c(150, 171, 158, 169, 160, 175, 165, 162, 171, 173, 160,
            165, 166, 169, 152, 157, 167, 166, 168, 153, 157, 167,
            180, 157, 170, 165, 155, 151, 165, 175, 156, 179, 169)
  
Poids3 <- c(55, 69, 60, 71, 63, 72, 70, 63, 61, 72, 61,
            63, 61, 68, 66, 59, 58, 64, 66, 64, 59, 63,
            75, 62, 65, 63, 58, 56, 61, 71, 60, 74, 72)

n <- 33

Etude <- data.frame(Taille, Poids3)


Nuage <- ggplot(data = Etude) + 
  geom_point(mapping = aes(x = Taille, y = Poids3)) +
  labs(
    x = "Taille",
    y = "Poids",
    title = "Nuage de points entre la taille et le poids"
  )


## Calcul du coefficient de Pearson

Produit <- Taille * Poids3


## Formule directe
R_pearson = (sum(Produit) - n *mean(Taille) * mean(Poids3))/((n-1)*sd(Taille)*sd(Poids3)) 

cor(Taille, Poids3) ## Fonction du package stat

## Graphique Correlation de Pearson

library(corrplot)

correlation2 <- cor(Etude)

corr2 <- corrplot(correlation2, method = "number")
corrplot(corr2,add=T,type="lower", method="number",
         col="black", diag=FALSE,tl.pos="n", cl.pos="n")

## Coefficient de spearman

Taille2 <- c(150, 171, 158, 169, 160, 175, 165, 162)
Poids4 <- c(55, 69, 60, 71, 63, 72, 70, 63)

cor(Taille2, Poids4, method = "spearman") ## Fonction du package stat

t_i <- rank(Taille2)
s_i <- rank(Poids4)

n<-8
produit1 <- t_i * s_i

## Formule directe
R_spearman = (sum(produit1) - n *mean(t_i) * mean(s_i))/((n-1)*sd(t_i)*sd(s_i)) 

## Graphique PIB de la R.D.Congo

df <- read_xlsx("PIB_RDC.xlsx") ## Lecture du jeu des données PIB (Source : Banque mondiale)

View(df) ## Visualisation

names(df)[names(df) == 'PIB par habitant ($ US courants)'] <- 'PIB_habitant'

Base <- ts(df$PIB_habitant, start = 1960 , frequency = 1) ## Lecture en time serie

#### Tracé du graphique ####

autoplot(Base, main="PIB par habitant de la R.D.Congo ($ US courants) ", 
         xlab="Année", ylab = "PIB_habitant", col = "blue")


### Vaccins selon le sexe ##

Vaccin <- c(rep("Astra_Zeneca", times = 69), rep("Moderna", times = 278),
            rep("Pfizer", times = 355),
            rep("Astra_Zeneca", times = 46), rep("Moderna", times = 232),
            rep("Pfizer", times = 420))
Sexe <- c(rep("Femme",times=702),rep("Homme",times=698))

Vaccin <- factor(Vaccin,levels=c("Astra_Zeneca","Moderna","Pfizer"))
Sexe <- factor(Sexe,levels=c("Femme","Homme"))
Tab_vac <- table(Sexe,Vaccin)

Freq_vac <- data.frame(Vaccin, Sexe)

## Graphique avec le diagramme en bâton empilés

Freq_diagram1 <- ggplot(data = Freq_vac) + 
  geom_bar(mapping = aes(x = Vaccin, fill = Sexe)) +  # aesthetic fill ajoutée
  labs(
    x = "Vaccin",
    y = "fréquences",
    fill = "Sexe",
    title = "Nombre de vaccinées par vaccin selon le sexe."
  ) 


## Graphique avec le diagramme en bâton groupés

Freq_diagram2 <- ggplot(data = Freq_vac) + 
  geom_bar(mapping = aes(x = Vaccin, fill = Sexe),
           position =  "dodge") +  # aesthetic fill ajoutée
  labs(
    x = "Vaccin",
    y = "fréquences",
    fill = "Sexe",
    title = "Nombre de vaccinées par vaccin selon le sexe."
  ) 


## Graphique variable qualit vs quantit

Taille <- c(170, 191, 188, 179, 170, 175, 195, 182,
            171, 187, 190, 176, 196, 180, 175, 184)

Sexe <- c("F", "M", "F", "F", "M", "M", "F", "M",
          "M", "M", "F", "F", "M", "F", "M", "M")

Sexe <- factor(Sexe, levels = c("F", "M"))

df1 <- data.frame(Taille, Sexe)


ggplot(data = df1) + 
  geom_boxplot(mapping = aes(x = Sexe, y = Taille, 
                             fill = Sexe)) +  # aesthetic fill ajoutée
  labs(
    x = "Sexe",
    y = "Taille des enfants",
    fill = "Sexe",
    title = "La taille des enfants en fonction du sexe "
  ) 


### Statistique de la taille en fonction du sexe

library(psych)

df2_Femme <- df1[df1$Sexe == "F",] ## Taille selon le sexe F

df2_Homme <- df1[df1$Sexe == "M",]  ## Taille selon le sexe M

describe(df2_Femme$Taille) ## Statistiques descriptives de la taille selon le sexe F

describe(df2_Homme$Taille) ## Statistiques descriptives de la taille selon le sexe 


#### Chapitre 5: Régression linéaire simple ####

### Droite de regression

## Création du heu de données Taille vs poids

df3 <- data.frame(Taille2, Poids4)


# Ajouter la droite de regression dans le diagramme de dispersion

ggplot(Etude, aes(x=Taille, y=Poids3)) + 
  geom_point()+
  geom_smooth(method=lm, se = FALSE)+
  #geom_jitter(colour = "blue", alpha = 0.3) + ## Ajout du bruit blanc
  labs(x ="Taille",
       y = "Poids",
       title = "Droite de régression entre la taille et le poids")



### Calcul de \beta_{0} et \bet_{1}

produit2 <- Taille * Poids3
n <- 33
ecart <- (Taille - mean(Taille)) **2

beta_1 <- (sum(produit2) - n* mean(Poids3) * mean(Taille))/sum(ecart)

beta_0 <- mean(Poids3) - beta_1*mean(Taille) 


lm(Poids3 ~ Taille, Etude) ## Fonction du package stats


