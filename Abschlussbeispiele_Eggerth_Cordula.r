# AUTOR: Cordula Thekla Eggerth
# MATRIKELNR.: 00750881

# ENDPROJEKT:

# Sicherstellen, dass keine alten Objekte im File sind:
rm(list=ls())

# ------------------------------------------------------------------------------------------------------------

# TEIL 1 - DATEN EINLESEN:

# Lese alle Dateien in R ein. Erstelle dabei ein Objekt, das alle Daten enthält. (3 P)

# - Kopiere dazu zuerst die 18 Datendateien in einen eigenen Ordner. 

    # siehe ordner "daten"

# - Ermittle daraufhin automatisiert die Dateinamen mit Hilfe der Funktion list.files().

    setwd("C:/Users/cordu/Desktop/endprojekt/daten")

    list.files()

# - Finde ein Muster, das jeder relevante Dateiname erfüllt und stelle damit sicher, dass etwaige andere 
#   Dateien im selben Ordner nicht mit eingelesen werden.
    
    namenvector <- list.files() 
    
    # PATTERN: am anfang muss Laser_ stehen, dann mind. 1 zahl, dann _, dann mind. 1 zeichen, am ende ".txt"
    richtige_namen_muster <- grep("^Laser_[0-9]+_.+.txt", namenvector, ignore.case = TRUE, value=TRUE)
    richtige_namen_muster
    
    einlesedaten_anzahl <- length(richtige_namen_muster)

# - Extrahiere die Namen der Spieler direkt aus dem Dateinamen und alle anderen Informationen aus den Dateien selbst.
    
    # namen der spieler extrahieren aus dateinamen:
      ersetze1_ <- sub("_","",richtige_namen_muster) # erstes "_" zeichen löschen
      index_vector <- regexpr("_",ersetze1_) # indexposition des "_" zeichens
      anzahlzeichen <- nchar(ersetze1_) # anzahl der zeichen im jeweiligen dateiname
      spielernamen <- substring(ersetze1_, first=(index_vector+1), last=(anzahlzeichen-4)) # spielernamen mittels indexposition ermitteln
    
# - Alle Daten einlesen und Objekt, das alle Daten enthaelt:
    
    # AUSGANGSDATEN:
      filenames <- namenvector
      playernames <- spielernamen  
      gesamtliste <- vector(mode="list")
      
      
    # FUNCTION, damit man daten eines spielers einlesen und in einer liste sammeln kann (return: liste aller daten des jeweiligen spielers):
      getSpielerdaten_alsListe <- function(playername, filename){
        
        # rohdaten extrahieren aus file
        rohdaten <- readLines(filename, warn=FALSE)
        
        # liste für gesamtdaten des betrachteten spielers anlegen
        datenliste_spieler_i <- vector(mode="list")
        
        # TEIL DATEN DER LISTE DES BETRACHETETEN SPIELERS
        # kategorienvector anlegen, damit automatisiert elemente des ergebnisdatenvector ermittelt werden können
        kategorienvector <- c("team","rank","score","shots","accuracy","youhit","hityou","powers")
        ergebnisdatenvector <- rep("0",9)
        names(ergebnisdatenvector) <- c("player", kategorienvector)
        
        # an 1. stelle steht der playername
        ergebnisdatenvector[1] <- playername
        
        # restliche elemente extrahieren und auf ergebnisdatenvector zuweisen
        for(i in 1:length(kategorienvector)){
          
            kategoriendaten <- rohdaten[grepl(pattern=kategorienvector[i], rohdaten)]
            kategoriendaten2 <- sub("<","",kategoriendaten)
            kategoriendaten_split <- unlist(strsplit(kategoriendaten2,">"))       
            index_kategorienende <- regexpr("<",kategoriendaten_split[2])
            
            if(kategorienvector[i] == "accuracy" || kategorienvector[i] == "powers"){
                kategorieninfo <- substring(first=2, last=(as.numeric(index_kategorienende)-1), kategoriendaten_split[2])
            } 
            else {
                kategorieninfo <- substring(first=1, last=(as.numeric(index_kategorienende)-1), kategoriendaten_split[2])
            }
            
            ergebnisdatenvector[i+1] <- kategorieninfo
             
        }
        
        
        # ELEMENT DATEN der liste zuweisen
        datenliste_spieler_i$Daten <- ergebnisdatenvector
        
        
        
        # TEIL HITS DER LISTE DES BETRACHETETEN SPIELERS
        indexgrenzen_hits <- which(grepl(pattern="body", rohdaten)) # indexgrenzen von body-teil 
        rohdaten_hits <- rohdaten[(indexgrenzen_hits[1]+1):(indexgrenzen_hits[2]-1)] # relevante informationen aus body nehmen    
        rohdaten_hits <- rohdaten_hits[2:length(rohdaten_hits)] # erstes element sind nur spaltenüberschriften 
        rohdaten_hits_splitted <- unlist(strsplit(rohdaten_hits,";")) # split anhand von ";" (hier reine daten)
        
        spieler_col <- rohdaten_hits_splitted[c(TRUE,FALSE,FALSE)] # spieler aus splitted daten nehmen
        you_hit_col <- rohdaten_hits_splitted[c(FALSE,TRUE,FALSE)] # you_hit aus splitted daten nehmen
        hit_you_col <- rohdaten_hits_splitted[c(FALSE,FALSE,TRUE)] # hit_you aus splitted daten nehmen
        
        # dataframe bilden mit den 3 col vectoren
        hits_df <- data.frame(player=spieler_col, you_hit=you_hit_col, hit_you=hit_you_col)
        
        
        # hits_df auf datenliste des spielers zuweisen
        datenliste_spieler_i$Hits <- hits_df
        
        
        # CHECK: print(datenliste_spieler_i)
        # CHECK: print(ergebnisdatenvector)
        # CHECK: print(hits_df)
        
        # return liste mit elementen Daten und Hits für den betrachteten spieler
        datenliste_spieler_i 
 
     }
      
    
    # CHECK DER FUNCTION zB für spieler "NATCHO" - ergebnis stimmt:    
    liste_returned <- getSpielerdaten_alsListe(playernames[1],filenames[1])
 
    
    # schleife über alle spieler, die die function für jeden spieler aufruft und den jeweiligen eintrag an die liste dranhängt:
    for(i in 1:einlesedaten_anzahl){
      
      liste_spieler_i <- getSpielerdaten_alsListe(playernames[i],filenames[i]) # alle daten von spieler i bekommen, RETURN: list
      
      gesamtliste <- c(gesamtliste, liste_spieler_i) # datenliste von spieler i an die gesamtliste dranhängen
      
    }
    
    
    # gesamtliste anzeigen
    gesamtliste  

 
  # Falls Teil 1 nicht gelingt, kannst du ab sofort das Objekt liste aus Laser.RData verwenden.
      
  load("C:/Users/cordu/Desktop/endprojekt/Laser.RDATA")
  liste



# ------------------------------------------------------------------------------------------------------------

# TEIL 2 - DATEN MANIPULIEREN: (4 P)

# 2.a) Erstelle ein Dataframe, das für alle Spieler die folgenden Informationen verwaltet:
#    rank, player, team, shots, accuracy, powers, score
#    Wähle sinnvolle Typen/Klassen für jede Variable. Sortiere dann die Zeilen des Dataframes
#    nach dem Spielernamen. (1.5 P)
  
     # anzahl rows:
     listelements <- length(liste)
  
  
     # variablen initialisieren (passende datentypen wählen):
     rank <- rep(0,listelements)
     player <- rep("0",listelements)
     team <- rep("0",listelements)
     shots <- rep(0,listelements)
     accuracy <- rep("0",listelements)
     powers <- rep(0,listelements)
     score <- rep(0,listelements)
     youhit <- rep(0,listelements)
     hityou <- rep(0,listelements)
  
     
     # setze infos in den jeweiligen vektoren für alle elemente aus der liste:
     for(i in 1:listelements){
       
       # rank:
       rank[i] <- as.numeric(liste[[i]]$Daten["rank"])

       # player:
       player[i] <- liste[[i]]$Daten["player"]
 
       # team: 
       team[i] <- liste[[i]]$Daten["team"]
       
       # shots:
       shots[i] <- as.numeric(liste[[i]]$Daten["shots"])
       
       # accuracy:
       accuracy[i] <- as.numeric(liste[[i]]$Daten["accuracy"])
       
       # powers:
       powers[i] <- as.numeric(liste[[i]]$Daten["powers"])
       
       # score:
       score[i] <- as.numeric(liste[[i]]$Daten["score"])

       # youhit:
       youhit[i] <- as.numeric(liste[[i]]$Daten["youhit"])
       
       # hityou:
       hityou[i] <- as.numeric(liste[[i]]$Daten["hityou"])
       
     }
     
     # gebildete spaltenvektoren in das dataframe aufnehmen:
     infos_df <- data.frame(rank=rank, player=player, team=team, score=score, shots=shots, accuracy=accuracy,
                            youhit=youhit, hityou=hityou, powers=powers)
     rownames(infos_df) <- infos_df$player
     
     # sortiere dataframe nach den spielernamen (player):
     sorted_infos_df <- infos_df[order(player), ]
     
# Falls 2a) nicht gelingt, kannst du ab sofort das Objekt info aus Laser.RData verwenden.
     info
     
     
     

# 2.b) Erstelle die beiden Matrizen you_hit bzw. hit_you, die angeben, wie oft der Zeilenspieler
#    jeden Spaltenspieler getroffen hat bzw. von jedem Spaltenspieler getroffen wurde. Beschrifte
#    dabei die Zeilen und Spalten der Matrizen mit den Spielernamen.
#    you_hit[i, j] enthält also die Information, wie oft Spieler i den Spieler j getroffen hat.
#    hit_you[i, j] enthält die Information, wie oft Spieler i von Spieler j getroffen wurde.

#    Überprüfe stichprobenartig die Korrektheit der Matrizen, zB gilt für LVLDH:
#    LVLDH hat DANGERDAVE 5 Mal getroffen.
#    > you_hit["LVLDH", "DANGERDAVE"]
#    [1] 5
#    LVLDH wurde von DANGERDAVE 9 Mal getroffen.
#    > hit_you["LVLDH", "DANGERDAVE"]
#    [1] 9 
#    (1.5 P)
     
     # ausgangspunkt:
     basis <- liste
     
     
     # YOU_HIT_MATRIX
     
     # you_hit_matrix wird nach rows eingeordnet:
     you_hit_spieler_1 <- as.numeric(liste[[1]]$Hits$you_hit)
     you_hit_matrix <- matrix(you_hit_spieler_1, nrow=1)
     rownames(you_hit_matrix)[1] <- spielernamen[1]
     
     # infos aus der liste einfüllen in vector:
     for(i in 2:listelements){
       
       # you_hit daten einfüllen:
       you_hit_spieler_i <- as.numeric(liste[[i]]$Hits$you_hit)
       you_hit_matrix <- rbind(you_hit_matrix, you_hit_spieler_i)
       rownames(you_hit_matrix)[i] <- liste[[i]]$Daten[1]
       
     } 
     
     # colnames setzen:
     colnames(you_hit_matrix) <- (liste[[1]]$Hits$player)     
     
     # nach spielernamen (alphabetisch) sortieren in rows und cols:
     sorted_you_hit_matrix_nachRownames <- you_hit_matrix[ order(rownames(you_hit_matrix)), ]
     sorted_you_hit_matrix_nachRowUndColnames <- sorted_you_hit_matrix_nachRownames[ , order(colnames(sorted_you_hit_matrix_nachRownames))]
     
     # meine ergebnisse stimmen mit den vorgegebenen tests überein:
     sorted_you_hit_matrix_nachRowUndColnames["LVLDH", "DANGERDAVE"] # ergibt 5
     you_hit["LVLDH", "DANGERDAVE"]
     
     
     
     # HIT_YOU_MATRIX:
     
     # hit_you_matrix wird nach rows eingeordnet:
     hit_you_spieler_1 <- as.numeric(liste[[1]]$Hits$hit_you)
     hit_you_matrix <- matrix(hit_you_spieler_1, nrow=1)
     rownames(hit_you_matrix)[1] <- spielernamen[1]

     
       # infos aus der liste einfüllen in vector:
       for(i in 2:listelements){
          
         # hit_you daten einfüllen:
         hit_you_spieler_i <- as.numeric(liste[[i]]$Hits$hit_you)
         hit_you_matrix <- rbind(hit_you_matrix, hit_you_spieler_i)
         rownames(hit_you_matrix)[i] <- liste[[i]]$Daten[1]
         
       } 
     
       # colnames setzen:
       colnames(hit_you_matrix) <- (liste[[1]]$Hits$player)     
     
         
       # nach spielernamen (alphabetisch) sortieren in rows und cols:
       sorted_hit_you_matrix_nachRownames <- hit_you_matrix[order(rownames(hit_you_matrix)), ]
       sorted_hit_you_matrix_nachRowUndColnames <- sorted_hit_you_matrix_nachRownames[ , order(colnames(sorted_hit_you_matrix_nachRownames))]
       
       
       # meine ergebnisse stimmen mit den vorgegebenen tests überein:
       
       sorted_hit_you_matrix_nachRowUndColnames["LVLDH", "DANGERDAVE"] # ergibt 9
       hit_you["LVLDH", "DANGERDAVE"]
       

#    Falls 2b) nicht gelingt, kannst du ab sofort die Objekte you_hit und hit_you aus Laser.RData verwenden.
      liste
      you_hit
      hit_you
     
      
      

# 2.c) Kontrolliere für jeden Spieler, ob die Gesamtanzahl seiner erzielten Treffer mit der Summe
#    seiner erzielten Treffer der Matrix you_hit aus b) übereinstimmt. Kontrolliere selbiges auch
#    für die Anzahl der eingesteckten Treffer (hit_you). Kontrolliere darüber hinaus, ob in
#    Summe genauso viele Treffer erzielt wie eingesteckt wurden. (0.5 P)
     
     # ERZIELTE TREFFER GESAMT FÜR JEDEN SPIELER:
      
     alle_youhit <- rep(0,listelements)
     
     for(i in 1:listelements){
       alle_youhit[i] <- as.numeric(liste[[i]]$Daten["youhit"])
     }
     
     names(alle_youhit) <- spielernamen
     ordered_youhit <- alle_youhit[order(names(alle_youhit))] # sortieren nach names alphabetisch
     
     # KONTROLLE MITTELS ERGEBNIS AUS b.:
     ergebnis_youhit_b <- sorted_you_hit_matrix_nachRowUndColnames
     summe_youhit <- rowSums(ergebnis_youhit_b, na.rm=TRUE) 
     
     ordered_youhit == summe_youhit # ja, vergleiche sind TRUE bei jedem eintrag, als ist ergebnis gleich
     
     
     
     
     # EINGESTECKTE TREFFER GESAMT FÜR JEDEN SPIELER:
     
     alle_hityou <- rep(0,listelements)
     
     for(i in 1:listelements){
       alle_hityou[i] <- as.numeric(liste[[i]]$Daten["hityou"])
     }
      
     names(alle_hityou) <- spielernamen
     ordered_hityou <- alle_hityou[order(names(alle_hityou))] # sortieren nach names alphabetisch
     
     # KONTROLLE MITTELS ERGEBNIS AUS b.:
     ergebnis_hityou_b <- sorted_hit_you_matrix_nachRowUndColnames
     summe_hityou <- rowSums(ergebnis_hityou_b, na.rm=TRUE) 
     
     ordered_hityou == summe_hityou # ja, vergleiche sind TRUE bei jedem eintrag, als ist ergebnis gleich
     
     
     # SUMMER ALLER TREFFER ALLER SPIELER (youhit):
     summe_aller_treffer_aller_spieler <- sum(sorted_you_hit_matrix_nachRowUndColnames, na.rm=TRUE) # 559
     
     # SUMMER ALLER EINGESTECKTEN HITS ALLER SPIELER (hityou):
     summe_aller_eingestecktenHits_aller_spieler <- sum(sorted_hit_you_matrix_nachRowUndColnames, na.rm=TRUE) # 559
     
     # VERGLEICH ALLER YOUHIT UND ALLER HITYOU:
     differenz <- summe_aller_eingestecktenHits_aller_spieler - summe_aller_treffer_aller_spieler # 0, also ist summe gesamt gleich
     

     
     
# 2.d) Erstelle die mit den Spielernamen beschriftete Matrix hit_you_rel mit folgender
#    Information: Gegeben, der Zeilenspieler wird getroffen: Mit welcher Wahrscheinlichkeit wird
#                 er dabei von einem bestimmten Spaltenspieler getroffen?

#    LVLDH wurde insgesamt 28 Mal getroffen, davon 9 Mal von DANGERDAVE. Damit gilt:
#    > 9 / 28
#    [1] 0.3214286
#    > hit_you_rel["LVLDH", "DANGERDAVE"]
#    [1] 0.3214286

     # ausgangsdaten: 
     summe_hityou_proSpieler <- summe_hityou 
     hityou_matrix_alleSpieler <- sorted_hit_you_matrix_nachRowUndColnames
     
     # matrix: 
     ergebnismatrix <- matrix(rep(0,ncol(hityou_matrix_alleSpieler)^2),ncol=ncol(hityou_matrix_alleSpieler))
     colnames(ergebnismatrix) <- colnames(hityou_matrix_alleSpieler)
     rownames(ergebnismatrix) <- rownames(hityou_matrix_alleSpieler)
     
     # berechne hit_you_rel als wahrscheinlichkeit pro zeilenspieler:
     for(i in 1:nrow(hityou_matrix_alleSpieler)){
       
       ergebnismatrix[i, ] <- hityou_matrix_alleSpieler[i, ] / summe_hityou_proSpieler[i]
       
     } 
     
     # ergebnis des hit_you_rel:
     ergebnismatrix
     
#    Tipp: Kontrolliere, ob die Zeilen der Matrix eine Wahrscheinlichkeitsverteilung definieren,
#          also alle Einträge der Matrix >= 0 und die Zeilensummen (annähernd) 1 betragen. (0.5 P)

     # prüfe alle einträge der matrix >= 0:
       all(ergebnismatrix >=0, na.rm=TRUE)  # ist in ordnung, weil keine werte kleiner null sind (NA werden exkludiert von berechnung)
     
     # prüfe alle zeilensummen == 1:
       all(rowSums(ergebnismatrix, na.rm=TRUE) == 1) # alle zeilensummen sind gleich 1 (NA werden exkludiert von berechnung)
     
     
# Falls 2d) nicht gelingt, kannst du ab sofort das Objekt hit_you_rel aus Laser.RData verwenden.
       hit_you_rel





# ------------------------------------------------------------------------------------------------------------

# TEIL 3 - BERECHNUNGEN DURCHFUEHREN: (3.5 P)

# Wir betrachten in diesem Teil die folgenden 5 Variablen:
# a ... Wie oft hat jemand einen Spieler des gegnerischen Teams getroffen?
# b ... Wie oft hat jemand einen Spieler des eigenen Teams getroffen?
# c ... Wie oft wurde jemand von einem Spieler des gegnerischen Teams getroffen?
# d ... Wie oft wurde jemand von einem Spieler des eigenen Teams getroffen?
# y ... score abzüglich accuracy und powers.

# LVLDH hat 49 Treffer erzielt. 48 davon haben einen Gegner getroffen und 1 einen Spieler des eigenen
# Teams (NATCHO by the way). Für LVLDH nehmen damit a bzw. b die Werte 48 bzw. 1 an. Der Wert y
# ist für LVLDH 4670 - 490 - 100 = 4080.

# 3.a) Bestimme für jeden Spieler die Variablen a, b, c, d und y. Wir bekommen am Ende also 5
#    Vektoren. Dein Code soll idealerweise die Teamnamen automatisiert bestimmen und für eine
#    beliebige Anzahl an Teams sowie beliebige Teamnamen funktionieren! (1.5 P)

    # y ermitteln (score abzüglich accuracy und powers; vector mit eintrag für jeden spieler):
       
       # leeren vector y anlegen:
       y_vector <- rep(0,listelements)
       
       # für alle listenelemente (spieler) y-berechnung machen:  
       for(i in 1:listelements){
         
          y_vector[i] <- as.numeric(liste[[i]]$Daten["score"]) - as.numeric(liste[[i]]$Daten["accuracy"]) - as.numeric(liste[[i]]$Daten["powers"])
         
       }       
        
       # y-ergebnis (HIER: in reihenfolge der liste; ordnung war in angabe nicht gegeben) (hat selbe werte wie y zur überprüfung):
       y_vector
       
       
   
       
    # a ermitteln (Wie oft hat jemand einen Spieler des gegnerischen Teams getroffen (see YOUHIT); vector mit eintrag für jeden spieler):
       
       # a vector anlegen
       a_vector <- rep(0,listelements)
       
       # vektor aller player anlegen
       players_vector <- rep("0",listelements)
       for(i in 1:listelements){
        players_vector[i] <- liste[[i]]$Daten["player"]
       }   

       # vektor aller teams anlegen
       teams_vector <- rep("0",listelements)
       for(i in 1:listelements){
         teams_vector[i] <- liste[[i]]$Daten["team"]
       }

       # für alle players a ermitteln
       for(i in 1:listelements){ # schleife über derzeit betrachteten spieler
          
         name_derzeitigerspieler <- liste[[i]]$Daten["player"]
         team_derzeitigerspieler <- liste[[i]]$Daten["team"]
         names(team_derzeitigerspieler) <- NULL
         hits_derzeitigerspieler <- liste[[i]]$Hits
         sum_youhit_gegnerteam <- 0
         
         for(j in 1:listelements){ # schleife über you_hit-werte
             hits_name_abgeschossenerSpieler <- hits_derzeitigerspieler$player[j]
             index_abgeschossenerSpieler <- which(players_vector==hits_name_abgeschossenerSpieler)
             team_abgeschossenerSpieler <- teams_vector[index_abgeschossenerSpieler]
             names(team_abgeschossenerSpieler) <- NULL
             
             if(team_derzeitigerspieler != team_abgeschossenerSpieler) {
               sum_youhit_gegnerteam <- sum(sum_youhit_gegnerteam, hits_derzeitigerspieler$you_hit[j], na.rm=TRUE)
             }
            
         }
         
         a_vector[i] <- sum_youhit_gegnerteam
         
       }
       
       # ergebnis a (HIER: in reihenfolge der liste) (hat selbe werte wie a zur überprüfung)
         a_vector
         
         
         
         

    # b ermitteln (Wie oft hat jemand einen Spieler des eigenen Teams getroffen (see YOUHIT); vector mit eintrag für jeden spieler):
       
         # b vector anlegen
         b_vector <- rep(0,listelements)
         
         # für alle players b ermitteln
         for(i in 1:listelements){ # schleife über derzeit betrachteten spieler
           
           name_derzeitigerspieler <- liste[[i]]$Daten["player"]
           team_derzeitigerspieler <- liste[[i]]$Daten["team"]
           names(team_derzeitigerspieler) <- NULL
           hits_derzeitigerspieler <- liste[[i]]$Hits
           sum_youhit_eigenesteam <- 0
           
           for(j in 1:listelements){ # schleife über you_hit-werte
             hits_name_abgeschossenerSpieler <- hits_derzeitigerspieler$player[j]
             index_abgeschossenerSpieler <- which(players_vector==hits_name_abgeschossenerSpieler)
             team_abgeschossenerSpieler <- teams_vector[index_abgeschossenerSpieler]
             names(team_abgeschossenerSpieler) <- NULL
             
             if(team_derzeitigerspieler == team_abgeschossenerSpieler) {
               sum_youhit_eigenesteam <- sum(sum_youhit_eigenesteam, hits_derzeitigerspieler$you_hit[j], na.rm=TRUE)
             }
             
           }
           
           b_vector[i] <- sum_youhit_eigenesteam
           
         }
         
         # ergebnis b (HIER: in reihenfolge der liste) (hat selbe werte wie b zur überprüfung)
         b_vector
          
        

       
    # c ermitteln (Wie oft wurde jemand von einem Spieler des gegnerischen Teams getroffen (see HITYOU); vector mit eintrag für jeden spieler):

         # c vector anlegen
         c_vector <- rep(0,listelements)
         
         # für alle players c ermitteln
         for(i in 1:listelements){ # schleife über derzeit betrachteten spieler
           
           name_derzeitigerspieler <- liste[[i]]$Daten["player"]
           team_derzeitigerspieler <- liste[[i]]$Daten["team"]
           names(team_derzeitigerspieler) <- NULL
           hits_derzeitigerspieler <- liste[[i]]$Hits
           sum_hityou_gegnerteam <- 0
           
           for(j in 1:listelements){ # schleife über you_hit-werte
             hits_name_schiessenderSpieler <- hits_derzeitigerspieler$player[j]
             index_schiessenderSpieler <- which(players_vector==hits_name_schiessenderSpieler)
             team_schiessenderSpieler <- teams_vector[index_schiessenderSpieler]
             names(team_schiessenderSpieler) <- NULL
             
             if(team_derzeitigerspieler != team_schiessenderSpieler) {
               sum_hityou_gegnerteam <- sum(sum_hityou_gegnerteam, hits_derzeitigerspieler$hit_you[j], na.rm=TRUE)
             }
             
           }
           
           c_vector[i] <- sum_hityou_gegnerteam
           
         }
         
         # ergebnis c (HIER: in reihenfolge der liste) (hat selbe werte wie c zur überprüfung)
         c_vector
         
         
   
       
    # d ermitteln (Wie oft wurde jemand von einem Spieler des eigenen Teams getroffen (see HITYOU); vector mit eintrag für jeden spieler):
       
         # d vector anlegen
         d_vector <- rep(0,listelements)
         
         # für alle players d ermitteln
         for(i in 1:listelements){ # schleife über derzeit betrachteten spieler
           
           name_derzeitigerspieler <- liste[[i]]$Daten["player"]
           team_derzeitigerspieler <- liste[[i]]$Daten["team"]
           names(team_derzeitigerspieler) <- NULL
           hits_derzeitigerspieler <- liste[[i]]$Hits
           sum_hityou_eigenesteam <- 0
           
           for(j in 1:listelements){ # schleife über you_hit-werte
             hits_name_schiessenderSpieler <- hits_derzeitigerspieler$player[j]
             index_schiessenderSpieler <- which(players_vector==hits_name_schiessenderSpieler)
             team_schiessenderSpieler <- teams_vector[index_schiessenderSpieler]
             names(team_schiessenderSpieler) <- NULL
             
             if(team_derzeitigerspieler == team_schiessenderSpieler) {
               sum_hityou_eigenesteam <- sum(sum_hityou_eigenesteam, hits_derzeitigerspieler$hit_you[j], na.rm=TRUE)
             }
             
           }
           
           d_vector[i] <- sum_hityou_eigenesteam
           
         }
         
         # ergebnis d (HIER: in reihenfolge der liste) (hat selbe werte wie d zur überprüfung)
         d_vector
  
       
       
         
# Falls 3.a) nicht gelingt, kannst du ab sofort die Objekte a, b, c, d und y aus Laser.RData verwenden.

# 3.b) Die Gesamtpunktezahl berechnet sich nach folgendem linearen Modell:
#    y = beta1*a + beta2*b + beta3*c + beta4*d	
#    Bestimme die Koeffizienten beta1, beta2, beta3 und beta4 des Modells auf zwei Arten:
#    . mit Hilfe eines Gleichungssystems
#      Baue die erforderlichen Objekte des Gleichungssystems automatisiert zusammen.
#    . mittels Regression auf Basis von lm()
#      Auf Moodle findest du im Ordner "Ergänzungen zum Skriptum" die Datei Hypothesentests_LineareModelle.R. 
#      Lese dir davor den Abschnitt über "Regression: Lineare Modelle" durch und stelle bei Bedarf weitere Recherchen an.
#      Rechne nach, ob deine Koeffizienten tatsächlich richtig sind. (2 P)
  

    # BASISDATEN:
      a_vec <- a
      b_vec <- b
      c_vec <- c
      d_vec <- d
         
    # MIT GLEICHUNGSSYSTEM:
      # beta: unbekannte
      # a,b,c,d: bekannte  
         
      ABCD_matrix <- cbind(a,b,c,d)[1:4, ] # muss abgeschnitten werden, weil solve befehl sonst nicht lösen kann und muss quadratisch sein   
      Y <- y[1:4]
       
      coeff <- solve(ABCD_matrix,Y) # gleichungssystem lösen
      
      # koeffizienten:
      # a=100 ... wird positiv gewertet, wenn spieler den gegner trifft 
      # b=-20 ... wird negativ gewertet, wenn spieler das eigene team trifft  
      # c=-25 ... wird negativ gewertet, wenn spieler vom gegner getroffen wird
      # d=0   ... wenn jemand vom eigenen teammitglied getroffen wird 
         
         
    # MITTELS REGRESSION UND LINEAREM MODELL:     
    
      # lineares modell (durch formula object beschreiben; intercept weggeben)
        linmod <- lm(formula = y ~ -1+a+b+c+d) 
      
      # struktur des linearen modells ist eine liste mit vielen elementen (wichtig sind hier die coefficients)
        str(linmod)
      
      # coefficients herausnehmen
        coefficients_linmod <- linmod$coefficients
        coefficients_linmod <- round(coefficients_linmod)
      
      
    # NACHRECHNEN, ob koeffizienten richtig sind:
      # in gleichung y = beta1*a + beta2*b + beta3*c + beta4*d
        beta1 <- coefficients_linmod[1]
        beta2 <- coefficients_linmod[2]
        beta3 <- coefficients_linmod[3]
        beta4 <- coefficients_linmod[4]
        
        y_check <- beta1*a + beta2*b + beta3*c + beta4*d

      # überprüfung auf gleichheit (ja, koeffizienten sind korrekt)   
        y == y_check
      
      

      
# ------------------------------------------------------------------------------------------------------------

# TEIL 4 - DATEN VISUALISIEREN: (3.5 P)  
  
# Wir wollen die Teams hinsichtlich folgender numerischer Variablen aus dem Dataframe aus 2a)
# miteinander mit Hilfe von Balkendiagrammen vergleichen:
#   . score
#   . shots
#   . accuracy
#   . youhit
#   . hityou

# Deine Balkendiagramme sollen den Sachverhalt korrekt und optisch ansprechend darstellen und
# derart beschriftet sein, dass man auf den ersten Blick erkennt, worum es in der Grafik geht.

# 4.a) Erstelle Balkendiagramme für jede der obigen Variablen. Kategorisiere dazu die Variablen
#    automatisiert auf geeignete Art und Weise. Auf der x-Achse sind die Teams abgebildet,
#    entlang der y-Achse sehen wir, wie viele Spieler des jeweiligen Teams in die ermittelten
#    Kategorien fallen. 
#    Definiere eine geeignete Farbpalette, die den ordinalskalierten Charakter der kategorisierten 
#    Variablen widerspiegelt.
#    Hinweis: Beachte den Hinweis Nummer 7 auf Seite 3. Welche Funktion(en) könnten dir dabei
#             helfen, jede der Variablen automatisiert zu kategorisieren? (tapply, ...) (2.5 P)
# 4.b) Wie a), nur soll auch eine Legende eingezeichnet werden. Achte darauf, dass die Legende
#    automatisiert erstellt wird und (idealerweise automatisiert) so angeordnet wird, dass kein
#    Balken überdeckt wird.
#    Hinweis: Das Skriptum und Quiz 5 könnten coole Inspirationsquellen sein ;-). Natürlich kann
#             man auch am Zeichenbereich der beiden Achse schrauben ;-) (1 P)

        
     # ANMERKUNG: 4.a. (barplot) und 4.b. (legend) werden von der function getPlot gleich in einem gemacht     
     # BASISDATEN:
     variablen <- c("score", "shots", "accuracy", "youhit", "hityou") # variablen, für die ein plot erstellt werden kann
     datalist <- liste # liste der gesamten spielerdaten
 
     # FUNCTION ZUR ERSTELLUNG DES PLOTS:     
     getPlot <- function(variable){

         # ergebnisvektoren initialisieren:
         teaminfos <- rep("0",listelements)  
         variableinfos <- rep(0,listelements)
       
         # schleife über die listenelemente
          for(i in 1:listelements){  
             
             teaminfos[i] <- liste[[i]]$Daten["team"]

             variableinfos[i] <- as.numeric(liste[[i]]$Daten[variable])
             
          }
 
         # CHECK: print(teaminfos)
         # CHECK: print(variableinfos)
           
         # gruppen einteilen mit cut
           leistung <- cut(variableinfos, breaks=quantile(variableinfos),  # defaultmäßig bei quantile 5 gruppen
                                  labels= c("wenig", "mittel", "viel", "sehr viel"),
                                  include.lowest=TRUE,
                                  ordered_result=TRUE) # ordinal-skalierter factor
                                   
           # kreuztabelle für barplot
           kreuztab <- table(leistung, teaminfos)
               # CHECK: print(kreuztab)
           
           # eigene farbpalette erstellen
           farbpalette <- colorRampPalette(c("red", "yellow", "darkgreen"))
           
           # barplot
           x11() # neues grafikfenster aufmachen
           barplot(kreuztab, col=farbpalette(nlevels(leistung)),
                   main=variable, xlab="teams", ylab="spieleranzahl",
                   beside=TRUE, ylim=c(0,6))
          
           # legende  
           legend("topright", legend = paste(levels(leistung)),
                  pch = 16, col = farbpalette(nlevels(leistung)), text.col = farbpalette(nlevels(leistung)), bty = "n")
           
          # RETURN: liste der teaminfos und variableinfos
            list(teaminfos=teaminfos, variableinfos=kreuztab)
           
     }
     
        
     # TEST function:
       ergebnis_plotdaten <- getPlot("score")
       
     # GETPLOT-FUNCTION AUFRUFEN FÜR ALLE VARIABLEN:
       for(i in 1:length(variablen)){
         ergebnis_plotdaten <- vector(mode="list")
         
         ergebnis_plotdaten[[i]] <- getPlot(variablen[i])
         
       }      


       
       

       
# ------------------------------------------------------------------------------------------------------------

# TEIL 5 - DATEN NACHSIMULIEREN: (4 P)

# Wir wollen das Spiel nachsimulieren. Unser grober Simulationsplan für die Simulation eines Spielers:
#   1. Simuliere die Gesamtanzahl der eingesteckten Treffer.
#   2. Gegeben die Anzahl an eingesteckten Treffern: Berechne, von wem der Spieler wie oft
#      getroffen wurde. (hityou)

# Diese Schritte wiederholen wir sodann für jeden Spieler. Die Anzahl der Treffer, die ein Spieler
# gelandet hat, können wir aus den eingesteckten Treffern ableiten: Werden die erlittenen Treffer von
# Spieler i simuliert, so bestimmen wir zB gleichzeitig die Treffer all jener, die Spieler i getroffen haben.

# SCHRITT 1:
# Die betrachtete Spielrunde hat 15 Minuten gedauert. Sei 
# Ni(t) die Zufallsvariable, welche die Anzahl der Treffer modelliert, die Spieler i in t Minuten einstecken muss. 
# Wir nehmen an, dass N_i(t)~Poisson(theta_i) gilt, also N_i(t) poissonverteilt ist. 
# Dabei gibt theta_i(t) an, wie viele Treffer Spieler i in t Zeiteinheiten im Mittel einstecken muss. 
# Diesen Wert können wir leicht aus den Daten schätzen:
   
#     theta_schaetzer_i(t) = t*n_i/15

# Wobei n_i die Anzahl der eingesteckten Treffer von Spieler i in der betrachteten Spielrunde ist. 
# Für LVLDH gilt zB: n_i = 28 (siehe liste$LVLDH$Daten["hityou"]). Wenn wir 60 Minuten simulieren wollen, so gilt:
#     theta_schaetzer_LVLDH(60)=60*28/15=112
# Wir erwarten also, dass LVLDH in 60 Minuten im Mittel 112 Mal getroffen wird.

# SCHRITT 2:
# Mit der Matrix hit_you_rel aus 2d) können wir Schritt 2 relativ leicht ausführen.
# a) Schreibe eine Funktion, welche ein Spiel simuliert. Die Zeitdauer des Spiels (in Minuten), das
#    Dataframe aus 2a) sowie die Matrix aus 2d) dienen als Inputparameter.
       
#    Output: Eine beschriftete Liste mit folgenden drei Komponenten:
#           . info: Dataframe mit folgenden Variablen: player, team, a, b, c, d 
#                   Die Variablen a, b, c und d haben dieselbe Semantik, wie zu Beginn von Teil 3 erläutert. 
#                   Versuche möglichst vektorwertig zu programmieren.
#           . hit_you: Matrix analog zu 2b)
#           . you_hit: Matrix analog zu 2b) (2.5 P)
       

       
        # ACHTUNG: ANGABE ZU DIESEM BEISPIEL IST NICHT KLAR BZGL SCHÄTZER-BERECHNUNG FÜR YOUHIT >> DAHER ANNAHME DES UNTENSTEHENDEN RECHENWEGS  
        # BASISDATEN:
        allespieler_daten <- liste
        hit_you_rel_2 <- hit_you_rel
        info_2 <- info
        
        info_2$score <- NULL 
        info_2$shots <- NULL
        info_2$accuracy <- NULL
        info_2$youhit <- NULL
        info_2$hityou <- NULL
        info_2$powers <- NULL
       
      # FUNCTION FÜR SIMULATION:
      simulation_spiel <- function(t=60, info=info_2, hit_you_rel=hit_you_rel_2){
          
          # a,b,c,d an info-dataframe dranhängen 
           info_2$a <- a
           info_2$b <- b
           info_2$c <- c
           info_2$d <- d
    

          # HITYOU: THETA-SCHÄTZER (gesamtanzahl eingesteckter treffer) für alle spieler berechnen
            theta_schaetzer <- rep(0,listelements)
            
            for(i in 1:listelements){
              # formel: theta_schaetzer = t * n / 15
              theta_schaetzer[i] <- t * as.numeric(allespieler_daten[[i]]$Daten["hityou"]) / 15       
            }
            names(theta_schaetzer) <- spielernamen
            theta_schaetzer <- theta_schaetzer[order(names(theta_schaetzer))] # theta_schaetzer-einträge alphabetisch ordnen
          
            
                # poissonverteilte zufallszahlen ziehen mit Hilfe von theta-schätzer
                  erwartete_hityou_alle <- rep(0,listelements)
                  
                  for(i in 1:listelements){
                    # n = anzahl der gezogenen zufallszahlen; lambda = parameter der poissonverteilung 
                    erwartete_hityou_alle[i] <- rpois(n=1, lambda=theta_schaetzer[i]) 
                  }
                  names(erwartete_hityou_alle) <- sort(spielernamen)
 

          # YOUHIT: THETA-SCHÄTZER (gesamtanzahl erzielter treffer) für alle spieler berechnen (ANNAHME: berechnungsformel gilt auch für youhit)
                  theta_schaetzer_2 <- rep(0,listelements)
                  
                  for(i in 1:listelements){
                    # formel: theta_schaetzer = t * n / 15
                    theta_schaetzer_2[i] <- t * as.numeric(allespieler_daten[[i]]$Daten["youhit"]) / 15       
                  }
                  names(theta_schaetzer_2) <- spielernamen
                  theta_schaetzer_2 <- theta_schaetzer_2[order(names(theta_schaetzer_2))] # theta_schaetzer_2-einträge alphabetisch ordnen
                  
                  
                  # poissonverteilte zufallszahlen ziehen mit Hilfe von theta-schätzer-2
                  erwartete_youhit_alle <- rep(0,listelements)
                  
                  for(i in 1:listelements){
                    # n = anzahl der gezogenen zufallszahlen; lambda = parameter der poissonverteilung 
                    erwartete_youhit_alle[i] <- rpois(n=1, lambda=theta_schaetzer_2[i]) 
                  }
                  names(erwartete_youhit_alle) <- sort(spielernamen)                                              

                              
          # HITYOU und YOUHIT MATRIX SCHÄTZER mit Hilfe von hit_you_rel berechnen
            # d.h. poissonverteilte zufallszahlen (erwartete_hityou_alle) mit wahrscheinlichkeiten aus hit_you_rel gewichten
            
            # HITYOU-SCHÄTZER:
              hit_you_simuliert <- matrix(rep(0,listelements*listelements),ncol=18) 
              colnames(hit_you_simuliert) <- names(erwartete_hityou_alle)
              rownames(hit_you_simuliert) <- names(erwartete_hityou_alle)
              
              for(i in 1:listelements){
                hit_you_simuliert[ ,i] <- hit_you_rel_2[ ,i]*erwartete_hityou_alle
              }
               
            # YOUHIT-SCHÄTZER:
              # you_hit_rel berechnen
              summe_youhit <- rowSums(you_hit, na.rm=TRUE) 

              # matrix anlegen
              you_hit_rel <- matrix(rep(0,listelements*listelements),ncol=listelements)
              colnames(you_hit_rel) <- names(erwartete_hityou_alle)
              rownames(you_hit_rel) <- names(erwartete_hityou_alle)
              
              # berechne hit_you_rel als wahrscheinlichkeit pro zeilenspieler
              for(i in 1:listelements){
                
                you_hit_rel[i, ] <- you_hit[i, ] / summe_youhit[i]
                
              }               
              
              # simulierte you_hit matrix berechnen
              you_hit_simuliert <- matrix(rep(0,listelements*listelements),ncol=18) 
              colnames(you_hit_simuliert) <- names(erwartete_youhit_alle)
              rownames(you_hit_simuliert) <- names(erwartete_youhit_alle)
              
              for(i in 1:listelements){
                you_hit_simuliert[ ,i] <- you_hit_rel[ ,i]*erwartete_youhit_alle       
              }
              
             
          # RETURN: liste mit info, hit_you, you_hit
              list(info=info_2, hit_you=hit_you_simuliert, you_hit=you_hit_simuliert)
              
      } # END FUNCTION
        
      
      
      # TEST AUFRUF DER FUNCTION simulation_spiel:
        simulationsergebnis <- simulation_spiel(t=60, info=info_2, hit_you_rel=hit_you_rel_2)  

  
           
    
# b) Simuliere mit Hilfe von a) 24 Stunden (die sogenannten 24 Stunden von Lasertag ;-)) (0 P)
    
# Falls 5ab) nicht gelingen, kannst du ab sofort das Objekt daten aus Laser.RData verwenden.

      # FUNCTION simulation_spiel mit parameter t=24*60 (in min) AUFRUFEN:
        simulationsergebnis_24h <- simulation_spiel(t=24*60, info=info_2, hit_you_rel=hit_you_rel_2)
  
        
    

# c) Hänge an das Dataframe info in der Liste aus 5ab) folgende beiden Informationen an:
#   . Shots: Finde eine Möglichkeit, die Anzahl der Schüsse zu schätzen, die jeder Spieler
#            abgegeben hat. Erkläre, wie du auf diese Schätzung gekommen bist.
#     Hinweis: Die Anzahl der Treffer kennen wir. Wie viele Schüsse könnte jeder Spieler
#              gebraucht haben, um diese Anzahl an Treffern zu erreichen?
        
#   . Score: Berechne für jeden Spieler die Anzahl der erreichten Punkte. Der Einfachheit
#            halber gibt es in dieser Simulation keine Bonuspunkte für präzises Schießen oder
#            aktivierte Power-Ups; der Score errechnet sich ausschließlich aus den erzielten und
#            eingesteckten Treffern. Verwende dazu die Koeffizienten aus 3b) 
        
#   Sollte 3b) nicht geklappt haben, so setze stattdessen eigene Zahlen für die
#   Koeffizienten ein. (1.5 P)
# Falls 5c) nicht gelingt, kannst du ab sofort das Objekt datenc aus Laser.RData verwenden.


        # BASISDATEN: 
          simulation_basis <- daten

        # FUNCTION FÜR SHOTS UND SCORE:
          add_Score_Shots <- function(simulation_basis){
            
            # SHOTS SIMULIEREN:
              # ausgangsdaten
              shots_alle <- matrix(rep(0,listelements*listelements),ncol=18) 
              colnames(shots_alle) <- sort(spielernamen)
              rownames(shots_alle) <- sort(spielernamen)
              
              # trefferfaktor bestimmen
              shotsfaktor <- 100 / (hit_you_rel*100)  
              
              # matrix mit allen schüssen berechnen
              for(i in 1:listelements){
                
                shots_alle[i, ] <- simulation_basis$hit_you[i, ] * shotsfaktor[i, ] - simulation_basis$hit_you[i, ]
    
              }
              
              shots_alle[is.nan(shots_alle)] <- 0
            
              shots_summe_alleSpieler <- round(rowSums(shots_alle, na.rm=TRUE))
              
              simulation_basis$info$shots <- shots_summe_alleSpieler
              
            # SCORE SIMULIEREN:
              # ausgangsdaten
                betas <- coeff 
                
              # score berechnen
                score <- betas[1]*a + betas[2]*b + betas[3]*c + betas[4]*d
               
              # score in info hinzufügen
                simulation_basis$info$score <- score
                
            # RETURN: info, youhit, hityou 
              simulation_basis
          }
        
            
      # TESTAUFRUF DER FUNCTION add_Score_Shots:
          simulationsergebnisse <- add_Score_Shots(simulation_basis)

          
          

          
          
# ------------------------------------------------------------------------------------------------------------

# TEIL 6 - DATEN ABSPEICHERN: (2 P)

# Speichere die simulierten Daten aus Teil 5 im selben Ordner und in exakt derselben Form ab, wie die
# Daten der gegebenen Spielrunde. Verwende lediglich für den Prefix des Dateinamens ein anderes Wort, 
# als "Laser", dh überschreibe die originalen Dateien nicht.

# Wie in Beispiel 5c) geschrieben, nehmen wir der Einfachheit halber an, dass kein Spieler Bonuspunkte
# für accuracy und powers bekommt - wir können also 0 einsetzen. (2 P)

# Teste daraufhin, ob dein Code auch für deine erzeugten Dateien reibungslos funktioniert. Idealerweise
# musst du dazu deinen R-Code an nur einer Stelle in Teil 1 abändern ;-).  >> ja, funktioniert man muss dafür nur in zeile 56
#                                                                             rohdaten <- readLines(filename, warn=FALSE)
#                                                                             den "filename" ändern


  # BASISDATEN:
    simulationen <- datenc

    daten_info <- datenc$info
    hityou_info <- datenc$hit_you
    youhit_info <- datenc$you_hit
    namensliste <- rownames(hityou_info)
    
    summe_hityou_allerSpieler <- colSums(hityou_info, na.rm=TRUE)
    summe_youhit_allerSpieler <- colSums(youhit_info, na.rm=TRUE)
    
    
   # DATEINAMEN GENERIEREN:
     dateinamenvec <- rep("0",listelements) 
    
     for(i in 1:listelements){
       dateinamenvec[i] <- paste0("SimulResults_",i,"_",namensliste[i],".txt")
     }
    
  
   # ABSPEICHERN:
     for(i in 1:listelements){
       
       # betrachteter spieler
       spieler_derzeit <- namensliste[i]
       
       # string mit tags zusammenstellen 
       string_head <- paste0("<head>\n  <team>", daten_info[i,"team"], "</team>\n  <rank>", daten_info[i,"rank"], "</rank>\n  <score>", daten_info[i,"score"], "</score>\n  <shots>", daten_info[i,"shots"], "</shots>\n  <accuracy>0</accuracy>\n  <youhit>", summe_youhit_allerSpieler[i], "</youhit>\n  <hityou>", summe_hityou_allerSpieler[i], "</hityou>\n  <powers>0</powers>\n</head>\n<body>\nplayer;you_hit;hit_you")
       string_body_ende <- paste0("</body>")                       
       string_mitte <- NULL
       
       # string_mitte mit den you_hit und hit_you ergebnisse zusammenstellen
       for(j in 1:listelements){
         string_mitte <- c(string_mitte, paste0(namensliste[j],";", ifelse(is.na(youhit_info[j,spieler_derzeit]),"-",youhit_info[j,spieler_derzeit]),";", ifelse(is.na(hityou_info[j,spieler_derzeit]),"-",hityou_info[j,spieler_derzeit])) )
       }
       
       # gesamtstring des spielers erzeugen
       gesamt_spielerstring <- c(string_head, string_mitte, string_body_ende)       
       
       # gesamtspielerdaten in passende datei schreiben
       writeLines(text = gesamt_spielerstring, dateinamenvec[i])
       
     }
    
    
     
     
     
     
     
    # TEST FÜR LVLDH
       string_head <- paste0("<head>\n  <team>", daten_info[3,"team"], "</team>\n  <rank>", daten_info[3,"rank"], "</rank>\n  <score>", daten_info[3,"score"], "</score>\n  <shots>", daten_info[3,"shots"], "</shots>\n  <accuracy>0</accuracy>\n  <youhit>", summe_youhit_allerSpieler[3], "</youhit>\n  <hityou>", summe_hityou_allerSpieler[3], "</hityou>\n  <powers>0</powers>\n</head>\n<body>\nplayer;you_hit;hit_you")
       string_body_ende <- paste0("</body>")                       
       string_mitte <- NULL
       
       for(i in 1:listelements){
         string_mitte <- c(string_mitte, paste0(namensliste[i],";", youhit_info[i,"LVLDH"],";", hityou_info[i,"LVLDH"],"\n") )
       }

       gesamt <- c(string_head, string_mitte, string_body_ende)

       writeLines(text = gesamt, "simulfile_MARIA_1.txt")






