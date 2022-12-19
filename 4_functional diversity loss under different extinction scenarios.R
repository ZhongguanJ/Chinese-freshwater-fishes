library(tidyverse)
library(rfishbase)

#### 1. Load data ----
functional_traits <- read.csv('data/functional traits.csv',header=TRUE,row.names = 1)
species_site <- read.csv('data/species site.csv',header= TRUE,row.names = 1)
trait_category <- read.csv('data/trait category.csv',header= TRUE)
ER_status <- read.csv('data/Extinct_Risk_Status.csv',header= TRUE)
Threat <- read_csv("data/Major_threats_for_red_species.csv")
regions_names <- read_csv("data/region_names.csv")


#### 2. Some data pre-processing ----
functional_traits$Diet <- factor(functional_traits$Diet)
functional_traits$Habitat <- factor(functional_traits$Habitat)
functional_traits$Body <- factor(functional_traits$Body) 
species_site <- as.matrix(species_site)[1:18,]

ER_status$Ext_Status <- gsub(" ","",ER_status$Ext_Status,fixed=T) #delete blank space

#### 2.1. Table threatened species - traits and threats ----
traits2 <- bind_cols(ER_status,functional_traits)
rownames(traits2) <- NULL
traits3 <- traits2 %>% select("Species","Sp_Code","Max","Trophic","Growth","Age","Length","Longevity","Diet","Habitat","Body")
names(Threat) <- c("N.","Order","Family","Chinese name","Species","IUCN","Criteria","Endemism","Max SL","Habitat preference","Dam construction","Habitat loss","Overfishing","Species invasion")
Threat2 <- Threat %>% select("Species","IUCN","Endemism","Dam construction","Habitat loss","Overfishing","Species invasion")
Threat_spp <- left_join(Threat2,traits3, by = c("Species"))
Threat_spp$IUCN_grad <- ifelse(Threat_spp$IUCN == "VU", 1, ifelse(Threat_spp$IUCN == "EN",2, ifelse(Threat_spp$IUCN == "CR", 3, 4)))

Threat_bin <- traits2
unique(Threat_bin$Ext_Status)
Threat_bin$IUCN_bin <- ifelse(Threat_bin$Ext_Status == "VU",1,
                              ifelse(Threat_bin$Ext_Status == "EN",1,
                                     ifelse(Threat_bin$Ext_Status == "CR",1,
                                            ifelse(Threat_bin$Ext_Status == "EX",1,
                                                   ifelse(Threat_bin$Ext_Status == "RE",1,0)))))

#### 3. Extract taxonomy ----
taxonomy <- rfishbase::fishbase
taxonomy2 <- as_tibble(taxonomy[,c("SpecCode","Genus","SubFamily","Family","Order","Class")])
Valid_Names <- rfishbase::validate_names(ER_status$Species)
SpCode <- rfishbase::ecology(ER_status$Species) %>% unique() %>% select(c("Species","SpecCode"))

TaxoFish <- tibble(Species=ER_status$Species)
TaxoFish2 <- left_join(TaxoFish,SpCode, by= "Species")
TaxoFish2$SpecCode <- as.numeric(TaxoFish2$SpecCode)
TaxoFish3 <- left_join(TaxoFish2,taxonomy2, by = "SpecCode") %>% unique()
TaxoFish4 <- select(TaxoFish3, c("Species","Genus","SubFamily","Family","Order","Class"))

naRows <- which(is.na(TaxoFish3$Genus))

for (i in 1:length(naRows)) {
  sp <- TaxoFish4[naRows[i],"Species"]
  genus <- word(sp,1)
  sp_taxo <- unique(TaxoFish4[which(TaxoFish4$Genus == genus),c("Genus","SubFamily","Family","Order","Class")])
  if(nrow(sp_taxo) > 0) {
    TaxoFish4[naRows[i],c("Genus","SubFamily","Family","Order","Class")] <- sp_taxo
  }
}

TaxoFish4[which(is.na(TaxoFish4$Genus)),]
TaxoFish4$Species[which(is.na(TaxoFish4$Genus))]

#Filling missing values manually

TaxoFish4[which(TaxoFish4$Species == "Paratanakia chii"), "Genus"] <- "Paratanakia"
TaxoFish4[which(TaxoFish4$Species == "Paratanakia chii"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Paratanakia chii"), "Family"] <- "Acheilognathidae"
TaxoFish4[which(TaxoFish4$Species == "Paratanakia chii"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Paratanakia chii"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Sinorhodeus microlepis"), "Genus"] <- "Sinorhodeus"
TaxoFish4[which(TaxoFish4$Species == "Sinorhodeus microlepis"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Sinorhodeus microlepis"), "Family"] <- "Acheilognathidae"
TaxoFish4[which(TaxoFish4$Species == "Sinorhodeus microlepis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Sinorhodeus microlepis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Spinibarbichthys yunnanensis"), "Genus"] <- "Spinibarbichthys"
TaxoFish4[which(TaxoFish4$Species == "Spinibarbichthys yunnanensis"), "SubFamily"] <- "Spinibarbinae"
TaxoFish4[which(TaxoFish4$Species == "Spinibarbichthys yunnanensis"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Spinibarbichthys yunnanensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Spinibarbichthys yunnanensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Systomus orphoides"), "Genus"] <- "Systomus"
TaxoFish4[which(TaxoFish4$Species == "Systomus orphoides"), "SubFamily"] <- "Smiliogastrinae"
TaxoFish4[which(TaxoFish4$Species == "Systomus orphoides"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Systomus orphoides"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Systomus orphoides"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Altigena discognathoides"), "Genus"] <- "Altigena"
TaxoFish4[which(TaxoFish4$Species == "Altigena discognathoides"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Altigena discognathoides"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Altigena discognathoides"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Altigena discognathoides"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Altigena lippa"), "Genus"] <- "Altigena"
TaxoFish4[which(TaxoFish4$Species == "Altigena lippa"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Altigena lippa"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Altigena lippa"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Altigena lippa"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Altigena tonkinensis"), "Genus"] <- "Altigena"
TaxoFish4[which(TaxoFish4$Species == "Altigena tonkinensis"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Altigena tonkinensis"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Altigena tonkinensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Altigena tonkinensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Altigena wui"), "Genus"] <- "Altigena"
TaxoFish4[which(TaxoFish4$Species == "Altigena wui"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Altigena wui"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Altigena wui"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Altigena wui"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Altigena yunnanensis"), "Genus"] <- "Altigena"
TaxoFish4[which(TaxoFish4$Species == "Altigena yunnanensis"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Altigena yunnanensis"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Altigena yunnanensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Altigena yunnanensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Altigena zhui"), "Genus"] <- "Altigena"
TaxoFish4[which(TaxoFish4$Species == "Altigena zhui"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Altigena zhui"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Altigena zhui"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Altigena zhui"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Ageneiogarra micropulvinus"), "Genus"] <- "Ageneiogarra"
TaxoFish4[which(TaxoFish4$Species == "Ageneiogarra micropulvinus"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Ageneiogarra micropulvinus"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Ageneiogarra micropulvinus"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Ageneiogarra micropulvinus"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Ageneiogarra imberba"), "Genus"] <- "Ageneiogarra"
TaxoFish4[which(TaxoFish4$Species == "Ageneiogarra imberba"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Ageneiogarra imberba"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Ageneiogarra imberba"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Ageneiogarra imberba"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Ceratogarra cambodgiensis"), "Genus"] <- "Ceratogarra"
TaxoFish4[which(TaxoFish4$Species == "Ceratogarra cambodgiensis"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Ceratogarra cambodgiensis"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Ceratogarra cambodgiensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Ceratogarra cambodgiensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Ceratogarra fasciacauda"), "Genus"] <- "Ceratogarra"
TaxoFish4[which(TaxoFish4$Species == "Ceratogarra fasciacauda"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Ceratogarra fasciacauda"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Ceratogarra fasciacauda"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Ceratogarra fasciacauda"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Decorus decorus"), "Genus"] <- "Decorus"
TaxoFish4[which(TaxoFish4$Species == "Decorus decorus"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Decorus decorus"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Decorus decorus"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Decorus decorus"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Decorus lemassoni"), "Genus"] <- "Decorus"
TaxoFish4[which(TaxoFish4$Species == "Decorus lemassoni"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Decorus lemassoni"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Decorus lemassoni"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Decorus lemassoni"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Decorus rendahli"), "Genus"] <- "Decorus"
TaxoFish4[which(TaxoFish4$Species == "Decorus rendahli"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Decorus rendahli"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Decorus rendahli"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Decorus rendahli"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Decorus tungting"), "Genus"] <- "Decorus"
TaxoFish4[which(TaxoFish4$Species == "Decorus tungting"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Decorus tungting"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Decorus tungting"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Decorus tungting"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Decorus xanthogenys"), "Genus"] <- "Decorus"
TaxoFish4[which(TaxoFish4$Species == "Decorus xanthogenys"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Decorus xanthogenys"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Decorus xanthogenys"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Decorus xanthogenys"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Fivepearlus yunnanensis"), "Genus"] <- "Fivepearlus"
TaxoFish4[which(TaxoFish4$Species == "Fivepearlus yunnanensis"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Fivepearlus yunnanensis"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Fivepearlus yunnanensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Fivepearlus yunnanensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Lanlabeo duanensis"), "Genus"] <- "Lanlabeo"
TaxoFish4[which(TaxoFish4$Species == "Lanlabeo duanensis"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Lanlabeo duanensis"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Lanlabeo duanensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Lanlabeo duanensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Prolixicheilus longisulcus"), "Genus"] <- "Prolixicheilus"
TaxoFish4[which(TaxoFish4$Species == "Prolixicheilus longisulcus"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Prolixicheilus longisulcus"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Prolixicheilus longisulcus"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Prolixicheilus longisulcus"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Stenorynchoacrum xijiangensis"), "Genus"] <- "Stenorynchoacrum"
TaxoFish4[which(TaxoFish4$Species == "Stenorynchoacrum xijiangensis"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Stenorynchoacrum xijiangensis"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Stenorynchoacrum xijiangensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Stenorynchoacrum xijiangensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Tariqilabeo bicornis"), "Genus"] <- "Tariqilabeo"
TaxoFish4[which(TaxoFish4$Species == "Tariqilabeo bicornis"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Tariqilabeo bicornis"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Tariqilabeo bicornis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Tariqilabeo bicornis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Tariqilabeo burmanicus"), "Genus"] <- "Tariqilabeo"
TaxoFish4[which(TaxoFish4$Species == "Tariqilabeo burmanicus"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Tariqilabeo burmanicus"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Tariqilabeo burmanicus"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Tariqilabeo burmanicus"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Vinagarra findolabium"), "Genus"] <- "Vinagarra"
TaxoFish4[which(TaxoFish4$Species == "Vinagarra findolabium"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Vinagarra findolabium"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Vinagarra findolabium"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Vinagarra findolabium"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Zuojiangia jingxiensis"), "Genus"] <- "Zuojiangia"
TaxoFish4[which(TaxoFish4$Species == "Zuojiangia jingxiensis"), "SubFamily"] <- "Labeoninae"
TaxoFish4[which(TaxoFish4$Species == "Zuojiangia jingxiensis"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Zuojiangia jingxiensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Zuojiangia jingxiensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Troglonectes shuilongensis"), "Genus"] <- "Troglonectes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes shuilongensis"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Troglonectes shuilongensis"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes shuilongensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes shuilongensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Troglonectes lingyunensis"), "Genus"] <- "Troglonectes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes lingyunensis"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Troglonectes lingyunensis"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes lingyunensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes lingyunensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Troglonectes acridorsalis"), "Genus"] <- "Troglonectes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes acridorsalis"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Troglonectes acridorsalis"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes acridorsalis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes acridorsalis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Troglonectes barbatus"), "Genus"] <- "Troglonectes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes barbatus"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Troglonectes barbatus"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes barbatus"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes barbatus"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Troglonectes daqikongensis"), "Genus"] <- "Troglonectes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes daqikongensis"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Troglonectes daqikongensis"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes daqikongensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes daqikongensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Troglonectes elongatus"), "Genus"] <- "Troglonectes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes elongatus"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Troglonectes elongatus"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes elongatus"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes elongatus"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Troglonectes furcocaudalis"), "Genus"] <- "Troglonectes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes furcocaudalis"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Troglonectes furcocaudalis"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes furcocaudalis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes furcocaudalis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Troglonectes macrolepis"), "Genus"] <- "Troglonectes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes macrolepis"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Troglonectes macrolepis"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes macrolepis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes macrolepis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Troglonectes microphthalmus"), "Genus"] <- "Troglonectes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes microphthalmus"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Troglonectes microphthalmus"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes microphthalmus"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes microphthalmus"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Troglonectes translucens"), "Genus"] <- "Troglonectes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes translucens"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Troglonectes translucens"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes translucens"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Troglonectes translucens"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Paracanthocobitis botia"), "Genus"] <- "Paracanthocobitis"
TaxoFish4[which(TaxoFish4$Species == "Paracanthocobitis botia"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Paracanthocobitis botia"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Paracanthocobitis botia"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Paracanthocobitis botia"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Petruichthys brevis"), "Genus"] <- "Petruichthys"
TaxoFish4[which(TaxoFish4$Species == "Petruichthys brevis"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Petruichthys brevis"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Petruichthys brevis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Petruichthys brevis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Eonemachilus caohaiensis"), "Genus"] <- "Eonemachilus"
TaxoFish4[which(TaxoFish4$Species == "Eonemachilus caohaiensis"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Eonemachilus caohaiensis"), "Family"] <- "Cyprinidae"
TaxoFish4[which(TaxoFish4$Species == "Eonemachilus caohaiensis"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Eonemachilus caohaiensis"), "Class"] <- "Actinopterygii"

TaxoFish4[which(TaxoFish4$Species == "Amblyceps yunnanense"), "Genus"] <- "Amblyceps"
TaxoFish4[which(TaxoFish4$Species == "Amblyceps yunnanense"), "SubFamily"] <- NA
TaxoFish4[which(TaxoFish4$Species == "Amblyceps yunnanense"), "Family"] <- "Amblycipitidae"
TaxoFish4[which(TaxoFish4$Species == "Amblyceps yunnanense"), "Order"] <- "Cypriniformes"
TaxoFish4[which(TaxoFish4$Species == "Amblyceps yunnanense"), "Class"] <- "Actinopterygii"

#Correct some families and subfamilies manually
TaxoFish4[which(TaxoFish4$Species == "Eonemachilus caohaiensis"), "Family"] <- "Nemacheilidae"
TaxoFish4[which(TaxoFish4$Species == "Discherodontus parvus"), "SubFamily"] <- "Cyprininae"

#Merge tables
TaxoFishFinal <- left_join(as_tibble(ER_status),TaxoFish4, by="Species")

#last corrections
TaxoFishFinal$SubFamily[which(TaxoFishFinal$SubFamily == "No subfamily")] <- NA
TaxoFishFinal$SubFamily[which(TaxoFishFinal$SubFamily == "ex-danioninae")] <- "Xenocyprididae"
TaxoFishFinal$SubFamily[which(TaxoFishFinal$Species == "Tanichthys albonubes")] <- "Tanichthyidae"

###---###---###
#write.csv(TaxoFishFinal,"C:/Users/ro03deda/Documents/Projects/Fish China/temp/TaxoFishFinal.csv", row.names = F)
TaxoFishFinal <- read_csv("C:/Users/ro03deda/Documents/Projects/Fish China/temp/TaxoFishFinal.csv")
###---###---###



#### 4. Functional space for All fish China ----

sp_dist_function_All <- mFD::funct.dist(
  sp_tr = functional_traits,
  tr_cat = trait_category,
  metric = "gower",
  scale_euclid = "scale_center",
  ordinal_var = "classic",
  weight_type = "equal",
  stop_if_NA = TRUE)
sp_dist_function_All <- round(sp_dist_function_All, 3)

# Compute multidimensional functional spaces and assess their quality
fspaces_quality_function_All <- mFD::quality.fspaces(
  sp_dist = sp_dist_function_All,
  maxdim_pcoa = 10,
  deviation_weighting = "absolute",
  fdist_scaling = FALSE,
  fdendro = "average")
round(fspaces_quality_function_All$"quality_fspaces", 3)

sp_faxes_coord_function_All <- fspaces_quality_function_All$"details_fspaces"$"sp_pc_coord"


#### 4. Calculations - Functional Diversity for each scenario----

FD_AT <- list()
FD_100 <- list()
FD_AT_R <- list()
FD_100_R <- list()

i <- 25
j <- 1
k <- 1
e <- 1
t <- 1


#### Running parallel ----

library("future")
library("future.apply")

plan(multisession, workers = 5)

hu <- future_lapply(1:100, function(i) {
  #### 4.1 Inferring missing Extinction risk of DD species ----
  DD_species <- TaxoFishFinal[which(TaxoFishFinal$Ext_Status == "DD"),] # Select species with DD status
  InfStatus <- c()
  
  #Loop for infer conservation status of DD species
  for(j in 1:nrow(DD_species)){
    SubFam <- DD_species$SubFamily[j] # identify subfamily
    ExtStatus <- pull(TaxoFishFinal[which(TaxoFishFinal$SubFamily == SubFam),"Ext_Status"]) # extract species conservation status in subfamily
    ExtStatus_Samp <- ExtStatus[which(ExtStatus != "DD")] # Delete DD status
    
    #conditional to infer conservation status according to subfamily, family, or oder
    if(length(ExtStatus_Samp) > 3) {
      InfStatus[j] <- sample(ExtStatus_Samp,1)
    } else {
      Fam <- DD_species$Family[j]
      Fam_ExtStatus <- pull(TaxoFishFinal[which(TaxoFishFinal$Family == Fam),"Ext_Status"])
      Fam_ExtStatus_Samp <- Fam_ExtStatus[which(Fam_ExtStatus != "DD")]
      if(length(Fam_ExtStatus_Samp) > 0){
        InfStatus[j] <- sample(Fam_ExtStatus_Samp,1)
      } else {
        Ord <- DD_species$Order[j]
        Ord_ExtStatus <- pull(TaxoFishFinal[which(TaxoFishFinal$Order == Ord),"Ext_Status"])
        Ord_ExtStatus_Samp <- Ord_ExtStatus[which(Ord_ExtStatus != "DD")]
        InfStatus[j] <- sample(Ord_ExtStatus_Samp,1)
      }
    }
  }
  
  # Replace the DD status for the new status
  TaxoFishFinal_noDD <- TaxoFishFinal
  TaxoFishFinal_noDD[which(TaxoFishFinal_noDD$Species %in% DD_species$Species),"Ext_Status"] <- InfStatus
  
  #### 4.3 Scenario IUCN AT ----
  SppExtin_AT <- pull(TaxoFishFinal_noDD[which(TaxoFishFinal_noDD$Ext_Status %in% c("VU","EN","CR","EX","RE")), "Sp_Code"]) #species extinct
  species_site_AT <- species_site[,-(which(colnames(species_site) %in% SppExtin_AT))] # species after extinction
  
  #### 4.3.1 calculate functional diversity - IUCN AT ----
  alpha_fd_indices_species_AT <- mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord_function_All[ , c("PC1","PC2","PC3","PC4","PC5")], 
    asb_sp_w = species_site_AT,
    ind_vect = c("fric"),
    scaling = TRUE,
    check_input = TRUE,
    details_returned = TRUE)
  
  fd_ind_values_species_AT <- alpha_fd_indices_species_AT$"functional_diversity_indices"
  fd_ind_values_species_AT2 <- as.data.frame(fd_ind_values_species_AT)
  fd_ind_values_species_AT2 <- as_tibble(rownames_to_column(fd_ind_values_species_AT2,"Spatial"))
  fd_ind_values_species_AT2$Iteration <- i
  
  FD_AT[[i]] <- fd_ind_values_species_AT2 # save results functional diversity
  write.csv(fd_ind_values_species_AT2, paste("C:/Users/ro03deda/Documents/Projects/Fish China/Results/Partial_metrics/By_region/","FD_AT_",i,".csv",sep = ""))
  
  #### 4.4 Scenario IUCN 100 ----
  Spp_Ext <- pull(TaxoFishFinal_noDD[which(TaxoFishFinal_noDD$Ext_Status %in% c("EX","RE")), "Sp_Code"])
  Spp_CR <- pull(TaxoFishFinal_noDD[which(TaxoFishFinal_noDD$Ext_Status == "CR"), "Sp_Code"])
  Spp_CR2 <- sample(Spp_CR, round(length(Spp_CR)*0.999,0))
  Spp_EN <- pull(TaxoFishFinal_noDD[which(TaxoFishFinal_noDD$Ext_Status == "EN"), "Sp_Code"])
  Spp_EN2 <- sample(Spp_EN, round(length(Spp_EN)*0.667,0))
  Spp_VU <- pull(TaxoFishFinal_noDD[which(TaxoFishFinal_noDD$Ext_Status == "VU"), "Sp_Code"])
  Spp_VU2 <- sample(Spp_VU, round(length(Spp_VU)*0.1,0))
  Spp_NT <- pull(TaxoFishFinal_noDD[which(TaxoFishFinal_noDD$Ext_Status == "NT"), "Sp_Code"])
  Spp_NT2 <- sample(Spp_NT, round(length(Spp_NT)*0.01,0))
  Spp_LC <- pull(TaxoFishFinal_noDD[which(TaxoFishFinal_noDD$Ext_Status == "LC"), "Sp_Code"])
  Spp_LC2 <- sample(Spp_LC, round(length(Spp_LC)*0.001,0))
  
  Spp_Ext_100 <- c(Spp_Ext,Spp_CR2,Spp_EN2,Spp_LC2,Spp_NT2,Spp_VU2)
  species_site_100 <- species_site[,-(which(colnames(species_site) %in% Spp_Ext_100))]
  
  #### 4.4.1 calculate functional diversity - IUCN 100 ----
  
  alpha_fd_indices_species_100 <- mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord_function_All[ , c("PC1","PC2","PC3","PC4","PC5")], 
    asb_sp_w = species_site_100,
    ind_vect = c("fric"),
    scaling = TRUE,
    check_input = TRUE,
    details_returned = TRUE)
  
  fd_ind_values_species_100 <- alpha_fd_indices_species_100$"functional_diversity_indices"
  fd_ind_values_species_100_2 <- as.data.frame(fd_ind_values_species_100)
  fd_ind_values_species_100_2 <- as_tibble(rownames_to_column(fd_ind_values_species_100_2,"Spatial"))
  fd_ind_values_species_100_2$Iteration <- i
  
  FD_100[[i]] <- fd_ind_values_species_100_2 # save results functional diversity
  write.csv(fd_ind_values_species_100_2, paste("C:/Users/ro03deda/Documents/Projects/Fish China/Results/Partial_metrics/By_region/","FD_100_",i,".csv",sep = ""))
  
  #### 4.5 Scenario IUCN AT - random ----
  
  FD_AT_Par <- list()
  for(k in 1:nrow(fd_ind_values_species_AT2)){
    spatial_sce <- fd_ind_values_species_AT2$Spatial[k] #identify the spatial scenario
    data_spatial <- matrix(species_site[which(row.names(species_site) == spatial_sce),], ncol = 1591, nrow = 1) #subset data and create a matrix
    colnames(data_spatial) <- colnames(species_site)
    spp_sce <- colnames(data_spatial)[which(data_spatial == 1)] #species present in a scenario
    num_spp_ext <- as.numeric(rowSums(species_site)[k]) - as.numeric(fd_ind_values_species_AT2[which(fd_ind_values_species_AT2$Spatial == spatial_sce), "sp_richn"])#number of species extinct
    SppExtin_AT_Rand <- sample(spp_sce, num_spp_ext) #species extinct
    species_site_AT_Rand <- species_site[k,-(which(colnames(species_site) %in% SppExtin_AT_Rand)), drop=FALSE] # species after extinction
    
    #### 4.5.1 calculate functional diversity - IUCN AT Rand----
    alpha_fd_indices_species_AT_Rand <- mFD::alpha.fd.multidim(
      sp_faxes_coord = sp_faxes_coord_function_All[ , c("PC1","PC2","PC3","PC4","PC5")], 
      asb_sp_w = species_site_AT_Rand,
      ind_vect = c("fric"),
      scaling = TRUE,
      check_input = TRUE,
      details_returned = TRUE)
    
    fd_ind_values_species_AT_Rand <- alpha_fd_indices_species_AT_Rand$"functional_diversity_indices"
    fd_ind_values_species_AT2_Rand <- as.data.frame(fd_ind_values_species_AT_Rand)
    fd_ind_values_species_AT2_Rand <- as_tibble(rownames_to_column(fd_ind_values_species_AT2_Rand,"Spatial"))
    fd_ind_values_species_AT2_Rand$Iteration <- i
    FD_AT_Par[[k]] <- fd_ind_values_species_AT2_Rand
  }
  
  fd_ind_values_species_AT3_Rand <- bind_rows(FD_AT_Par)
  FD_AT_R[[i]] <- bind_rows(fd_ind_values_species_AT3_Rand) # save results functional diversity
  write.csv(fd_ind_values_species_AT3_Rand, paste("C:/Users/ro03deda/Documents/Projects/Fish China/Results/Partial_metrics/By_region/","FD_AT_R_",i,".csv",sep = ""))
  
  #### 4.6 Scenario IUCN 100 - random ----
  
  FD_100_Par <- list()
  for(y in 1:nrow(fd_ind_values_species_100_2)){
    spatial_sce <- fd_ind_values_species_100_2$Spatial[y] #identify the spatial scenario
    data_spatial <- matrix(species_site[which(row.names(species_site) == spatial_sce),], ncol = 1591, nrow = 1) #subset data and create a matrix
    colnames(data_spatial) <- colnames(species_site)
    spp_sce <- colnames(data_spatial)[which(data_spatial == 1)] #species present in a scenario
    num_spp_ext <- as.numeric(rowSums(species_site)[y]) - as.numeric(fd_ind_values_species_100_2[which(fd_ind_values_species_100_2$Spatial == spatial_sce), "sp_richn"])#number of species extinct
    if(num_spp_ext > 0){
      SppExtin_100_Rand <- sample(spp_sce, num_spp_ext) #species extinct
      species_site_100_Rand <- species_site[y,-(which(colnames(species_site) %in% SppExtin_100_Rand)), drop=FALSE]
      
      #### 4.6.1 calculate functional diversity - IUCN 100 ----
      alpha_fd_indices_species_100_Rand <- mFD::alpha.fd.multidim(
        sp_faxes_coord = sp_faxes_coord_function_All[ , c("PC1","PC2","PC3","PC4","PC5")], 
        asb_sp_w = species_site_100_Rand,
        ind_vect = c("fric"),
        scaling = TRUE,
        check_input = TRUE,
        details_returned = TRUE)
      
      fd_ind_values_species_100_Rand <- alpha_fd_indices_species_100_Rand$"functional_diversity_indices"
      fd_ind_values_species_100_2_Rand <- as.data.frame(fd_ind_values_species_100_Rand)
      fd_ind_values_species_100_2_Rand <- as_tibble(rownames_to_column(fd_ind_values_species_100_2_Rand,"Spatial"))
      fd_ind_values_species_100_2_Rand$Iteration <- i
      FD_100_Par[[y]] <- fd_ind_values_species_100_2_Rand
    } else {
      species_site_100_Rand <- species_site[y,]
      species_site_100_Rand_2 <- matrix(species_site_100_Rand, ncol = length(species_site_100_Rand), nrow = 1) #subset data and create a matrix
      colnames(species_site_100_Rand_2) <- names(species_site_100_Rand)
      rownames(species_site_100_Rand_2) <- spatial_sce
      alpha_fd_indices_species_100_Rand <- mFD::alpha.fd.multidim(
        sp_faxes_coord = sp_faxes_coord_function_All[ , c("PC1","PC2","PC3","PC4","PC5")], 
        asb_sp_w = species_site_100_Rand_2,
        ind_vect = c("fric"),
        scaling = TRUE,
        check_input = TRUE,
        details_returned = TRUE)
      
      fd_ind_values_species_100_Rand <- alpha_fd_indices_species_100_Rand$"functional_diversity_indices"
      fd_ind_values_species_100_2_Rand <- as.data.frame(fd_ind_values_species_100_Rand)
      fd_ind_values_species_100_2_Rand <- as_tibble(rownames_to_column(fd_ind_values_species_100_2_Rand,"Spatial"))
      fd_ind_values_species_100_2_Rand$Iteration <- i
      FD_100_Par[[y]] <- fd_ind_values_species_100_2_Rand
    }
  
  }
  
  fd_ind_values_species_100_3_Rand <- bind_rows(FD_100_Par)
  FD_100_R[[i]] <- bind_rows(fd_ind_values_species_100_3_Rand) # save results functional diversity
  write.csv(fd_ind_values_species_100_3_Rand, paste("C:/Users/ro03deda/Documents/Projects/Fish China/Results/Partial_metrics/By_region/","FD_100_R_",i,".csv",sep = ""))
  
})


#read all results
File_paths <- list.files("C:/Users/ro03deda/Documents/Projects/Fish China/Results/Partial_metrics/By_region/", full.names = T)

#Read files per metric and scenario
FD_100_2 <- lapply(File_paths[setdiff(grep("FD_100",File_paths,fixed = T),grep("FD_100_R",File_paths,fixed = T))], read.csv)
FD_AT_2 <- lapply(File_paths[setdiff(grep("FD_AT",File_paths,fixed = T),grep("FD_AT_R",File_paths,fixed = T))], read.csv)
FD_AT_R_2 <- lapply(File_paths[grep("FD_AT_R",File_paths,fixed = T)], read.csv)
FD_100_R_2 <- lapply(File_paths[grep("FD_100_R",File_paths,fixed = T)], read.csv)

#Bind tables by metric and scenario
FD_100_3 <- dplyr::bind_rows(FD_100_2)
FD_AT_3 <- dplyr::bind_rows(FD_AT_2)
FD_AT_R_3 <- dplyr::bind_rows(FD_AT_R_2)
FD_100_R_3 <- dplyr::bind_rows(FD_100_R_2)



#Save results
#write.csv(FD_100_3,"C:/Users/ro03deda/Documents/Projects/Fish China/Results/merged/Partial_metrics/FD_100_3.csv")
#write.csv(FD_AT_3,"C:/Users/ro03deda/Documents/Projects/Fish China/Results/merged/Partial_metrics/FD_AT_3.csv")
#write.csv(FD_AT_R_3,"C:/Users/ro03deda/Documents/Projects/Fish China/Results/merged/Partial_metrics/FD_AT_R_3.csv")
#write.csv(FD_100_R_3,"C:/Users/ro03deda/Documents/Projects/Fish China/Results/merged/Partial_metrics/FD_100_R_3.csv")



#### 5. Functional diversity All fish China ----
sp_dist_function_All <- mFD::funct.dist(
  sp_tr = functional_traits,
  tr_cat = trait_category,
  metric = "gower",
  scale_euclid = "scale_center",
  ordinal_var = "classic",
  weight_type = "equal",
  stop_if_NA = TRUE)
sp_dist_function_All <- round(sp_dist_function_All, 3)

# Compute multidimensional functional spaces and assess their quality
fspaces_quality_function_All <- mFD::quality.fspaces(
  sp_dist = sp_dist_function_All,
  maxdim_pcoa = 10,
  deviation_weighting = "absolute",
  fdist_scaling = FALSE,
  fdendro = "average")
round(fspaces_quality_function_All$"quality_fspaces", 3)

sp_faxes_coord_function_All <- fspaces_quality_function_All$"details_fspaces"$"sp_pc_coord"

function_tr_faxes_All <- mFD::traits.faxes.cor(
  sp_tr = functional_traits, 
  sp_faxes_coord = sp_faxes_coord_function_All[ , c("PC1", "PC2", "PC3", "PC4","PC5")], 
  plot = FALSE)

alpha_fd_indices_species_All <- mFD::alpha.fd.multidim(
  sp_faxes_coord = sp_faxes_coord_function_All[ , c("PC1","PC2","PC3","PC4","PC5")], 
  asb_sp_w = species_site,
  ind_vect = c("fric"),
  scaling = TRUE,
  check_input = TRUE,
  details_returned = TRUE)

fd_ind_values_species_All <- alpha_fd_indices_species_All$"functional_diversity_indices"
fd_ind_values_species_All_2 <- as.data.frame(fd_ind_values_species_All)
fd_ind_values_species_All_2 <- as_tibble(rownames_to_column(fd_ind_values_species_All_2,"Spatial"))

#write.csv(fd_ind_values_species_All_2, paste("C:/Users/ro03deda/Documents/Projects/Fish China/Results/Results_All/FD_All.csv",sep = ""))
fd_ind_values_species_All_2 <- read.csv("C:/Users/ro03deda/Documents/Projects/Fish China/Results/Results_All/FD_All.csv")



#### 7. Functional loss ----
FD_A_F <- fd_ind_values_species_All_2

FD_100_4 <- FD_100_3[,-1]
FD_AT_4 <- FD_AT_3[,-1]
FD_AT_R_4 <- FD_AT_R_3[,-1]
FD_100_R_4 <- FD_100_R_3[,-1]


###---###
i <- 1
FD_100_5 <- list()
for(i in 1:length(unique(FD_100_4$Iteration))){
  FD_A_F_X <- FD_A_F[, c("Spatial","sp_richn", "fric")]
  FD_100_X <- FD_100_4[which(FD_100_4$Iteration == i), c("Spatial","sp_richn","fric")]
  S <- FD_A_F_X[,c("sp_richn", "fric")] - FD_100_X[,c("sp_richn", "fric")]
  S2 <- ((S * 100)/FD_A_F_X[,c("sp_richn", "fric")])*-1
  FD_100_F <- cbind(FD_A_F_X[,1,drop=FALSE],S2)
  FD_100_F$Iteration <- i
  FD_100_5[[i]] <- FD_100_F
}

FD_100_Dif <- bind_rows(FD_100_5)
write.csv(FD_100_Dif, "C:/Users/ro03deda/Documents/Projects/Fish China/Results/merged/Dif/Partial_metrics/By_region/FD_100_Dif.csv")


###---###
FD_AT_5 <- list()
for(i in 1:length(unique(FD_AT_4$Iteration))){
  FD_A_F_X <- FD_A_F[,c("Spatial","sp_richn", "fric")]
  FD_AT_X <- FD_AT_4[which(FD_AT_4$Iteration == i),c("Spatial","sp_richn", "fric")]
  S <- FD_A_F_X[,c("sp_richn", "fric")] - FD_AT_X[,c("sp_richn", "fric")]
  S2 <- ((S * 100)/FD_A_F_X[,c("sp_richn", "fric")])*-1
  FD_AT_F <- cbind(FD_A_F_X[,1,drop=FALSE],S2)
  FD_AT_F$Iteration <- i
  FD_AT_5[[i]] <- FD_AT_F
}

FD_AT_Dif <- bind_rows(FD_AT_5)
write.csv(FD_AT_Dif, "C:/Users/ro03deda/Documents/Projects/Fish China/Results/merged/Dif/Partial_metrics/By_region/FD_AT_Dif.csv")

###---###
FD_AT_R_5 <- list()
for(i in 1:length(unique(FD_AT_R_4$Iteration))){
  FD_A_F_X <- FD_A_F[, c("Spatial","sp_richn", "fric")]
  FD_AT_R_X <- FD_AT_R_4[which(FD_AT_R_4$Iteration == i), c("Spatial","sp_richn", "fric")]
  S <- FD_A_F_X[,c("sp_richn", "fric")] - FD_AT_R_X[,c("sp_richn", "fric")]
  S2 <- ((S * 100)/FD_A_F_X[,c("sp_richn", "fric")])*-1
  FD_AT_R_F <- cbind(FD_A_F_X[,1,drop=FALSE],S2)
  FD_AT_R_F$Iteration <- i
  FD_AT_R_5[[i]] <- FD_AT_R_F
}

FD_AT_R_Dif <- bind_rows(FD_AT_R_5)
write.csv(FD_AT_R_Dif, "C:/Users/ro03deda/Documents/Projects/Fish China/Results/merged/Dif/Partial_metrics/By_region/FD_AT_R_Dif.csv")

###---###
FD_100_R_5 <- list()
for(i in 1:length(unique(FD_100_R_4$Iteration))){
  FD_A_F_X <- FD_A_F[, c("Spatial","sp_richn", "fric")]
  FD_100_R_X <- FD_100_R_4[which(FD_100_R_4$Iteration == i), c("Spatial","sp_richn", "fric")]
  S <- FD_A_F_X[,c("sp_richn", "fric")] - FD_100_R_X[,c("sp_richn", "fric")]
  S2 <- ((S * 100)/FD_A_F_X[,c("sp_richn", "fric")])*-1
  FD_100_R_F <- cbind(FD_A_F_X[,1,drop=FALSE],S2)
  FD_100_R_F$Iteration <- i
  FD_100_R_5[[i]] <- FD_100_R_F
}

FD_100_R_Dif <- bind_rows(FD_100_R_5)
write.csv(FD_100_R_Dif, "C:/Users/ro03deda/Documents/Projects/Fish China/Results/merged/Dif/Partial_metrics/By_region/FD_100_R_Dif.csv")


###---###

#Add 
FD_100_Dif$scenario <- "IUCN_100"
FD_AT_Dif$scenario <- "IUCN_AT"
FD_AT_R_Dif$scenario <- "IUCN_AT_R"
FD_100_R_Dif$scenario <- "IUCN_100_R"

#Merge dataset
FD_ALL <- rbind(FD_100_Dif,FD_AT_Dif,FD_AT_R_Dif,FD_100_R_Dif)

#Change names Spatial
FD_ALL_2 <- left_join(FD_ALL,regions_names[,c("region_ID","region_name")],by=c("Spatial"="region_ID"))
write.csv(FD_ALL_2, "C:/Users/ro03deda/Documents/Projects/Fish China/Results/merged/Dif/Partial_metrics/By_region/FD_ALL_China.csv")



#### 8. statistical test ----

FD_ALL_2 <- as_tibble(FD_ALL_2)
head(FD_ALL_2)
regions <- unique(FD_ALL_2$region_name)
scenarios <- c("IUCN_100","IUCN_AT","IUCN_AT_R","IUCN_100_R")
metrics <- c("sp_richn","fric")


i <- 1
j <- 1
t <- 1

combin <- combn(scenarios,2)
Results <- tibble(region=NA, metric=NA, scenario_1=NA, scenario_2=NA, test=NA, p_value=NA)
row_line <- 0

for(i in 1:length(regions)){
  data_x <- filter(FD_ALL_2, region_name == regions[i])
  
  for(t in 1:ncol(combin)){
    data_1 <- filter(data_x, scenario == combin[1,t])
    data_2 <-  filter(data_x, scenario == combin[2,t])
    
    for(j in 1:length(metrics)){
      data_metric_1 <- pull(select(data_1, metrics[j]))
      data_metric_2 <- pull(select(data_2, metrics[j]))
      st1 <- shapiro.test(data_metric_1)
      st2 <- shapiro.test(data_metric_2)
      st1_p <- st1$p.value
      st2_p <- st2$p.value
      
      if(st1_p > 0.05 & st2_p > 0.05){
        test_res <- t.test(data_metric_1,data_metric_2)
        row_line <- row_line + 1
        Results[row_line,"region"] <- regions[i]
        Results[row_line,"metric"] <- metrics[j]
        Results[row_line,"scenario_1"] <- unique(data_1$scenario)
        Results[row_line,"scenario_2"] <- unique(data_2$scenario)
        Results[row_line,"test"] <- "t_test"
        Results[row_line,"p_value"] <- test_res$p.value
      } else {
        test_res <- wilcox.test(data_metric_1,data_metric_2)
        row_line <- row_line + 1
        Results[row_line,"region"] <- regions[i]
        Results[row_line,"metric"] <- metrics[j]
        Results[row_line,"scenario_1"] <- unique(data_1$scenario)
        Results[row_line,"scenario_2"] <- unique(data_2$scenario)
        Results[row_line,"test"] <- "Wilcox"
        Results[row_line,"p_value"] <- test_res$p.value
      }
    }
  }
}

write.csv(Results, "C:/Users/ro03deda/Documents/Projects/Fish China/Results/merged/Dif/Partial_metrics/By_region/Results_stat_test.csv")









