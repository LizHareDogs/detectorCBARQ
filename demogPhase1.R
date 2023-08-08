### remove records with shifted character fields
### list dog IDs for bad records
badDogs <- c(155436, 155545)
### keep all except bad dog
rawPhase1 <- rawPhase1[!rawPhase1$Dog.ID %in% badDogs, ]

### change erroneous character cols to numeric
rawPhase1$Fear21 <- as.numeric(rawPhase1$Fear21)
rawPhase1$Excite35 <- as.numeric(rawPhase1$Excite35)

## age
class(rawPhase1$Birthday)
head(rawPhase1$Birthday)
tail(rawPhase1$Birthday)
### they are Excel dates
rawPhase1$dob <- convert_to_date(rawPhase1$Birthday)
min(rawPhase1$dob)
max(rawPhase1$dob)

### createdt is when they took survey?
class(rawPhase1$createdt)
head(rawPhase1$createdt)
rawPhase1$surveyDate <- convert_to_date(rawPhase1$createdt)
min(rawPhase1$surveyDate)
max(rawPhase1$surveyDate)

rawPhase1$age <- rawPhase1$surveyDate - rawPhase1$dob
rawPhase1$ageY <- round(as.numeric(rawPhase1$age/365.25), digits=2)
summary(as.numeric(rawPhase1$ageY))

### sex
class(rawPhase1$Sex)
table(rawPhase1$Sex, useNA="ifany")
rawPhase1$sexF <- as.factor(rawPhase1$Sex)

class(rawPhase1$Breed)
breedT <- table(rawPhase1$Breed, useNA="ifany")
rawPhase1$breedF <- factor(rawPhase1$Breed)
### hunting/herding ?
### list breeds
write.csv(rownames(breedT), file="breeds.csv")
rawPhase1$breedGroup[rawPhase1$breedF == "American Coonhound"] <- "Hound"
rawPhase1$breedGroup[rawPhase1$breedF == "American Pit Bull Terrier"] <- "Terrier"
rawPhase1$breedGroup[rawPhase1$breedF == "Anatolian Shepherd "] <- "Working"
rawPhase1$breedGroup[rawPhase1$breedF == "Australian Cattle Dog"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Australian Kelpie "] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Australian Koolie"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Australian Shepherd"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Beauceron"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Belgian Malinois "] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Belgian Malinois x Dutch Shepherd cross"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Belgian Tervuren "] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Bloodhound"] <- "Hound"
rawPhase1$breedGroup[rawPhase1$breedF == "Border Collie"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Boxer"] <- "Working"
rawPhase1$breedGroup[rawPhase1$breedF == "Brittany"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Chesapeake Bay Retriever "] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Cocker Spaniel (English)"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Collie (Rough)"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Cross-breed"] <- NA
rawPhase1$breedGroup[rawPhase1$breedF == "Curly-Coated Retriever "] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Dalmatian"] <- "Non-Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Doberman Pinscher"] <- "Working"
rawPhase1$breedGroup[rawPhase1$breedF == "Dutch Shepherd"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "English Coonhound"] <- "Hound"
rawPhase1$breedGroup[rawPhase1$breedF == "English Shepherd"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "English Springer Spaniel"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Flat-Coated Retriever "] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Fox Terrier (Smooth)"] <- "Terrier"
rawPhase1$breedGroup[rawPhase1$breedF == "German Shepherd"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "German Shorthaired Pointer"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "German Wirehaired Pointer "] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Golden Retriever"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Gordon Setter"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "English Coonhound"] <- "Hound"
rawPhase1$breedGroup[rawPhase1$breedF == "English Shepherd"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "English Springer Spaniel"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Flat-Coated Retriever "] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Fox Terrier (Smooth)"] <- "Terrier"
rawPhase1$breedGroup[rawPhase1$breedF == "German Shepherd"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "German Shorthaired Pointer"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "German Wirehaired Pointer "] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Golden Retriever"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Gordon Setter"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Gordon Setter"] <- "Terrier"
rawPhase1$breedGroup[rawPhase1$breedF == "Jagd Terrier"] <- "Terrier"
rawPhase1$breedGroup[rawPhase1$breedF ==  "Labrador Retriever"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Lagotto Romagnolo"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Lancashire Heeler"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "McNab Shepherd"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Mixed-breed"] <- NA
rawPhase1$breedGroup[rawPhase1$breedF == "Nova Scotia Duck Tolling Retriever "] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Papillon "] -> "Toy"
rawPhase1$breedGroup[rawPhase1$breedF == "Pembroke Welsh Corgi "] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Pit Bull"] <- "Terrier"
rawPhase1$breedGroup[rawPhase1$breedF == "Poodle (Standard)"] <- "Non-Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Rhodesian Ridgeback"] <- "Hound"
rawPhase1$breedGroup[rawPhase1$breedF == "Rottweiler"] <- "Working"
rawPhase1$breedGroup[rawPhase1$breedF == "Schipperke "] <- "Non-Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Shetland Sheepdog"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Vizsla"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Welsh Springer Spaniel "] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "Weimaraner"] <- "Sporting"
rawPhase1$breedGroup[rawPhase1$breedF == "White Swiss Shepherd Dog"] <- "Herding"
rawPhase1$breedGroup[rawPhase1$breedF == "Wirehaired Pointing Griffon "] <- "Sporting"
table(rawPhase1$breedGroup, useNA="ifany")
rawPhase1$breedGroupF <- factor(rawPhase1$breedGroup)




table(rawPhase1$breeddescription, useNA="ifany")
### these are varied free text

table(rawPhase1$breedGroup, useNA="ifany")

### neutered
### variable from xlsx is "Neutered?" and the question mark causes problems with R. Rename

rawPhase1$neutered <- rawPhase1[ ,c("Neutered?")]
rawPhase1$neuteredF <- factor(rawPhase1$neutered)

### country
table(rawPhase1$Country)
rawPhase1$countryF <- factor(rawPhase1$Country)

### working status
table(rawPhase1$workingstatus, useNA="ifany")
rawPhase1$workingStatusF <- factor(rawPhase1$workingstatus,
                             levels=c("training", "working", "retired"))
### types of work
### dual purpose
table(rawPhase1$duallytrained, useNA="ifany")
rawPhase1$duallyTrainedF <- factor(rawPhase1$duallytrained,
                             level=c("no", "yes"))

### odor detection variables
odorVars <- dplyr::select(rawPhase1, starts_with("odor"))
### remove column with free text so you can convert others to no/yes factors
odorVars$odordescription <- NULL
### convert to factors
### approach to converting all columns from
dim(odorVars)
index <- 1:ncol(odorVars)
odorVars[ ,index] <- lapply(odorVars[ ,index], as.factor)
sapply(odorVars, class)
sapply(odorVars, levels)






### separate dually trained dogs for table
### convert to factor
rawPhase1$duallyTrainedF <- factor(rawPhase1$duallytrained,
                                levels=c("no", "yes"))
### df with only dually trained dogs
rawPhase1Dual <- rawPhase1[rawPhase1$duallyTrainedF == "yes", ]
### remove unused levels of Breed that only occur in the single purpose dogs
rawPhase1Dual$breedF <- droplevels(rawPhase1Dual$breedF)
table(rawPhase1Dual$breedF)


certificationVars <- dplyr::select(rawPhase1, starts_with("cert"))
certificationVars$certificatedescription <- NULL

### convert to factors
### approach to converting all columns from
index <- 1:ncol(certificationVars)
certificationVars[ ,index] <- lapply(certificationVars[ ,index], as.factor)
sapply(certificationVars, class)
sapply(certificationVars, levels)
### make factors for some individual variables
table(rawPhase1$alerttype, useNA="ifany")
rawPhase1$alerttypeF <- factor(rawPhase1$alerttype)


table(rawPhase1$rewardsystem, useNA="ifany")
rawPhase1$rewardsystemF <- factor(rawPhase1$rewardsystem)

table(rawPhase1$dogshandledbefore, useNA="ifany")
rawPhase1$dogshandledbeforeF <- factor(rawPhase1$dogshandledbefore,
                                 levels=c("0", "1 to 2", "3 to 5", "more than 5"))

table(rawPhase1$dogshomelocation, useNA="ifany")
rawPhase1$dogshomelocationF <- factor(rawPhase1$dogshomelocation)


### keep ratings numeric
table(rawPhase1$scentrating, useNA="ifany")
table(rawPhase1$behaviorrating, useNA="ifany")
### remove responses that are out of bounds
class(rawPhase1$scentrating)
rawPhase1$scentrating[rawPhase1$scentrating > 10] <- NA
rawPhase1$behaviorrating[rawPhase1$behaviorrating > 10] <- NA

### sample (dna?)
table(rawPhase1$sampleprovided, useNA="ifany")
rawPhase1$sampleprovidedF <- factor(rawPhase1$sampleprovided)## DNA sample

### rename columns to match otch phase 2 df

rawPhase1 <- rawPhase1 %>%
    rename(
        TRAIN01 = Train01,
        TRAIN02 = Train02,
        TRAIN03 = Train03,
        TRAIN04 = Train04,
        TRAIN05 = Train05,
        TRAIN06 = Train06,
        TRAIN07 = Train07,
        TRAIN08 = Train08,
        AGG09 = Agg09,
        AGG10 = Agg10,
        AGG11 = Agg11,
        AGG12 = Agg12,
        AGG13 = Agg13,
        AGG14 = Agg14,
        AGG15 = Agg15,
        AGG16 = Agg16,
        AGG17 = Agg17,
        AGG18 = Agg18,
        AGG19 = Agg19,
        AGG20 = Agg20,
        FEAR21 = Fear21,
        FEAR22 = Fear22,
        FEAR23 = Fear23,
        FEAR24 = Fear24,
        FEAR25 = Fear25,
        FEAR26 = Fear26,
        FEAR27 = Fear27,
        FEAR28 = Fear28,
        FEAR29 = Fear29,
        FEAR30 = Fear30,
        FEAR31 = Fear31,
        SEPR32 = SepProb32,
        SEPR33 = SepProb33,
        SEPR34 = SepProb34,
        EXCITE35 = Excite35,
        EXCITE36 = Excite36,
        EXCITE37 = Excite37,
        EXCITE38 = Excite38,
        ATT39 = AttAtt39,
        ATT40 = AttAtt40,
        ATT41 = AttAtt41,
        ATT42 = AttAtt42,
        PLAY43 = Play43,
        PLAY44 = Play44,
        PLAY45 = Play45,
        PLAY46 = Play46,
        IMP47 = Frus47,
        IMP48 = Frus48,
        IMP49 = Frus49,
        MISC50 = Frus50,
        MISC51 = Misc51,
        MISC52 = Misc52,
        MISC53 = Misc53,
        MISC54 = Misc54,
        MISC55 = Misc55,
        MISC56 = Misc56,
        MISC57 = Misc57,
        MISC58 = Misc58,
        MISC59 = Misc59,
        MISC60 = Misc60,
        MISC61 = Misc61,
        MISC62 = Misc62,
        MISC63 = Misc63,
        MISC64 = Misc64,
        MISC65 = Misc65,
        MISC66 = Misc66,
        MISC67 = Misc67,
        MISC68 = Misc68,
        MISC69 = Misc69,
        MISC70 = Misc70,
        MISC71 = Misc71)



save(rawPhase1, file="rawPhase1.Rda")
