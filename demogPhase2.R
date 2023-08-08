### remove chasing items
rawPhase2$MISC72 <- NULL
rawPhase2$MISC73 <- NULL
rawPhase2$MISC74 <- NULL

## age
class(rawPhase2$Birthday)
head(rawPhase2$Birthday)
tail(rawPhase2$Birthday)
### they are Excel dates
rawPhase2$dob <- convert_to_date(rawPhase2$Birthday)
min(rawPhase2$dob)
max(rawPhase2$dob)

### createdt is when they took survey?
class(rawPhase2$createdt)
head(rawPhase2$createdt)
rawPhase2$surveyDate <- convert_to_date(rawPhase2$createdt)
min(rawPhase2$surveyDate)
max(rawPhase2$surveyDate)

rawPhase2$age <- rawPhase2$surveyDate - rawPhase2$dob
rawPhase2$ageY <- round(as.numeric(rawPhase2$age/365.25), digits=2)
summary(as.numeric(rawPhase2$ageY))

### sex
class(rawPhase2$Sex)
table(rawPhase2$Sex, useNA="ifany")
rawPhase2$sexF <- as.factor(rawPhase2$Sex)

class(rawPhase2$Breed)
breedT <- table(rawPhase2$Breed, useNA="ifany")
rawPhase2$breedF <- factor(rawPhase2$Breed)
### hunting/herding ?
### list breeds
write.csv(rownames(breedT), file="breeds.csv")
rawPhase2$breedGroup[rawPhase2$breedF == "American Coonhound"] <- "Hound"
rawPhase2$breedGroup[rawPhase2$breedF == "American Pit Bull Terrier"] <- "Terrier"
rawPhase2$breedGroup[rawPhase2$breedF == "Anatolian Shepherd "] <- "Working"
rawPhase2$breedGroup[rawPhase2$breedF == "Australian Cattle Dog"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Australian Kelpie "] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Australian Koolie"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Australian Shepherd"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Beauceron"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Belgian Malinois "] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Belgian Malinois x Dutch Shepherd cross"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Belgian Tervuren "] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Bloodhound"] <- "Hound"
rawPhase2$breedGroup[rawPhase2$breedF == "Border Collie"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Boxer"] <- "Working"
rawPhase2$breedGroup[rawPhase2$breedF == "Brittany"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Chesapeake Bay Retriever "] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Cocker Spaniel (English)"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Collie (Rough)"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Cross-breed"] <- NA
rawPhase2$breedGroup[rawPhase2$breedF == "Curly-Coated Retriever "] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Dalmatian"] <- "Non-Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Doberman Pinscher"] <- "Working"
rawPhase2$breedGroup[rawPhase2$breedF == "Dutch Shepherd"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "English Coonhound"] <- "Hound"
rawPhase2$breedGroup[rawPhase2$breedF == "English Shepherd"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "English Springer Spaniel"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Flat-Coated Retriever "] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Fox Terrier (Smooth)"] <- "Terrier"
rawPhase2$breedGroup[rawPhase2$breedF == "German Shepherd"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "German Shorthaired Pointer"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "German Wirehaired Pointer "] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Golden Retriever"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Gordon Setter"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "English Coonhound"] <- "Hound"
rawPhase2$breedGroup[rawPhase2$breedF == "English Shepherd"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "English Springer Spaniel"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Flat-Coated Retriever "] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Fox Terrier (Smooth)"] <- "Terrier"
rawPhase2$breedGroup[rawPhase2$breedF == "German Shepherd"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "German Shorthaired Pointer"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "German Wirehaired Pointer "] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Golden Retriever"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Gordon Setter"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Gordon Setter"] <- "Terrier"
rawPhase2$breedGroup[rawPhase2$breedF == "Jagd Terrier"] <- "Terrier"
rawPhase2$breedGroup[rawPhase2$breedF ==  "Labrador Retriever"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Lagotto Romagnolo"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Lancashire Heeler"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "McNab Shepherd"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Mixed-breed"] <- NA
rawPhase2$breedGroup[rawPhase2$breedF == "Nova Scotia Duck Tolling Retriever "] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Papillon "] <- "Toy"
rawPhase2$breedGroup[rawPhase2$breedF == "Pembroke Welsh Corgi "] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Pit Bull"] <- "Terrier"
rawPhase2$breedGroup[rawPhase2$breedF == "Poodle (Standard)"] <- "Non-Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Rhodesian Ridgeback"] <- "Hound"
rawPhase2$breedGroup[rawPhase2$breedF == "Rottweiler"] <- "Working"
rawPhase2$breedGroup[rawPhase2$breedF == "Schipperke "] <- "Non-Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Shetland Sheepdog"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Vizsla"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Welsh Springer Spaniel "] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "Weimaraner"] <- "Sporting"
rawPhase2$breedGroup[rawPhase2$breedF == "White Swiss Shepherd Dog"] <- "Herding"
rawPhase2$breedGroup[rawPhase2$breedF == "Wirehaired Pointing Griffon "] <- "Sporting"
table(rawPhase2$breedGroup, useNA="ifany")
rawPhase2$breedGroupF <- factor(rawPhase2$breedGroup)

### table(rawPhase2$breeddescription, useNA="ifany")
###  these are varied free text

table(rawPhase2$breedGroup, useNA="ifany")

### neutered
### variable from xlsx is "Neutered?" and the question mark causes problems with R. Rename

rawPhase2$neutered <- rawPhase2[ ,c("Neutered?")]
rawPhase2$neuteredF <- factor(rawPhase2$neutered)
table(rawPhase2$neuteredF, useNA="ifany")
### country
table(rawPhase2$Country)
rawPhase2$countryF <- factor(rawPhase2$Country)

### working status
table(rawPhase2$workingstatus, useNA="ifany")
rawPhase2$workingStatusF <- factor(rawPhase2$workingstatus,
                             levels=c("training", "working", "retired"))
### types of work
### dual purpose
table(rawPhase2$duallytrained, useNA="ifany")
rawPhase2$duallyTrainedF <- factor(rawPhase2$duallytrained,
                             level=c("no", "yes"))

### odor detection variables
 odorVars <- dplyr::select(rawPhase2, starts_with("odor"))
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
rawPhase2$duallyTrainedF <- factor(rawPhase2$duallytrained,
                                levels=c("no", "yes"))
### df with only dually trained dogs
rawPhase2Dual <- rawPhase2[rawPhase2$duallyTrainedF == "yes", ]
### remove unused levels of Breed that only occur in the single purpose dogs
rawPhase2Dual$breedF <- droplevels(rawPhase2Dual$breedF)
table(rawPhase2Dual$breedF)


certificationVars <- dplyr::select(rawPhase2, starts_with("cert"))
certificationVars$certificatedescription <- NULL

### convert to factors
### approach to converting all columns from
index <- 1:ncol(certificationVars)
certificationVars[ ,index] <- lapply(certificationVars[ ,index], as.factor)
sapply(certificationVars, class)
sapply(certificationVars, levels)
### make factors for some individual variables
table(rawPhase2$alerttype, useNA="ifany")
rawPhase2$alerttypeF <- factor(rawPhase2$alerttype)


table(rawPhase2$rewardsystem, useNA="ifany")
rawPhase2$rewardsystemF <- factor(rawPhase2$rewardsystem)

table(rawPhase2$dogshandledbefore, useNA="ifany")
rawPhase2$dogshandledbeforeF <- factor(rawPhase2$dogshandledbefore,
                                 levels=c("0", "1 to 2", "3 to 5", "more than 5"))

table(rawPhase2$dogshomelocation, useNA="ifany")
rawPhase2$dogshomelocationF <- factor(rawPhase2$dogshomelocation)


### keep ratings numeric
table(rawPhase2$scentrating, useNA="ifany")
table(rawPhase2$behaviorrating, useNA="ifany")
### remove responses that are out of bounds
class(rawPhase2$scentrating)
rawPhase2$scentrating[rawPhase2$scentrating > 10] <- NA
rawPhase2$behaviorrating[rawPhase2$behaviorrating > 10] <- NA

### sample (dna?)
table(rawPhase2$sampleprovided, useNA="ifany")
rawPhase2$sampleprovidedF <- factor(rawPhase2$sampleprovided)## DNA sample


### Change some CBARQ variable names for compatibility with detector dog + chasing version

rawPhase2 <- rawPhase2 %>%
    rename(
        MISC51 = DISTRACT51,
        MISC52 = DISTRACT52,
        MISC53 = DISTRACT53,
        MISC54 = DISTRACT54,
        MISC55 = DISTRACT55
    )
 


save(rawPhase2, file = "data/rawPhase2.Rda")
