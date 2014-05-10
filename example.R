# import algorithm
source("./lib/talp.R")

# ageing function
ageing <- function(date1, date2="2010-12-01"){
	diff <- as.numeric(difftime(date2, date1, unit="weeks"))
	if(diff>0){
		value <- 1*exp(-0.01*diff)
	} else {
		value <- 0
	}
	return(value)
}

# define link types
linkTypes <- c("type", "knows", "creator", "assignedTag", "assignedCategory", "usedAPI", "supportedProtocol", "supportedFormat")

# define entities
entityTypes <- c("/type/Person", "/type/Mashup", "/type/Service", "/type/Category", "/type/Tag", "/type/Format", "/type/Protocol")
persons <-c("/person/Duvander", "/person/Andres")
mashups <-c("/mashup/JungleThingy")
services <- c("/service/Google-Maps-API")
formats <- c("/format/XML")
protocols <- c("/protocol/JavaScript")
categories <- c("/category/Mapping")
tags <- c("/tag/travel", "/tag/mapping", "/tag/places")
entities <- c(entityTypes, persons, mashups, services, formats, protocols, categories, tags)

# initialize tensor
t <- initializeTensor(entities, linkTypes)

# add links: source, link type, target, value of link ; 1 for "strong" links, otherwise decreased by ageing
setTensorValue(t,"/person/Duvander","type","/type/Person",1)
setTensorValue(t,"/person/Andres","type","/type/Person",1)
setTensorValue(t,"/person/Andres","knows","/person/Duvander",ageing("2008-06-26"))
setTensorValue(t,"/person/Duvander","knows","/person/Andres",ageing("2009-01-13"))

setTensorValue(t,"/mashup/JungleThingy","type","/type/Mashup",1)
setTensorValue(t,"/mashup/JungleThingy","creator","/person/Andres",ageing("2009-03-07"))
setTensorValue(t,"/mashup/JungleThingy","assignedTag","/tag/travel",ageing("2009-03-07"))
setTensorValue(t,"/mashup/JungleThingy","assignedTag","/tag/mapping",ageing("2009-03-07"))
setTensorValue(t,"/mashup/JungleThingy","usedAPI","/service/Google-Maps-API",ageing("2009-03-07"))

setTensorValue(t,"/service/Google-Maps-API","type","/type/Service",1)
setTensorValue(t,"/service/Google-Maps-API","assignedTag","/tag/mapping",ageing("2005-12-05"))
setTensorValue(t,"/service/Google-Maps-API","assignedTag","/tag/places",ageing("2005-12-05"))
setTensorValue(t,"/service/Google-Maps-API","assignedCategory","/category/Mapping",ageing("2005-12-05"))
setTensorValue(t,"/service/Google-Maps-API","supportedFormat","/format/XML",ageing("2005-12-05"))
setTensorValue(t,"/service/Google-Maps-API","supportedProtocol","/protocol/JavaScript",ageing("2005-12-05"))

setTensorValue(t,"/category/Mapping","type","/type/Category",1)
setTensorValue(t,"/tag/mapping","type","/type/Tag",1)
setTensorValue(t,"/tag/travel","type","/type/Tag",1)
setTensorValue(t,"/tag/places","type","/type/Tag",1)
setTensorValue(t,"/protocol/JavaScript","type","/type/Protocol",1)
setTensorValue(t,"/format/XML","type","/type/Format",1)

# print original tensor
printTensor(t)

# factorization, 10 latent factors, max 100 iterations and difference between iterations 0.01
factorizationOutput <- factorization(t, 10, 100, 0.01)

# predict links
predictedLinks <- topNtargets("/service/Google-Maps-API","assignedTag", t, factorizationOutput)
print(predictedLinks[grep("/tag/",predictedLinks[,1]),])
# output --> /tag/travel
