library("readxl")
library(dplyr)
library(data.table)
install.packages("kableExtra")
library(kableExtra)

setwd("C:\\Users\\Travis Froberg\\Documents\\DPS Project")

scoreData <- read_excel("StudentScores.xlsx")
View(scoreData)

snapData <- read_excel("EducatorEffectivenessSnapshot.xlsx")
View(snapData)

snapData <- snapData[-(1:5),]
View(snapData)
names(snapData) <- snapData[1,]
snapData <- snapData[-c(1, 109),]

levels(factor(scoreData$ISDname))
levels(factor(scoreData$ISDcode))
levels(factor(scoreData$district_code))
levels(factor(scoreData$school_year))
levels(factor(scoreData$grade))

class(scoreData$average_scaled_score)
scoreData2 <- subset(scoreData, scoreData$average_scaled_score != "< 10")
View(scoreData2)
nrow(scoreData2)

levels(factor(scoreData$building_name))

loc_vector <- c(snapData$location)
build_name_vector <- c(scoreData2$building_name)
build_name_vector2 <- unique(build_name_vector)
build_name_vector2

mismatch_vector <- build_name_vector2[!(build_name_vector2 %in% loc_vector)]
mismatch_vector
remove <- which(scoreData2$building_name %in% mismatch_vector)
scoreData3 <- scoreData2[-remove,]
nrow(scoreData3)
View(scoreData3)

is.element("Burton International School", scoreData3$building_name)
is.element("Pulaski Elementary-Middle School", scoreData3$building_name)

# 1 
tempData <- subset(scoreData3, subgroup == "All Students" & subject_name == "Mathematics") 
View(tempData)

tempData2 <- aggregate(x = as.numeric(tempData$percent_proficient), 
          by = list(tempData$building_name),
          FUN = mean)


mean.prof.df <- tempData2 %>% arrange(desc(tempData2$x)) 
colnames(mean.prof.df) <- c("School", "Average Math Proficiency Rate")
nrow(mean.prof.df)

max(tempData2$x)
nrow(tempData2)

table.math.prof <- mean.prof.df[1:10,]
table.math.prof %>%
  kbl() %>%
  kable_minimal() %>%
  add_header_above(c("Top Ten Schools in Math Proficiency for Grades 3-8 in Detroit City School District (2013-2014)",""))

# 2 
snapData2 <- snapData[-c(1,2),]

snapData3 <- snapData2[match(mean.prof.df$School, snapData2$location, nomatch = 0),]

nrow(snapData3)
nrow(mean.prof.df)
View(snapData3)

snapData3 <- snapData3[order(snapData3$location),]
scoreData4 <- mean.prof.df[order(mean.prof.df$School),]
View(scoreData4)
nrow(scoreData4)

snapData3$location == scoreData4$School

mergedData <- data.frame(snapData3, scoreData4)
View(mergedData)


