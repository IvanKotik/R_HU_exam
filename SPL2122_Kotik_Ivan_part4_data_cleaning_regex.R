################################################################################
################## Statistical Programming Languages 2021/22 ###################
##################               Take-home Exam              ###################
##################                                           ###################
##################   	  Sigbert Klinke, Eva-Maria Maier,   ###################
##################       		  Alexander Volkmann         ###################
################################################################################

#-------------------------------------------------------------------------------
# Surname: Kotik
# Name: Ivan
#-------------------------------------------------------------------------------

### Exercise 5 -----------------------------------------------------------------

nick <- read.csv("C://Users//qwerty//Downloads//bavarian_nicknames_fem.csv",
                 encoding = "UTF-8",
                 sep = ":", header = FALSE, strip.white=TRUE)  # +getting rid of the spaces

colnames(nick) <- c("Nickname", "Name")  # renaming the columns
nick <- nick[!apply(nick == "", 1, all),]  # deleting empty columns

# Deleting the special characters and writing out the nickname combinations in $Nickname
for(i in 1:nrow(nick)){

    # pattern with ...(...)... and ...-.../...
    if (grepl("\\S+\\(\\S+\\)\\S*-\\S+\\/\\S+\\b", nick[i, 1]) == TRUE){
      # part 1: picks the part before the start and "(", (start;A)
      p1 <- regmatches(nick[i, 1], regexpr("\\S+(?=\\()", nick[i ,1], perl = TRUE))
      # part 2: pick the part between the "(" and the ")", (A;B)
      p2 <- regmatches(nick[i, 1], regexpr("(?<=\\()\\S+(?=\\))", nick[i ,1], perl = TRUE))
      # part 3: pick the part between the ")" and the "-", (B;C)
      p3 <- regmatches(nick[i, 1], regexpr("(?<=\\))\\S+(?=\\-)", nick[i ,1], perl = TRUE))
      # part 4: pick the part between the "-" and the "/", (C;D)
      p4 <- regmatches(nick[i, 1], regexpr("(?<=\\-)\\S+(?=\\/)", nick[i ,1], perl = TRUE))
      # part 5: pick the part between the "/" and the end, (D;end)
      p5 <- regmatches(nick[i, 1], regexpr("(?<=\\/)\\S+\\b", nick[i ,1], perl = TRUE))
      c1 <- paste(p1, p2, p3, p4, sep = "")
      c2 <- paste(p1, p2, p3, p5, sep = "")
      c3 <- paste(p1, p3, p4, sep = "")
      c4 <- paste(p1, p3, p5, sep = "")
      nick[i, 1] <- gsub("\\S+\\(\\S+\\)\\S*-\\S+\\/\\S+\\b", paste(c1, c2, c3, c4, sep = ", "),  nick[i, 1])
    }

    # pattern with only ...(...)...
    if (grepl("\\S+\\(\\S+\\)\\S*(?=,|\"|\b|$)", nick[i, 1], perl = TRUE) == TRUE){
      # part 1: picks the part before the start and "(", (start;A)
      p1 <- regmatches(nick[i, 1], regexpr("\\S*(?=\\()", nick[i ,1], perl = TRUE))
      # part 2: pick the part between the "(" and the ")", (A;B)
      p2 <- regmatches(nick[i, 1], regexpr("(?<=\\()\\S+(?=\\))", nick[i ,1], perl = TRUE))
      # part 3: pick the part between the ")" and the "-", (B;C)
      p3 <- regmatches(nick[i, 1], regexpr("(?<=\\))\\S*(?=,|\"|\\b)", nick[i ,1], perl = TRUE))
      c1 <- paste(p1, p2, p3, sep = "")
      c2 <- paste(p1, p3, sep = "")
      nick[i, 1] <- gsub("\\S+\\(\\S+\\)\\S*(?=,|\"|\\b|$)", paste(c1, c2, sep = ", "), nick[i, 1], perl = TRUE)
    }

    # pattern with only ...-.../...
    if (grepl("\\S+-\\S+\\/\\S+\\b", nick[i, 1], perl = TRUE) == TRUE){
      # part 3: pick the part between the start and the "-", (start;C)
      p3 <- regmatches(nick[i, 1], regexpr("\\S+(?=\\-)", nick[i ,1], perl = TRUE))
      # part 4: pick the part between the "-" and the "/", (C;D)
      p4 <- regmatches(nick[i, 1], regexpr("(?<=\\-)\\S+(?=\\/)", nick[i ,1], perl = TRUE))
      # part 5: pick the part between the "/" and the end, (D;end)
      p5 <- regmatches(nick[i, 1], regexpr("(?<=\\/)\\S+\\b", nick[i ,1], perl = TRUE))
      c1 <- paste(p3, p4, sep = "")
      c2 <- paste(p3, p5, sep = "")
      nick[i, 1] <- gsub("\\S+-\\S+\\/\\S+\\b", paste(c1, c2, sep = ", "),  nick[i, 1], perl = TRUE)
    }

    # pattern with .../...
    if (grepl("\\/", nick[i, 1]) == TRUE) {
      nick[i, 1] <- gsub("\\/", ", ",  nick[i, 1], perl = TRUE)
    }
}

# Deleting "[...]"'s in $Name
for (i in 1:nrow(nick)){
  if (grepl(".(?<=\\[).*(?=\\]).", nick[i, 2], perl = TRUE) == TRUE)
  nick[i, 2] <- gsub(".(?<=\\[).*(?=\\]).", " ",  nick[i, 2], perl = TRUE)
}

###########################################

# Reshaping the dataframe to have unique nicknames from $Nickname as id's and aggregate Name's
nick <- stack(setNames(strsplit(nick$Name,", "), nick$Nickname))  # separate Names
nick$ind <- as.character(nick$ind)  # de-factorize
nick <- stack(setNames(strsplit(nick$ind,", "), nick$values))  # separate nicknames
nick$ind <- as.character(nick$ind)  # de-factorize
nick <- aggregate(nick$ind ~ nick$values, data = nick, paste, collapse = ", ")  # finilize the dataframe
colnames(nick) <- c("Nicknames", "Names")  # give propper column names