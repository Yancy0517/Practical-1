#1
# Team member information:
# s2404675 Heyu Nie
# s2281875 Jialong He
# s2317223 Linsheng Shu

#2
# Step 4 was done by  Heyu Nie alone 
# Step 7 was done by Jialong He alone 
# step 10 was done by Linsheng Shu alone 
# The rest of the steps were written individually. At the end, the group carried out a comparison and discussion of each person's code to find the best approach.

#3
setwd("C:\\Users\\niehe\\Desktop\\S.P")
a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)]
a <- a[-grep("[0123456789]:[0123456789]", a)] ## Strip out verse numbers

#4
# Split_punct function can separate the punctuation marks from words
split_punct <- function(a, punct){
  bible <- a 
  bible <- gsub("[()]", "", bible) # Remove "(" and ")" .
  ii <- grep(punct, bible) # Position of words with a punctuation mark
  split_p<- rep ("", length(ii)+ length(bible)) # New vector to store words and punctuation mark
  iis <- ii+1:length(ii) # new position of punctuation mark.
  split_p[iis]<- substr(bible[ii],nchar(bible[ii]),nchar(bible[ii])) # Add punctuation mark to new vetor
  bible<- gsub(punct,"" , bible) # Remove punctuation marks from words
  split_p[-iis] <- substr(bible,0,nchar(bible[1:length(bible)])) # Add words to new vector
  return(split_p) # Return final vector.
}
#5 
after_split <- split_punct(a, "[,.?!:;]") # Separate the punctuation marks