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

#6
lowerb <- tolower(after_split) # Lowercase the upper case text in the data
unique_word <- unique(lowerb) # Find the unique words in the "lowerb"
indicies <- match(lowerb,unique_word) # Position of each unique word 
amount <- tabulate(indicies) # Counting the number of each unique word.
lowerp <- order(amount,decreasing = TRUE) # Sort the occurrences of each unique word in "amount" from most to least (The output is the location)
b <- unique_word[lowerp[1:500]] # The 500 most frequently occurring words

#10 We insert the code needed in step 10 here
sortb <- sort(amount, decreasing = TRUE) # Sort the occurrences of each unique word in "amount" from most to least (The output is the number of occurrences)
number_of_occurrences <- sortb[1:500] # Number of occurrences corresponding to the 500 most frequently occurring words
bm <- cbind(b,number_of_occurrences) # Create a matrix of "b" and their corresponding numbers

capitalp <- grep("[A-Z]", after_split) #Find the positions of the capitalized word
capital <- after_split[capitalp] # Capital words
unique_capital <- unique(capital) # unique capitalized words
lower_unique_capital <- tolower(unique_capital) # Lowercase all words in "unique_capital"
position_in_b <- match(lower_unique_capital, b) # match "lower_unique_capital" and "b"
de <- which(is.na(position_in_b)) #Position of all output NA in "position_in_b"
unique_capital<- unique_capital[-de] # Find words in the Bible that appear with capital letters and are also in "b".
position_in_as <- match(after_split, unique_capital) # Match after_split and unique_capital
amount_of_capital <- tabulate(position_in_as) # Counting the number 
cm <- cbind(unique_capital,amount_of_capital) # Create a matrix of uppercase words and their corresponding numbers

#7
# probability of Three-dimensional arrays: T 
p <- match(after_split, b) # match "after_split" and "b"
c1 <- p[-(length(p)-1):-length(p)] # Delete the last two data in "p"
c2 <- p[-1] # Delete the first data in "p"
c2 <- c2[-length(c2)] #delete the last data in ("p"/old "c2")
c3 <- p[-1:-2] # Delete the first two data in "p"
M3 <- cbind(c1,c2,c3) # Make a matrix 
rs <-rowSums(M3) # Summation of each row of the matrix
del_rows <- which(is.na(rs)) # Find which rows has NA
M3 <- M3[-del_rows,] # Remove rows with NA
T <- array( data=0, dim = c(500,500,500)) # Build T
for(i in 1:500){
  t<- which(M3[,1]==i) 
  if(length(t)!=0){
    for(k in 1:500){
      q <- which(M3[t,2]==k)
      if(length(q)!=0){
        for(j in 1:500){
          v <- which(M3[q,3]==j)
          if(length(v)!=0){
            T[i,k,j]=length(v)/length(q) # Probability of getting each 3 words combinations
            
          }
        }
      }
    }
  }
}

# Single probability: S
del_NA <- p[-(which(is.na(p)))] # remove NA
S <- tabulate(del_NA)/length(del_NA) # Probability of each word 


# Two-dimensional array probabilities: A
c21 <- p[-length(p)] # delete the last data in "P"
c22 <- p[-1] # delete the first data in "p"

M2 <- cbind(c21,c22) # make a matrix
del_rows2 <- is.na(rowSums(M2)) # find 
M2 <- M2[-which(del_rows2),]  # Remove rows with NA
A <- array(data = 0, c(500,500)) # Build A
for(i in 1:500){
  m <- which(M2[,"c21"]==i)
  if(length(m)!=0){
    for(j in 1:500){
      n <- which(M2[m,"c22"]==j)
      if(length(n)!=0){
        A[i,j]=length(n)/length(m) # Probability of getting each 2 words combinations
      }
    }
  }
} 

#8
print_txt_position <- c() # build a set
# Create a for loop and use S,A,T in the loop to simulate the random generation of 50 words
for(i in 1:50){
  if(i==1){                                                       # First word could only use S
    print_txt_position[i] <-sample(1:500, 1,rep = TRUE, prob = S)
  }
  if(i==2){                                                       # second word could only use A or S
    if(sum(A[print_txt_position[i-1],])!=0){
      print_txt_position[i] <- sample(1:500, 1, replace = TRUE, prob = A[print_txt_position[i-1],]) 
    }
    else{
      print_txt_position[i] <- sample(1:500, 1, replace = TRUE, prob = S)
    }
  }
  if(i>2){                                                        # From now on consider the three cases S,A,T separately
    if(sum(T[print_txt_position[i-2], print_txt_position[i-1],])!=0){
      print_txt_position[i] <- sample(1:500, 1, replace = TRUE, prob = T[print_txt_position[i-2],print_txt_position[i-1],])
    }else if(sum(A[print_txt_position[i-1],])!=0){
      print_txt_position[i] <- sample(1:500, 1, replace = TRUE, prob = A[print_txt_position[i-1],]) 
    }else{
      print_txt_position[i] <- sample(1:500, 1, replace = TRUE, prob = S)
    }
  }
}
cat(b[print_txt_position]) 

#9 
# Simulate 50 word sections of text where the word probabilities are simply taken from S.
cat(b[sample(1:500, 50, replace= TRUE, prob=S)]) 