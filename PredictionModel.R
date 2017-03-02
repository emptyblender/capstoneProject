##Open files

setwd("C:/Users/trevo/Desktop/datascience/Capstone/final/en_US")
freq1<-read.table("freq1.txt")
freq2<-read.table("freq2.txt")
freq3<-read.table("freq3.txt")
freq4<-read.table("freq4.txt")


#Separating tables into individual words
library(tidyr)

quad<-freq4
quad<-separate(quad,quadgram,c("first","second","third","fourth"), " ")

tri<- freq3
tri<-separate(tri,trigram,c("first","second","third"), " ")


bi<- freq2
bi<-separate(bi,bigram,c("first","second"), " ")



#reorder tables by occurrence of similar words
library(dplyr)

quad<-arrange(quad,first,second,third)
tri<-arrange(tri,first,second)
bi<-arrange(bi,first)

#rename unigram table
uni<- freq1


#PREDICTION


## Katz Back-Off Model
prediction<- function(input) {
  
  library(tm)
  library(stringr)
  
  #clean input
  input<-removeNumbers(input)
  input<-removePunctuation(input)
  input<-tolower(input)
  input<-stripWhitespace(input)
  
  inputLength<-sapply(strsplit(input, "\\s+"), length)
  

  
  if (inputLength == 0) {
  
  print("Enter a phrase.")
  }
  
  else if (inputLength == 1) {
    # Start with bi grams
    matches2<-bi[input==bi$first,]
    count2<-sum(matches2$frequency)
    d<-1
    
    matches2<-mutate(matches2,q=(frequency-d)/count)
    alpha<-1-sum(matches2$q)
    
    matches2Final<-matches2[1:3,2:4]
    names(matches2Final)[names(matches2Final) == "second"] <- "word"
    
    # Back off to uni grams
    matches1<-uni[1:100,]
    matches1<-matches1[,c("word","frequency")]
    count1<-sum(matches1$frequency)
    
    
    matches1<-mutate(matches1,q=alpha*frequency/count1)
    matches1Final<-matches1[1:3,1:3]
    
    matchesFinal<-rbind(matches2Final,matches1Final)
    matchesFinal<-arrange(matchesFinal,desc(q))
    matchesFinal<-unique(matchesFinal$word)
    print(matchesFinal[1:3])
    
  }
  
  else if (inputLength == 2) {
    #start with trigrams
    input3<-word(input,-2:-1)
    
    ##create new DF with only the search words and apply KBO method
    matches3<-tri[tri$first==input3[[1]]&tri$second==input3[[2]],]
    count3<-sum(matches3$frequency)
    d<-1
    matches3<-mutate(matches3,q=(frequency-d)/count3)
    alpha3<-1-sum(matches3$q)
    
    #pick top three answers with trigram
    matches3Final<-matches3[1:3,3:5]
    names(matches3Final)[names(matches3Final) == "third"] <- "word"
    
    
    #Back off to bigrams
    input2<-word(input,-1)
    
    matches2<-bi[input2==bi$first,]
    count2<-sum(matches2$frequency)
    d<-1
    matches2<-mutate(matches2,q=alpha3*(frequency-d)/count2)  
    alpha2<-1-sum(matches2$q)-sum(matches3$q)
    
    matches2Final<-matches2[1:3,2:4]
    names(matches2Final)[names(matches2Final) == "second"] <- "word"
    
    matchesFinal<-rbind(matches3Final,matches2Final)
    
    # Back off to unigrams
    matches1<-uni[1:100,]
    matches1<-matches1[,c("word","frequency")]
    count1<-sum(matches1$frequency)
    
    
    matches1<-mutate(matches1,q=alpha2*frequency/count1)
    matches1Final<-matches1[1:3,1:3]
    
    matchesFinal<-rbind(matchesFinal,matches1Final)
    matchesFinal<-arrange(matchesFinal,desc(q))
    matchesFinal<-unique(matchesFinal$word)
    print(matchesFinal[1:3])
    
  }
    
    else {
      input4<-word(input,-3:-1)
      
      ##create new DF with only the search words and apply Katz Back Off method
      matches4<-quad[quad$first==input4[[1]]&quad$second==input4[[2]]&quad$third==input4[[3]],]
      count4<-sum(matches4$frequency)
      d<-1
      matches4<-mutate(matches4,q=(frequency-d)/count4)
      alpha4<-1-sum(matches4$q)
      
      #pick top three answers quadgram
      matches4Final<-matches4[1:3,4:6]
      names(matches4Final)[names(matches4Final) == "fourth"] <- "word"
      
      ##Back off to trigram
      input3<-word(input,-2:-1)
      
      matches3<-tri[tri$first==input3[[1]]&tri$second==input3[[2]],]
      count3<-sum(matches3$frequency)
      d<-1
      matches3<-mutate(matches3,q=alpha4*(frequency-d)/count3)
      alpha3<-1-sum(matches3$q)-sum(matches4$q)
      
      #pick top three answers with trigram
      matches3Final<-matches3[1:3,3:5]
      names(matches3Final)[names(matches3Final) == "third"] <- "word"
      matchesFinal<-rbind(matches4Final,matches3Final)
      
      
      #Back off to bigrams
      input2<-word(input,-1)
      
      matches2<-bi[input2==bi$first,]
      count2<-sum(matches2$frequency)
      d<-1
      matches2<-mutate(matches2,q=alpha3*(frequency-d)/count2)  
      alpha2<-1-sum(matches2$q)-sum(matches3$q)
      
      matches2Final<-matches2[1:3,2:4]
      names(matches2Final)[names(matches2Final) == "second"] <- "word"
      
      matchesFinal<-rbind(matchesFinal,matches2Final)
      
      # Back off to unigrams
      matches1<-uni[1:100,]
      matches1<-matches1[,c("word","frequency")]
      count1<-sum(matches1$frequency)
      
      
      matches1<-mutate(matches1,q=alpha2*frequency/count1)
      matches1Final<-matches1[1:3,1:3]
      
      matchesFinal<-rbind(matchesFinal,matches1Final)
      matchesFinal<-arrange(matchesFinal,desc(q))
      matchesFinal<-unique(matchesFinal$word)
      print(matchesFinal[1:3])
      
      }
      
      
  }

