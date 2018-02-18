#### Book link here : http://www.matthewjockers.net/text-analysis-with-r-for-students-of-literature/ ####

          #####Chapter 2#####

#read the file 
#sep argument below with \n breaks up the text where there are new lines
text.v <- scan("data/plainText/melville.txt",what="character",sep="\n")

#text.v <- scan("http://www.gutenberg.org/cache/epub/2701/pg2701.txt",what="character",sep="\n")

#View entire text of Moby Dick
text.v          

#View contents of first item in text
text.v[1]
text.v[2]
text.v[408]

#returns line number where the given text is present
start.v <- which(text.v =="CHAPTER 1. Loomings.")
end.v <- which(text.v == "orphan.")

#returns number of lines in text
length(text.v)

#Store text before first line of novel          
start.metadata.v <- text.v[1:start.v - 1]

#Store text after last line of novel
end.metadata.v <- text.v[(end.v+1):length(text.v)]

#Combine the above both metadata
metadata.v <- c(start.metadata.v,end.metadata.v)
metadata.v <- c(text.v[1:start.v-1],text.v[(end.v+1):length(text.v)])

#Extract the lines containing the novel
novel.lines.v <- text.v[start.v:end.v]

length(text.v)
length(novel.lines.v)

#join all the lines - glue them using the space (collapse function)
novel.v <- paste(novel.lines.v,collapse=" ")
          
length(novel.v)

#convert the entire text to lower case
novel.lower.v <- tolower(novel.v)

#Extract the words and organize them into a list - using strsplit
#\\W is a 'regular expression' and matches any non-word character
moby.words.1 <- strsplit(novel.lower.v,"\\W")

#Identify class of the datatype
class(novel.lower.v)
class(moby.words.1)

#More details about the list
str(moby.words.1)

#Create a vector again instead of a list (We don't need a list in this problem)
moby.word.v <- unlist(moby.words.1)

#identify items which are not blank
not.blanks.v <-which(moby.word.v!="")

#overwrite the data with the non-blank entries
moby.word.v <- moby.word.v[not.blanks.v]

#Find words in specific positions
moby.word.v[c(4,5,6)]

#Find index based on content of the vector
which(moby.word.v=='whale')

#Number of times the word 'whale' appears
length(moby.word.v[which(moby.word.v=="whale")])

#Percentage of 'whale' occurences in the novel
length(moby.word.v[which(moby.word.v=='whale')])/length(moby.word.v)

#Number of unique words in the novel
length(unique(moby.word.v))

#How often does he use each of the words?
moby.freqs.t <- table(moby.word.v)
sorted.moby.freqs.t <- sort(moby.freqs.t, decreasing=TRUE)

sorted.moby.freqs.t[1:10]

#plot the words and frequency
plot(sorted.moby.freqs.t[1:10],type="b",xlab="Top ten words",
     ylab="Word Count",xaxt="n")
axis(1,1:10,labels=names(sorted.moby.freqs.t[1:10]))

          #####Chapter 3#####
          
#compare words he,she, him,her    
sorted.moby.freqs.t["he"]
sorted.moby.freqs.t["she"]
sorted.moby.freqs.t["him"]
sorted.moby.freqs.t["her"]

#With the data in table object - we can use both numerical and named indexing
sorted.moby.freqs.t[1]
sorted.moby.freqs.t["the"]

#how frequent is he as compared to she?
sorted.moby.freqs.t["he"]/sorted.moby.freqs.t["she"]

#convert the word counts into relative frequency percentages - provides a better picture
sorted.moby.rel.freqs.t <- 100*(sorted.moby.freqs.t/sum(sorted.moby.freqs.t))

#export it as a csv file
write.csv(sorted.moby.rel.freqs.t,'abc.csv',row.names=TRUE)

#number of occurences as a percentage
sorted.moby.rel.freqs.t[1]
sorted.moby.rel.freqs.t["the"]
          
#plot the relative frequency of words
plot(sorted.moby.rel.freqs.t[1:10],type='b',xlab="Top Ten Words",
     ylab="Percentage of Full Text",xaxt="n")
axis(1,1:10,labels=names(sorted.moby.rel.freqs.t[1:10]))
          
          ##### Chapter 4 #####

#sequence of 10 numbers
seq(1:10)

#Now, create a sequence of numbers for all words in the book
n.time.v <- seq(1:length(moby.word.v))

#Locate occurence of whale in the novel
whales.v <- which(moby.word.v == "whale")

#repeat NA as many times as there are items in w.count.v
w.count.v <- rep(NA, length(n.time.v))

#replace NA values to 1 where a 'whale' was found
w.count.v[whales.v] <- 1

#Dispersion plot showing distribution of the word 'whale'
plot(w.count.v,main="Dispersion Plot of 'Whale' in Moby Dick",
     xlab="Novel Time",ylab = "whale",type ="h",ylim=c(0,1),yaxt='n')          

ahabs.v <- which(moby.word.v == "ahab")
          
#repeat NA as many times as there are items in w.count.v
w.count.v <- rep(NA, length(n.time.v))
          
#replace NA values to 1 where a 'ahab' was found
w.count.v[ahabs.v] <- 1
          
#Dispersion plot showing distribution of the word 'ahab'
plot(w.count.v,main="Dispersion Plot of 'ahab' in Moby Dick",
xlab="Novel Time",ylab = "ahab",type ="h",ylim=c(0,1),yaxt='n')          

###When whale is mostly present, the word ahab decreases

#create a fresh session
rm(list=ls())

text.v <- scan("data/plainText/melville.txt",what="character",sep="\n")
start.v <- which(text.v =="CHAPTER 1. Loomings.")
end.v <- which(text.v == "orphan.")
novel.lines.v <- text.v[start.v:end.v]

#Identify positions of chapter using grep function
chap.positions.v <- grep("^CHAPTER \\d",novel.lines.v)

#Add an additional word to the end of the novel and add it to chapter positions
#This is done to make sure we can split chapters by using a uniform logic -

novel.lines.v <- c(novel.lines.v,"END")
last.position.v <- length(novel.lines.v)
chap.positions.v <-c(chap.positions.v,last.position.v)

#print chapter positions 1 by 1 using for loop
for(i in 1:length(chap.positions.v))
    {print(chap.positions.v[i])}

for(i in 1:length(chap.positions.v))
    {print(paste("Chapter ",i, " begins at position ",chap.positions.v[i],
    sep=""))}

#For loop to perform the previous operations chapter-wise

chapter.raws.l <- list()
chapter.freqs.l <- list()
          
for(i in 1:length(chap.positions.v)){
  if(i != length(chap.positions.v)){
    chapter.title <- novel.lines.v[chap.positions.v[i]]
    start <- chap.positions.v[i] + 1
    end <- chap.positions.v[i+1] - 1
    chapter.lines.v <- novel.lines.v[start:end]
    chapter.words.v <- tolower(paste(chapter.lines.v,collapse=" "))
    chapter.words.l <- strsplit(chapter.words.v,"\\W")
    chapter.word.v <- unlist(chapter.words.l)
    chapter.word.v <- chapter.word.v[which(chapter.word.v!="")]
    chapter.freqs.t <- table(chapter.word.v)
    chapter.raws.l[[chapter.title]] <- chapter.freqs.t
    chapter.freqs.t.rel <- 100*(chapter.freqs.t/sum(chapter.freqs.t))
    chapter.freqs.l[[chapter.title]] <- chapter.freqs.t.rel
  }
}

#rbind example (With different size vectors, recycling occurs)
x <- c(1,2,3,4,5,7)
y <- c(2,3,4,5,6)
z <- rbind(x,y)

#In below example, the first element in x is multiplied by the first element in y
#and 2nd by 2nd and 3rd by 1st and 4th by 2nd and so on 
x<-c(1,2,3,4,5)
y<-c(1,2)
z<-x*y

#lapply example
x<-list(a=1:10,b=2:25,b=100:1090)

#provides mean of the 3 different integer objects 
lapply(x,mean)

#relative frequency of word "whale" in 1st chapter
chapter.freqs.l[[1]]["whale"]

#lapply does the above job for all the objects in the list
whale.l <- lapply(chapter.freqs.l,'[','whale')

#Do.call (Do dot call) example
#Using do.call in the below example binds the contents of each list row-wise
x <- list(1:3,4:6,7:9)
do.call(rbind,x)

whales.m <- do.call(rbind,whale.l)

ahab.l <- lapply(chapter.freqs.l,'[','ahab')
ahabs.m <- do.call(rbind,ahab.l)

#cbind example
x <- c(1,2,3,4,5,6)
y <- c(2,4,5,6,7,8)

z <- c(24,23,34,32,12,10)

test.m <- cbind(x,y,z)
test.m[2,3]
test.m[2,]

whales.v <- whales.m[,1]
ahabs.v <- ahabs.m[,1]

whales.ahabs.m <- cbind(whales.v,ahabs.v)
dim(whales.ahabs.m)

colnames(whales.ahabs.m)
colnames(whales.ahabs.m) <- c("whale","ahab")

barplot(whales.ahabs.m,beside=T,col="grey")

          #####Chapter 5#####

#For performing correlation analysis, we need to replace NA's with 0
whales.ahabs.m[which(is.na(whales.ahabs.m))] <- 0

#Find correlation using cor function
cor(whales.ahabs.m)

#Since the above code provides a lot of extraneous information, 
#we can use specify the columns for whih we need the correlation

mycor <- cor(whales.ahabs.m[,"whale"],whales.ahabs.m[,"ahab"])

##matrix vs data.frame
x <-  matrix(1,3,3)
#Once we introduce a character in the matrix all the numeric values get converted to a character
x[2,2] <- "check"

#Now create a dataframe from the above created matrix
x <- matrix(1,3,3)
x.df <- as.data.frame(x)

x.df[1,2] <- "check"
class(x.df[1,1])
class(x.df[1,2])

#Access a column in a dataframe? 3 ways to do the same
x.df[,2]
x.df["V2"]
x.df$V2

#Now convert the whales.ahabs.m file into a dataframe
cor.data.df <- as.data.frame(whales.ahabs.m)

cor(cor.data.df)

#To check if the correlation is random, we need to shuffle the order of any one column
#If the shuffled dataset also provides a similar correlation, then there may not be a significant relation

sample(cor.data.df$whale)
#If we run the ^sample code multiple times, we see that the values are not in the same order everytime

cor(sample(cor.data.df$whale),cor.data.df$ahab)

#Instead of running it multiple times, we can use a for loop to do that
mycors.v <- NULL
for(i in 1:10000)
{
mycors.v <- c(mycors.v, cor(sample(cor.data.df$whale),cor.data.df$ahab))  
}

#Summary of the 10000 correlation numbers
summary(mycors.v)

#Histogram of random correlation coefficients
h <- hist(mycors.v, breaks=100, col="grey",
          xlab="Correlation Coefficient",
          main="Histogram of Random Correlation Coefficients\n with normal curve",
          plot=T)
xfit <- seq(min(mycors.v),max(mycors.v),length=1000)
yfit <- dnorm(xfit,mean=mean(mycors.v),sd=sd(mycors.v))
yfit <- yfit*diff(h$mids[1:2])*length(mycors.v)
lines(xfit,yfit,col="black",lwd=2)

#Another way to test the significance of correlation
cor.test(cor.data.df$whale,cor.data.df$ahab,method="pearson")

          #####Chapter 6#####

length(chapter.raws.l)
names(chapter.raws.l)

#Any item in a list can be examined individually
class(chapter.raws.l$`CHAPTER 1. Loomings.`)
class(chapter.raws.l[[1]])

#Calculate the mean word frequency
sum(chapter.raws.l[[1]])/length(chapter.raws.l[[1]])
mean(chapter.raws.l[[1]])

#Use lapply to calculate mean for all the chapters
lapply(chapter.raws.l,mean)

#bind the results (to plot later) using do.call
mean.word.use.m <- do.call(rbind, lapply(chapter.raws.l,mean))

#dimensions of the resultant matrix
dim(mean.word.use.m)

#Where the bar is low, individual word types are used less often
plot(mean.word.use.m,type="h")

#scale the values for easier understanding
plot(scale(mean.word.use.m),type="h")

#Order - When means are sorted in decreasing order, 
#Chapter 54 ranks 1  
order(mean.word.use.m,decreasing = T)

#Calculate Type Token Ratio(TTR) to assess lexical richness
length(chapter.raws.l[[1]])/sum(chapter.raws.l[[1]])*100

#Since there is no function for TTR, we can create a custom function
ttr.l <-  lapply(chapter.raws.l, function(x) {length(x)/sum(x) *100})
ttr.m <- do.call(rbind,ttr.l)

ttr.m[order(ttr.m,decreasing=TRUE),]

plot(ttr.m, type="h")

          #####Chapter 7#####

#Words that appear once are called as singletons or one-zies, but more formally
#called hapax legomena

#Count the number of hapax in each chapter
chapter.hapax.v <- sapply(chapter.raws.l, function(x) sum(x == 1))

#Total number of words ine ach chapter
chapter.lengths.m <- do.call(rbind, lapply(chapter.raws.l,sum))

#Find hapax percentage
hapax.percentage <- chapter.hapax.v/chapter.lengths.m

#Plot the hapax percentage as a bar plot
barplot(hapax.percentage,beside=T,col="grey",
        names.arg = seq(1:length(chapter.raws.l)))

#Correlation between chapter length and number of hapax
cor(chapter.hapax.v,chapter.lengths.m)

#We can conclude that the with increase in number of hapax, the length of the 
#chapter increases

          ##### Chapter 8 #####

#KWIC (Key Word in Context searches)

#Load all the .txt files from the required directory
setwd("C:\\Users\\rohan\\Desktop\\Virginia Tech\\Semester 2\\Library\\TextAnalysisWithR")

input.dir <- "data/plainText"
files.v <- dir(input.dir, "\\.txt$")

#Use a function to print vector of file names in a readable format
show.files <- function(file.name.v){
  for(i in 1:length(file.name.v)){
    cat(i,file.name.v[i],"\n",sep=" ")
    } 
}

#Send the fies.v object to the show.files function and see the numbered result
show.files(files.v)

#Function takes a vector of file names and a directory path and 
#returns a list in which each item in the list is an 
#ordered vector of words from one of the files in the vector of file names

make.file.word.v.l <- function(files.v,input.dir){
  #set up an empty container
  text.word.vector.l <- list()
  #loop over the files
  for(i in 1:length(files.v)){
    #read the file - we need to know the input directory
    text.v <- scan(paste(input.dir,files.v[i],sep="/"),
                   what="character",sep="\n")
    #convert to single string
    text.v <- paste(text.v,collapse = " ")
    #lowercase and split on non-word characters
    text.lower.v <- tolower(text.v)
    text.words.v <- strsplit(text.lower.v, "\\W")
    text.words.v <- unlist(text.words.v)
    #remove the blanks
    text.words.v <- text.words.v[which(text.words.v!="")]
    #use the index id from the files.v vector as the "name" in the list
    text.word.vector.l[[files.v[i]]] <- text.words.v

  }
  return(text.word.vector.l)
}

my.corpus.l <- make.file.word.v.l(files.v,input.dir)

#Get first 100 words in Jane Austen's novel
my.corpus.l[[1]][1:100]

#positions where the word "Gutenberg" occurs
positions.v <- which(my.corpus.l[[1]][]=="gutenberg")

length(positions.v)

first.instance <- positions.v[1]
my.corpus.l[[1]][first.instance]
my.corpus.l[[1]][4]

#Find words next to the word "Gutenberg"
my.corpus.l[[1]][(first.instance-1):(first.instance+1)]

#Make the display prettier

cat(my.corpus.l[[1]][(first.instance-1) : (first.instance+1)])

          ##### Chapter 9 #####

#Environment can be cleared at this point
#Store all the functions into another  file and call it here
source("code/corpusFunctions.R")

input.dir <- "data/plainText"

output.dir <- "results/"

#Load the corpus into meory 
files.v <- dir(input.dir,".*txt")
my.corpus.l <- make.file.word.v.l(files.v,input.dir)

#Prompt the use to choose the required file
mytext <- scan(file.choose(),what="character",sep="\n")

#Use readline function to allow user interaction
myyear <- readline("What year was Moby Dick published? \n")

#show the file names
show.files(names(my.corpus.l))

#Function to dynamically show the keyword and context search
doitKwic(my.corpus.l)

          ##### Chapter 10 #####

#install XML package
install.packages("XML")

#Call the package into the active R session
library(XML)

#load the XML version of Moby Dick
doc <- xmlTreeParse("data/XML1/melville1.xml",useInternalNodes = TRUE)

#Xpath is a language for representing and selecting XML nodes or elements 
#in an XML document

chapters.ns.l <- getNodeSet(doc, "/tei:TEI//tei:div1[@type='chapter']",
                           namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))

#Find class of the above file
class(chapters.ns.l)
chapters.ns.l[1]
#Chapter title "Loomings is inside the  <head> node

#First item of this list is an XML node
class(chapters.ns.l[[1]])

#To get the entire node of 'head'
chap.title <- xmlElementsByTagName(chapters.ns.l[[1]],"head")
chap.title

#To get ONLY the content of 'head'
xmlValue(chap.title[[1]])

#parse the files - similar to how we did to "Moby Dick" previously
chapter.freqs.l <- list()
chapter.raws.l <- list()

for(i in 1:length(chapters.ns.l)){
  #first get the chapter title from the head element
  chap.title <- xmlValue(xmlElementsByTagName(chapters.ns.l[[i]],
                                              "head")[[1]])
  #get only the contents of the paragraph tags
  paras.ns <- xmlElementsByTagName(chapters.ns.l[[i]],"p")
  #combine all the words from every paragraph
  chap.words.v <- paste(sapply(paras.ns,xmlValue), collapse = " ")
  #convert to lowercase
  words.lower.v <- tolower(chap.words.v)
  #tokenize
  words.l <- strsplit(words.lower.v, "\\W")
  word.v <- unlist(words.l)
  word.v <- word.v[which(word.v!="")]
  #Calculate the frequencies
  chapter.freqs.t <- table(word.v)
  chapter.raws.l[[chap.title]] <- chapter.freqs.t
  chapter.freqs.l[[chap.title]] <- 100*(chapter.freqs.t/sum(chapter.freqs.t))
}

#Run the exact same analysis that we ran on the plain text
whales <- do.call(rbind, lapply(chapter.freqs.l,'[','whale'))
ahabs <- do.call(rbind,lapply(chapter.freqs.l,'[','ahab'))
whales.ahabs <- cbind(whales,ahabs)
whales.ahabs[which(is.na(whales.ahabs))] <- 0
colnames(whales.ahabs) <- c("whale","ahab")

#plot of ahabs and whales in percentage
barplot(whales.ahabs, beside=T, col="grey")

#Correlation
whales.ahabs.df <- as.data.frame(whales.ahabs)
cor.test(whales.ahabs.df$whale,whales.ahabs.df$ahab)

#To find additional details on the title of Moby Dick
xpathApply(doc,"/tei:TEI//tei:fileDesc//tei:titleStmt//tei:title",
           namespaces =c(tei = "http://www.tei-c.org/ns/1.0"))

#Refine the result further and remove the title from the node set,
#modify the code to reference the item held inside the first([[1]]) position

xpathApply(doc,"/tei:TEI//tei:fileDesc//tei:titleStmt//tei:title",
           namespaces=c(tei = "http://www.tei-c.org/ns/1.0"))[[1]]

#Below syntax would not only store the contents in title
#It would also print the results
(title <- xpathApply(doc,"/tei:TEI//tei:fileDesc//tei:titleStmt//tei:title",
           namespaces=c(tei = "http://www.tei-c.org/ns/1.0"))[[1]])

(author <- unlist(xpathApply(doc,
                             "/tei:TEI//tei:author//tei:name",
                             namespaces=c(tei="http://www.tei-c.org/ns/1.0")))[[1]])

xpathApply(doc,"/tei:TEI//tei:teiHeader//tei:note",
           namespaces=c(tei="http://www.tei-c.org/ns/1.0"))

#Get the attribute values
(nation <- unlist(xpathApply(doc,"/tei:TEI//tei:teiHeader//tei:note",
                             namespaces=c(tei="http://www.tei-c.org/ns/1.0"),
                             xmlGetAttr,"nation")))

(gender <- unlist(xpathApply(doc,"/tei:TEI//tei:teiHeader//tei:note",
                             namespaces=c(tei="http://www.tei-c.org/ns/1.0"),
                             xmlGetAttr,"gender")))

(pubdate <- unlist(xpathApply(doc,"/tei:TEI//tei:teiHeader//tei:creation/tei:date",
                             namespaces=c(tei="http://www.tei-c.org/ns/1.0"),
                             xmlGetAttr,"value")))

          ##### Chapter 11 #####

setwd("C:\\Users\\rohan\\Desktop\\Virginia Tech\\Semester 2\\Library\\TextAnalysisWithR")

#Even working directory would be prefixed with [1], even this is vectorized in R
getwd()

#Create data with nationality of people
nations <- c(Joyce="Irish",Twain = "American", Dickens="English")

#If we don't know the nationality of someone, we can give it is as NA
nations <- c(nations,Smith=NA)
is.na(nations)

#Location where the files are stored for the clustering experiment
input.dir <- "data/XMLAuthorCorpus"

#Vector containing list of files inside input.dir with the provided pattern
files.v <- dir(path=input.dir,pattern=".*xml")

#The first file is named "anonymous.xml" - we don't know the author, we will use text analysis and unsupervised 
#clustering to compare word frequency signal of anonymous novel to the signals of the others
#AND WE WILL PREDICT THE AUTHOR OF THE ANONYMOUS FILE

file.path(input.dir,files.v[i])
library(XML)

#Create a reusable function for the XML files
getTEIWordTableList <- function(doc.object){
#Add all paragraph nodes using this function
paras <-getNodeSet(doc.object,"/tei:TEI/tei:text/tei:body//tei:p",
                     c(tei="http://www.tei-c.org/ns/1.0"))
#Join the paragraph node content using a blank space
words <- paste(sapply(paras,xmlValue),collapse = " ")

#Convert to lower case
words.lower <- tolower(words)

#Split the words based on non-word characters
words.l <- strsplit(words.lower,"\\W")

#Convert the list to words
word.v <- unlist(words.l)

#Remove blanks
word.v[which(word.v!="")]

#Embed all this in the table function
book.freqs.t <- table(word.v[which(word.v!="")])

#Calculate the relative word frequencies
book.freqs.rel.t <- 100*(book.freqs.t/sum(book.freqs.t))

return(book.freqs.rel.t)

}

#Save the functions in another file and source it here
source("code/corpusFunctions.R")

#This would serve as a container for the processed docs
book.freqs.l <- list()
for(i in 1:length(files.v)){
  #Use xmlTreeParse to load the first XML file
  doc.object <- xmlTreeParse(file.path(input.dir,files.v[i]),
                             useInternalNodes=TRUE)
  worddata <- getTEIWordTableList(doc.object)
  book.freqs.l[[files.v[i]]] <- worddata
}

#We will use Euclidean metric to calculate each single book's distance from every other book in corpus
#Simple example of books and features

a <- c(10,5)
b <- c(11,6)
c <-  c(4,13)

my.m <- rbind(a,b,c)
colnames(my.m) <- c("f1","f2")

#From plotting or finding distance - we can see that a and b are similar
plot(my.m)
dist(my.m)

#For using in authorship data, we need to convert it into r dataframe
#Each table object is converted to a data.frame object
freqs.l <- mapply(data.frame, ID = seq_along(book.freqs.l),
                  book.freqs.l, SIMPLIFY = FALSE, MoreArgs = list(stringsAsFactors=FALSE))

#Compare the difference
class(freqs.l[[1]])
class(book.freqs.l[[1]])

#First few would be digits as they are counted as word characters
freqs.l[[1]][1:10,]
freqs.l[[1]][100:110,]

#This data should be in matrix object
freqs.df <- do.call(rbind,freqs.l)

#reshape the dataframe into wide format - xtabs is used for creating cross tabulations(like pivot table) 
result <- xtabs(Freq~ID+Var1, data=freqs.df)

#For analysis, convert xtabs to matrix
final.m <- apply(result,2,as.numeric)

#Limit the feature list to only those words that appears across the 
#entire corpus with the mean relative frequency of some threshold
smaller.m <- final.m[,apply(final.m,2,mean) >= .25]

#Above operation is done so that only high frequency words are used to compare the texts

#create a distance object
dm <- dist(smaller.m)
#Perform a cluster analysis on distance object
cluster <- hclust(dm)
#Get the book file names to use as labels
cluster$labels <- names(book.freqs.l)

#Plot a dendogram for inspection
plot(cluster)

#From the cluster plot, we can see Kyne is the author of anonymous.

          ##### Chapter 12 #####

#When the classes are known(closed set of possible authors or cases),
#Supervised classification offers a better approach

setwd("C://Users//rohan//Desktop//Virginia Tech//Semester 2//Library//TextAnalysisWithR")

library(XML)

#input the location and data files
input.dir <- "data/XMLAuthorCorpus"
files.v <- dir(input.dir,".xml")

#Segment the files into multiple chunks
#Each file is converted to 10 chunks
getTEIWordSegmentTableList <- function(doc.object,chunk.size=10){
  #Identify XML nodes that match a particular criterion
  paras <- getNodeSet(doc.object,"/d:TEI/d:text/d:body//d:p",
                      c(d = "http://www.tei-c.org/ns/1.0"))
  #Join the paragraph node content using a blank space
  words <- paste(sapply(paras,xmlValue),collapse = " ")
  #Convert to lower case
  words.lower <- tolower(words)
  #Split the word in terms of non-word characters
  words.list <- strsplit(words.lower,"\\W")
  
  #Convert the list to vectors
  word.v <- unlist(words.list)
  #split the data file word.v based on chunk size
  max.length <- length(word.v)/chunk.size
  
  #Create vector of word positions to chunk the text
  x <- seq_along(word.v) 
  
  
  chunks.l <- split(word.v,ceiling(x/max.length))
  
  #remove blanks from file
  chunks.l <- lapply(chunks.l,removeBlanks)
  
  #Make a table of the words
  freq.chunks.l <- lapply(chunks.l,table)
  rel.freq.chunk.l <- lapply(freq.chunks.l,prop.table)
  return(rel.freq.chunk.l)
}

#Create a empty list
book.freqs.l <- list()
for(i in 1:length(files.v))
{
  doc.object <- xmlTreeParse(file.path(input.dir,files.v[i]),
                             useInternalNodes = TRUE)
  #temporary storage (chunk.data.l)
  chunk.data.l <- getTEIWordSegmentTableList(doc.object,10)
  book.freqs.l[[files.v[i]]] <- chunk.data.l
}

#First 3 words in the first chunk of the book Norris1
book.freqs.l$Norris1.xml[[1]][1:3]



#Use mapply to convert each table of word frequencies to data.frame
my.mapply <- function(x){
  my.list <- mapply(data.frame,ID=seq_along(x),x,SIMPLIFY = FALSE,
                    MoreArgs = list(stringsAsFactors=FALSE))
  my.df <- do.call(rbind,my.list)
}

#Bind the results into a long form data frame
freqs.l <- lapply(book.freqs.l,my.mapply)
freqs.df <- do.call(rbind,freqs.l)

#View the top few rows
head(freqs.df)

#gsub takes a pattern to search for, a replacement value and a object to search within
bookids.v <- gsub("\\..*","",rownames(freqs.df))

#Glue the new strings in the ID column
book.chunk.ids <- paste(bookids.v,freqs.df$ID,sep="_")

#Replace the existing values in ID column with the new values
freqs.df$ID <- book.chunk.ids

#Perform cross tabulation - similar to pivot table in Excel
result.t <- xtabs(Freq~ID+Var1, data=freqs.df)

final.df <- as.data.frame.matrix(result.t)

#Extract all the author names into a new variable
metacols.m <- do.call(rbind,strsplit(rownames(final.df),"_"))
colnames(metacols.m) <- c("sampletext","samplechunk")

#Distinct author names
unique(metacols.m[,"sampletext"])

#Delete the digits in the author names 
author.v <- gsub("\\d+$","",metacols.m[,"sampletext"])

#Create a final dataframe that binds these columns to final.df
authorship.df <- cbind(author.v,metacols.m,final.df)

#Reduce number of features 
#Keep only the features with MRF of 0.005

#Calculate mean of all columns
freq.means.v <- colMeans(authorship.df[,4:ncol(authorship.df)])

#Keep only those columns with mean >0.005
keepers.v <- which(freq.means.v>=0.005)

#Retain columns which have been shortlisted
smaller.df <- authorship.df[,c(names(authorship.df)[1:3],names(keepers.v))]

#Identify rows written by anonymous 
anon.v <- which(smaller.df$author.v == "anonymous")

#Identify the dataset used to train the model
#Remove the anonymous rows and the metadata columns
train <- smaller.df[-anon.v,4:ncol(smaller.df)]

#Identify column the classifier will use to organize the data (author column)
class.f <- smaller.df[-anon.v,"author.v"]

#Load the package for SVM
library(e1071)

#Generate a model using the SVM classifier function and 
#data contained in train and class.f objects

model.svm <- svm(train,class.f)
summary(model.svm)

#Test accuracy of the model
pred.svm <- predict(model.svm,train)
as.data.frame(pred.svm)
check <- table(pred.svm,class.f)
#We can see that everything has been classified correctly 
#Except one of carleton's works has been misclassified as Mchenry's
#BASICALLY MODEL IS PRETTY AWESOME

#Isolate the test data
testdata <- smaller.df[anon.v,4:ncol(smaller.df)]

#Send the test data to model for prediction
final.result <- predict(model.svm,testdata)
as.data.frame(final.result)

#RESULTS OF SVM CLASSIFICATION CONFIRM WHAT WAS OBSERVED IN CLUSTERING
#KYNE IS THE LIKELY AUTHOR OF EVERY ANONYMOUS BOOK

          ##### Chapter 13 #####

#Unlike previous chapter, we will split the text into chunk size
#of 1000 words each 

#Load package, referencing the corpus directory
#and generate a vector of file names

library(XML)
inputDir <- "data/XMLAuthorCorpus"
files.v <- dir(path=inputDir,pattern=".*xml")

#Variable for number of words per chunk
chunk.size <- 1000 #number of words per chunk

#Create a function for processing the corpus and splitting into chunks
makeFlexTextChunks <- function(doc.object,chunk.size=10,percentage=TRUE)
{
  paras <- getNodeSet(doc.object,
                      "/d:TEI/d:text/d:body//d:p",
                      c(d = "http://www.tei-c.org/ns/1.0"))
  words <- paste(sapply(paras,xmlValue), collapse = " ")
  words.lower <- tolower(words)
  #Find punctutation marks except apostrophe and replace with blank spaces
  words.lower <-gsub("[^[:alnum:][:space:]']"," ",words.lower)
  words.l <- strsplit(words.lower,"\\s+")
  #Unlist and create vector of word positions to chunk the text
  word.v <- unlist(words.l)
  x <- seq_along(word.v)
  if(percentage){
    max.length <- length(word.v)/chunk.size
  } else{
    chunks.l <- split(word.v, ceiling(x/chunk.size))
    #If last chunk size is half of the fixed size, then add it to the 
    #previous chunk
    if(length(chunks.l[[length(chunks.l)]]) <= chunk.size/2){
      chunks.l[[length(chunks.l)-1]] <- c(chunks.l[[length(chunks.l) - 1]],
                                          chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]] <- NULL
    }
    
  }
  chunks.l <- lapply(chunks.l,paste,collapse = " ")
  chunks.df <- do.call(rbind,chunks.l)
  return(chunks.df)
}

#capture original file names and remove file extensions
textname <- gsub("\\..*","",files.v[i])

topic.m <- NULL
for(i in 1:length(files.v)){
  doc.object <-xmlTreeParse(file.path(inputDir,files.v[i]),
                            useInternalNodes = TRUE)
  chunk.m <- makeFlexTextChunks(doc.object,chunk.size,
                                percentage=FALSE)
  textname <- gsub("\\..*","",files.v[i])
  segments.m <- cbind(paste(textname,
                            segment=1:nrow(chunk.m),sep="_"),chunk.m)
  topic.m <- rbind(topic.m,segments.m)
}

#To ingest this into mallet package, convert to data frame
#and rename the column headers
documents <- as.data.frame(topic.m, stringsAsFactors=F)
colnames(documents) <- c("id","text")


#Download mallet package for topic modelling
#Set Java Home directory manually 
Sys.setenv(JAVA_HOME='C:\\Users\\rohan\\Downloads\\_temp_matlab_R2017b_win64\\sys\\java\\jre\\win64\\jre') 
library(rJava)
library(mallet)

#In topic modeling, we need to remove high frequency words
#These words carry little weight in terms of thematic or topical value

#mallet.import takes 5 arguments
#1. id.array -> vector of document id's
#2. text.array -> text strings
#3. stoplist.file -> locates the file in your computer
#4. preserve.case -> used to lowercase (irrelevant in our example)
#5. token.regexp -> for tokenizing the text strings - changes to be made for retaining apostrophe's
mallet.instances <- mallet.import(documents$id,
                                  documents$text,
                                  "data/stoplist.csv",
                                  FALSE,
                                  token.regexp = "[\\p{L}']+")

#mallet.instances object created is a Java object that is called a Mallet instance list
#This is not a R object and must be accessed using Java methods

#Create a topic model trainer object
topic.model <- MalletLDA(num.topics = 43)

#Fill the trainer object with textual data
topic.model$loadDocuments(mallet.instances)

#access the vocabulary of the corpus
vocabulary <- topic.model$getVocabulary()

#Access info about frequency of words in corpus
word.freqs <- mallet.word.freqs(topic.model)

#term.freq -> provides a count of total number of token of that word in corpus
#doc.freq -> no. of documents that contain the word atleast once

#Set number of iterations to 400
topic.model$train(400)

#Mallet provides 2 functions that return robjects :
#mallet.topic.words and mallet.top.words

#Create a matrix where each row is a topic and each column is a word in corpus
topic.words.m <- mallet.topic.words(topic.model, smoothed=TRUE,
                                    normalized = TRUE)

#Normalization has been set to true - values in each topic
#are converted to % that sum up to 1
rowSums(topic.words.m)

#Give headers to these matrix to identify the words
vocabulary <- topic.model$getVocabulary()
colnames(topic.words.m) <- vocabulary
topic.words.m[1:3,1:3]

#Select few columns from matrix
keywords <- c("california","ireland")
topic.words.m[,keywords]

#Identify which of the topic rows have highest concentration of these key terms
#using R's rowsums and max functions inside a call to which

imp.row <- which(rowSums(topic.words.m[,keywords]) == 
                   max(rowSums(topic.words.m[,keywords  ])))

#For ranked sorting of topic words, use function mallet.top.words

mallet.top.words(topic.model,topic.words.m[imp.row,],10)
#Most heavily weighted word in this topic is irish

#Create word cloud using wordcloud package
library(wordcloud)

#Grab top 100 words and their associated weights from the model
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[imp.row,],100)

#Use word cloud and add arguments to make it more aesthetic
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8),rot.per=0,random.order=F)

#mallet provides a function to assess proportion of a document
#that is about each topic
doc.topics.m <- mallet.doc.topics(topic.model,smoothed=T,
                                  normalized = T)

#Take segments from all the novels and store them
file.ids.v <- documents[,1]

#Edit names in vector to split chunk identifier from main file name
file.id.l <-strsplit(file.ids.v,"_")
file.chunk.id.l <- lapply(file.id.l,rbind)
file.chunk.id.m <- do.call(rbind,file.chunk.id.l)
head(file.chunk.id.m)

#convert to data.frame
doc.topics.df <- as.data.frame(doc.topics.m)

#Bind character data values in first column 
doc.topics.df <- cbind(file.chunk.id.m[,1],doc.topics.df)

#Calculate mean across segments of each document
doc.topic.means.df <- aggregate(doc.topics.df[,2:ncol(doc.topics.df)],
                                list(doc.topics.df[,1]),
                                mean)

#Pre-processing with a POS tagger
inputDir <- "data/taggedCorpus"
files.v <- dir(path=inputDir,pattern=".*xml")
chunk.size=500

#Split the tagged file into a vector where each value is a single word/POS pair
splitText <- function(text){
  unlist(strsplit(text," "))
}

#Function to walk through the vector and pick out only those values
#that contain certain target POS markers
selectTaggedWords <- function(tagged.words,target.tag){
  tagged.words[grep(target.tag,tagged.words)]
}

#remove any characters starting with slash followed by 2 or 3 capital letters
removeTags <- function(word.pos)
{sub("/[A-Z]{2,3}","",word.pos)}

#Create a function to wrap all of the above
makeFlexTextChunksFromTagged <- function(tagged.text,
                                         chunk.size=500,percentage=TRUE){
  tagged.words <- splitText(tagged.text)
  tagged.words.keep <- c(selectTaggedWords(tagged.words,"/NN$"))
  words <- removeTags(tagged.words.keep)
  words.lower <- tolower(words)
  word.v <- gsub("[^[:alnum:][:space:]']","",words.lower)
  x <- seq_along(word.v)
  if(percentage) {
    max.length <- length(word.v)/chunk.size
    chunks.l <- split(word.v,ceiling(x/max.length))
  } else{
    chunks.l <- split(word.v,ceiling(x/chunk.size))
    if(length(chunks.l[[length(chunks.l)]])<=
       length(chunks.l[[length(chunks.l)]])/2){
      chunks.l[[length(chunks.l) - 1]] <-
        c(chunks.l[[length(chunks.l) - 1]],chunks.l[[length(chunks.l)]])
      chunks.l[[length(chunks.l)]] <- NULL
    }
  }
  chunks.l <- lapply(chunks.l,paste,collapse=" ")
  chunks.df <- do.call(rbind,chunks.l)
  return(chunks.df)
}


source("code/corpusFunctions.R")

topic.m <- NULL
for(i in 1:length(files.v)){
  tagged.text <- scan(file.path(inputDir,files.v[i]),
                      what="character",sep="\n")
  chunk.m <- makeFlexTextChunksFromTagged(tagged.text,
                                          chunk.size,percentage=FALSE)
  textname <- gsub("\\..*","",files.v[i])
  segments.m <- cbind(paste(textname,segment=1:nrow(chunk.m),
                            sep="_"), chunk.m)
  topic.m <- rbind(topic.m,segments.m)
}

documents <- as.data.frame(topic.m,stringsAsFactors = F)
colnames(documents) <- c("id","text")
library(mallet)
mallet.instances <- mallet.import(documents$id,
                                  documents$text,
                                  "data/stoplist.csv",
                                  FALSE,token.regexp="[\\p{L}']+")
topic.model <- MalletLDA(num.topics=43)
topic.model$loadDocuments(mallet.instances)

vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
topic.model$train(400)
topic.words.m <- mallet.topic.words(topic.model,
                                    smoothed=TRUE,
                                    normalized=TRUE)
colnames(topic.words.m) <- vocabulary


#Create word cloud using wordcloud package
library(wordcloud)

#Grab top 100 words and their associated weights from the model
topic.top.words <- mallet.top.words(topic.model,
                                    topic.words.m[imp.row,],100)

#Use word cloud and add arguments to make it more aesthetic
wordcloud(topic.top.words$words,
          topic.top.words$weights,
          c(4,.8),rot.per=0,random.order=F)

#################################################################
#################################################################
#######################Additional Analysis#######################
#################################################################
#################################################################

#import lyrics of stairway to heaven
lyrics <- scan("Humble_lyrics.txt",what="character",sep="\n")

#join all the lines
lyrics <- paste(lyrics,collapse=" ")

#convert the entire text to lower case
lyrics_lower <- tolower(lyrics)

#Extract the words and organize them into a list - using strsplit
#\\W is a 'regular expression' and matches any non-word character
lyrics_list <- strsplit(lyrics_lower,"\\W")

#Create a vector again instead of a list (We don't need a list in this problem)
lyrics_list <- unlist(lyrics_list)

#identify items which are not blank
not.blanks <-which(lyrics_list!="")

#overwrite the data with the non-blank entries
lyrics_list <- lyrics_list[not.blanks]

#How often does he use each of the words?
lyrics_freq <- table(lyrics_list)
sorted.lyrics <- as.data.frame(sort(lyrics_freq, decreasing=TRUE))

wordcloud(sorted.lyrics$lyrics_list,sorted.lyrics$Freq,scale=c(4,.5),
          max.words=Inf)
