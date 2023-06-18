#Cerrado Perception

# quanteda -  content analysis --------------------------------------------
#Load packages
library(ggplot2)
library(tidyverse)
library(quanteda)
library(stopwords)
library(readtext)
library(quanteda.textplots)
library(quanteda.textmodels)
library(quanteda.textstats)
stopwords_getsources()


#English translated by Google Translate service#
################################################

stopwords_getsources()

text_spec <- read.csv("Open_Answers_Specs_En.csv", h=T)
text_spec$Type <- rep("Specialist", nrow(text_spec))

text_rural <- read.csv("Open_Answers_Rural_En.csv", h=T)
text_rural$Type <- rep("RuralProp", nrow(text_rural))

text_rural_spec <- rbind(text_rural, text_spec)

Cerrado.pos <- corpus(text_rural_spec[is.na(text_rural_spec$Cerrado_Pos)==FALSE,], 
                      text_field = "Cerrado_Pos")
Cerrado.pos
summary(Cerrado.pos)

Cerrado.neg <- corpus(text_rural_spec[is.na(text_rural_spec$Cerrado_Neg)==FALSE,], 
                      text_field = "Cerrado_Neg")
Cerrado.neg
summary(Cerrado.neg)

stopwords("en")

#Create a document feature matrix
#Negative
mytokens.neg <- tokens(Cerrado.neg,remove_punct = TRUE,remove_numbers = TRUE)
doc.neg.dfm <- dfm(mytokens.neg)
doc.neg.dfm <- dfm_remove(doc.neg.dfm, 
                          pattern=c(stopwords("en")))
doc.neg.dfm <- dfm_select(doc.neg.dfm,selection="remove",min_nchar=3)
doc.neg.dfm <- dfm_wordstem(doc.neg.dfm, language = "en")
doc.neg.dfm

topfeatures(doc.neg.dfm, 50)  # 50 top words
topfeatures(doc.neg.dfm, 100)  # 100 top words
topfeatures(doc.neg.dfm, 50, groups=Type)  # 25 top words

#Positive
mytokens.pos <- tokens(Cerrado.pos,remove_punct = TRUE,remove_numbers = TRUE)
doc.pos.dfm <- dfm(mytokens.pos)
doc.pos.dfm <- dfm_remove(doc.pos.dfm, 
                          pattern=c(stopwords("en")))
doc.pos.dfm <- dfm_select(doc.pos.dfm,selection="remove",min_nchar=3)
doc.pos.dfm <- dfm_wordstem(doc.pos.dfm, language = "en")
doc.pos.dfm

topfeatures(doc.pos.dfm, 50)  # 50 top words
topfeatures(doc.pos.dfm, 100)  # 100 top words
topfeatures(doc.pos.dfm, 50, groups=Type)  # 25 top words


#Network analysis#
##################

#Positive
fcm.subset.pos<-fcm(doc.pos.dfm, context = "document", count = "frequency")
fcm.subset.pos
rowSums(fcm.subset.pos> 2)

head(fcm.subset.pos, n = 15, nf = 15)
tail(fcm.subset.pos, n = 15, nf = 15)

quartz(8,12)
set.seed(123)
textplot_network(fcm.subset.pos,edge_alpha = 0.4, edge_size = 1,min_freq=2, max.overlaps=5)

doc.pos.dfm.group <- dfm_group(doc.pos.dfm, groups = Type)

quartz(8,12)
textplot_network(doc.pos.dfm.group,edge_alpha = 0.4, edge_size = 1,min_freq=.99, max.overlaps=5)

quartz(8,8)
textplot_wordcloud(doc.pos.dfm.group, min_count = 2, random_order = FALSE,
                   rotation = .25, comparison = T,
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

quartz(8,8)
textplot_wordcloud(doc.pos.dfm, min_count = 2, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

#Negative
fcm.subset.neg<-fcm(doc.neg.dfm, context = "document", count = "frequency")
fcm.subset.neg
rowSums(fcm.subset.neg> 0)

head(fcm.subset.neg, n = 15, nf = 15)
tail(fcm.subset.neg, n = 15, nf = 15)

quartz(8,12)
set.seed(123)
textplot_network(fcm.subset.neg,edge_alpha = 0.4, edge_size = 1,min_freq=2, max.overlaps=5)

doc.neg.dfm.group <- dfm_group(doc.neg.dfm, groups = Type)

quartz(8,12)
textplot_network(doc.neg.dfm.group,edge_alpha = 0.4, edge_size = 1,min_freq=.99, max.overlaps=5,
                 comparison = T)

quartz(8,8)
textplot_wordcloud(doc.neg.dfm.group, min_count = 2, random_order = FALSE,
                   rotation = .25, comparison = T,
                   color = RColorBrewer::brewer.pal(8,"Dark2"))

quartz(8,8)
textplot_wordcloud(doc.neg.dfm, min_count = 2, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))


#################
#Text statistics#
#################
library(quanteda.textstats)

#Negative
freq.stats.neg<-textstat_frequency(doc.neg.dfm)
write.table(freq.stats.neg,"freq.stats.neg.en.txt")

freq.stats.neg.type<-textstat_frequency(doc.neg.dfm,groups=Type)
write.table(freq.stats.neg.type,"freq.stats.neg.type.en.txt")

#Positive
freq.stats.pos<-textstat_frequency(doc.pos.dfm)
write.table(freq.stats.pos,"freq.stats.pos.en.txt")

freq.stats.pos.type<-textstat_frequency(doc.pos.dfm,groups=Type)
write.table(freq.stats.pos.type,"freq.stats.pos.type.en.txt")

#################
#Fit text models#
#################
library(quanteda.textmodels)

#######################
#Por tipo de documento#
#######################

#Positive
(tmod.pos <- textmodel_nb(x=doc.pos.dfm, y = Cerrado.pos$Type, 
                          distribution = "Bernoulli",prior="termfreq"))
summary(tmod.pos)
coef.tmod.pos<-coef(tmod.pos)
write.table(coef.tmod.pos,"coef.tmod.pos.eng.txt")

#Inspect the quality of classification
actual_class <- docvars(doc.pos.dfm, "Type")
predicted_class <- predict(tmod.pos, newdata = doc.pos.dfm)
tab_class <- table(actual_class, predicted_class)
tab_class #91%!!!
prop.table(table(predict(tmod.pos) == docvars(doc.pos.dfm, "Type"))) * 100

r <- 1:10000
for(i in 1:10000){
  r[i] <- prop.table(table(sample(predict(tmod.pos)) == docvars(doc.pos.dfm, "Type")))[1] * 100 #random
}
mean(r)

library(caret)
(confusion<-confusionMatrix(tab_class, mode = "everything"))

library(tidyverse)
nb.terms <- as.data.frame(t(tmod.pos$param)) %>% 
  rownames_to_column("Word") %>% 
  gather(Type, Association, -Word) %>% 
  arrange(Type, desc(Association)) %>% 
  group_by(Type) %>% 
  mutate(Rank = row_number()) %>% 
  filter(Rank <= 25)

quartz(8,8)
ggplot(filter(nb.terms, Type == "RuralProp"), aes(reorder(Word, Rank), Association)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Association") + ylab("Word") + ggtitle("Rural landowners")

quartz(8,8)
ggplot(filter(nb.terms, Type == "Specialist"), aes(reorder(Word, Rank), Association)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Association") + ylab("Word") + ggtitle("Specialists")

#Negative
(tmod.neg <- textmodel_nb(x=doc.neg.dfm, y = Cerrado.neg$Type, 
                          distribution = "Bernoulli",prior="termfreq"))
summary(tmod.neg)
coef.tmod.neg<-coef(tmod.neg)
write.table(coef.tmod.neg,"coef.tmod.neg.eng.txt")

#Inspect the quality of classification
actual_class <- docvars(doc.neg.dfm, "Type")
predicted_class <- predict(tmod.neg, newdata = doc.neg.dfm)
tab_class <- table(actual_class, predicted_class)
tab_class #82.35%!!!
prop.table(table(predict(tmod.neg) == docvars(doc.neg.dfm, "Type"))) * 100

r <- 1:10000
for(i in 1:10000){
  r[i] <- prop.table(table(sample(predict(tmod.neg)) == docvars(doc.neg.dfm, "Type")))[1] * 100 #random
}
mean(r)

library(caret)
(confusion<-confusionMatrix(tab_class, mode = "everything"))

nb.terms <- as.data.frame(t(tmod.neg$param)) %>% 
  rownames_to_column("Word") %>% 
  gather(Type, Association, -Word) %>% 
  arrange(Type, desc(Association)) %>% 
  group_by(Type) %>% 
  mutate(Rank = row_number()) %>% 
  filter(Rank <= 25)

quartz(8,8)
ggplot(filter(nb.terms, Type == "RuralProp"), aes(reorder(Word, Rank), Association)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Association") + ylab("Word") + ggtitle("Rural landowners")

quartz(8,8)
ggplot(filter(nb.terms, Type == "Specialist"), aes(reorder(Word, Rank), Association)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Association") + ylab("Word") + ggtitle("Specialists")

#Topic Models#
##############


#Using STM#
###########
library(stm)

#Negative
dfm.neg2stm <- convert(doc.neg.dfm, to = "stm")

neg.stm.idealK <- searchK(dfm.neg2stm$documents, 
                          dfm.neg2stm$vocab, 
                          prevalence = ~Type, content = ~Type,
                          data = dfm.neg2stm$meta,
                          K = seq(3, 20, by = 1))

plot(neg.stm.idealK)

set.seed(123)
neg_lda_fit10 <- stm(dfm.neg2stm$documents, 
                     dfm.neg2stm$vocab, 
                     prevalence = ~Type, content = ~Type,
                     data = dfm.neg2stm$meta, 
                     K = 13, verbose = FALSE)

summary(neg_lda_fit10)

prep.neg <- estimateEffect(~ Type, neg_lda_fit10, metadata = dfm.neg2stm$meta)
summary(prep.neg)#Only topics 5 and 8

quartz(8,8)
plot(neg_lda_fit10)

# quartz(12,8)
# par(mar=c(1,1,1,1))
# plot(neg_lda_fit10,"labels")

quartz(8,8)
plot.topicCorr(topicCorr(neg_lda_fit10))

#quartz(8,8)
plot(neg_lda_fit10, "perspectives", topics = c(6,2))

#quartz(8,8)
plot(neg_lda_fit10, "perspectives", topics = c(8,3))

#quartz(8,8)
plot(neg_lda_fit10, "hist")

#quartz(8,8)
cloud(neg_lda_fit10, topic = 12)

#Positive
dfm.pos2stm <- convert(doc.pos.dfm, to = "stm")

pos.stm.idealK <- searchK(dfm.pos2stm$documents, 
                          dfm.pos2stm$vocab, 
                          prevalence = ~Type, content = ~Type,
                          data = dfm.pos2stm$meta,
                          K = seq(3, 20, by = 1))

plot(pos.stm.idealK)

set.seed(123)
pos_lda_fit10 <- stm(dfm.pos2stm$documents, 
                     dfm.pos2stm$vocab, K = 8, verbose = FALSE,
                     prevalence = ~Type, content = ~Type,
                     data = dfm.pos2stm$meta)

summary(pos_lda_fit10)

prep.pos <- estimateEffect(~ Type, pos_lda_fit10, metadata = dfm.pos2stm$meta)
summary(prep.pos)#Only topics 5 and 8

quartz(8,8)
plot(pos_lda_fit10)

quartz(12,8)
par(mar=c(1,1,1,1))
plot(pos_lda_fit10,"labels")

quartz(8,8)
plot.topicCorr(topicCorr(pos_lda_fit10))

quartz(8,8)
plot(pos_lda_fit10, "perspectives", topics = 8)

quartz(8,8)
plot(pos_lda_fit10, "perspectives", topics = 5)

quartz(8,8)
plot(pos_lda_fit10, "hist")
