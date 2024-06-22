rm(list = ls())
require(tm)
#require(rtweet)
require("textmineR")
require("qdapRegex")
require("stringi")
require("stringr")
#remember to reference the stopwords
stop.words <- c("εκει","https","κάποια","εσυ","αυτος","αυτη","εμεις","πάνω","κάτω","t.co","u","0001f92a","εχεις","αλλα","άλλα","τι","κατά","γιατι",
                "γιατί","αλλά","ως","μέσα","ειχε","όπως","όλο","ο","α","β","γ","δ","ε","ζ","η","θ","ι","κ","λ","μ","ν","ξ","ο","π","ρ","σ","τ",
                "υ","φ","χ","ψ","ω","a","b","c","d","e","f","να","ναι","μας","τετοιες","ήταν","ηταν","αυτο","ας","εγω","εχει","ή","η","εκεί",
                "και","λίγο","λιγο","πάλι","μονο","απ","μόνο","αυτά","αυτή","αυτα","αυτη","εγώ","ούτε","υπάρχει","-","κάνει","στους","κάθε","πρέπει",
                "τώρα","λέει","όχι","ήταν","amp","δύο","σαν","το","να","για","του","είναι","ειναι","στις","έχω","μετά","μη","κάτι","είσαι","πολύ",
                "σήμερα","καλημέρα","όλα","ολα","όλοι","ολοι","όλες","ολες","πολυ","πολλή","πολλά","πολλη","πολλα","την","με","του","της","τα","που" , 
                "δεν", "στο","είναι", "θα", "τον","σε","από","απο", "μου","στην","οι", "τους","μας","τη", "των", "στη","στα","τις", "ότι","οτι", "σου","στον","αλλά","μια", "τι","αν",
                "σας","έχει","ένα","αυτό","δε","όταν","κι", "γιατί", "πως","πιο", "μην", "έχουν", "ρε","μόνο", "rt", "kali_nda", "antireport", "fullnews.gr",
                "deai_セフレ", "enikos.gr", "sharethis", "cocobilly", "vice.gr", "oknhn5glyl","ereportaz", "account is", "fuckiditsa", "athyrostomix",
                "adiasistos", "partalia", "vivanews.gr", "left.gr","freedybruna","oknhn5xwpt","oknhn5xwpt", "oknhn5xwpt_","ismini259","parafrwn",
                "xx","triklopodia","sharethi","triklopodia_gr","τοτε","tovimagr","μέσω_χρήστη","klark_kentis","zappasaspa","adekastos_baras","κως_ειδήσεις",
                "ειδήσεις_νέα","iefimerida_gr","el_ant","qf","okbrnjq","eidiseis","n","w","zfw","lavrentisberia", "προς","wwwdonnagr","figaro","gt",
                "stefanosntampos","lampatzampa","セフレ","deai","dcpyb","b","d","to","ikarosm","it","parafrwn","xx","ειδήσεις","αλλος","άλλος","πχ",
                "μέσω","μεσω","χρήστη","χρηστη","newspepper","blacktom","lifo","nserv","xeay","εν","bou","pou","i","m","vasilisxtm","in")

#df.all <- read.csv("/home/stathis/Desktop/res/ref/data/2016/q2016.unique.per.w.csv")
df.all <- read.csv("/home/stathis/Desktop/res/ref/data/2015/tweets.per.w.fixed")
j = 1
for( j in 1:NROW(unique(df.all$period))){
#week1
name <- unique(df.all$period)[j]

df <- subset(df.all, df.all$period == name)

df$text <- tolower(df$text)
df$text <- rm_url(df$text)
#df$text <- stemDocument(df$text, "greek")
#Data_corpus[[1]]$content
Data_corpus <- Corpus(VectorSource(df$text))
s <- tm_map(Data_corpus, removeWords, stop.words)

df$tmtext <- NA

for(i in 1:NROW(df)){
  df$tmtext[i] <- s[[i]]$content  
}


#  df.l <- subset(df, df$lang != "el")
#  df <- subset(df, df$lang == "el")
#  df <- subset(df, df$interval == unique(df$interval[1]))
#  df$tmtext <- rm_url(df$text)
#  df$nlp.text <- tolower(df$nlp.text)


df$idn <- 1:NROW(df)
#df$nlp.text <- gsub("μετανάστες", "", df$nlp.text)

#dtm
dtm <- CreateDtm(doc_vec = df$tmtext, # character vector of documents
                 doc_names = df$idn, # document names
                 ngram_window = c(2, 3), # minimum and maximum n-gram length
                 stopword_vec = stop.words, # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 4) # default is all available cpus on the system

#dtm <- CreateDtm(doc_vec = nih_sample$ABSTRACT_TEXT, # character vector of documents
#                 doc_names = nih_sample$APPLICATION_ID, # document names
#                 ngram_window = c(1, 2), # minimum and maximum n-gram length
#                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
#                                  stopwords::stopwords(source = "smart")), # this is the default value
#                 lower = TRUE, # lowercase - this is the default value
#                 remove_punctuation = TRUE, # punctuation - this is the default
#                 remove_numbers = TRUE, # numbers - this is the default
#                 verbose = FALSE, # Turn off status bar for this demo
#                 cpus = 2) # default is all available cpus on the system


dtm <- dtm[,colSums(dtm) > 2]


k = round(nrow(df)/650)
model <- FitLdaModel(dtm = dtm, 
                     k = k,
                     iterations = 600, # I usually recommend at least 500 iterations or more
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.01,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 4) 



#save(model, "/home/stathis/Desktop/ref/res/data/lmodel")
#save.image(file = "/home/stathis/Desktop/data.RData")
title.text <- paste(name,"r2 =",model$r2)
plot.l <- paste0("/home/stathis/Desktop/res/ref/data/results/plots/test/",title.text,".png")
png(plot.l, width = 480, height = 480, units = "px")
plot(model$log_likelihood, type = "l", main = title.text)
dev.off()

model$top_terms <- GetTopTerms(phi = model$phi, M = 10)
model$top_terms
head(t(model$top_terms))
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100
# prevalence should be proportional to alpha
#plot.p <- plot(model$prevalence, model$alpha, xlab = "prevalence", ylab = "alpha", main = name)

plot.p.path <- paste0("/home/stathis/Desktop/res/ref/data/results/plots/test/",unique(df.all$period)[j],".png")
png(filename = plot.p.path, width = 480, height = 480, units = "px")
plot(model$prevalence, model$alpha, xlab = "prevalence", ylab = "alpha", main = name)
dev.off()

model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dtm,
                            M = 1)


model$labels
#>     label_1                      
#> t_1 "radiation_necrosis"         
#> t_2 "kda_fragment"               
#> t_3 "cardiovascular_disease"     
#> t_4 "mast_cell"                  
#> t_5 "radiation_necrosis"         
#> t_6 "laparoscopic_fundoplication"

# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)


lda.summary <- model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ][ 1:k , ]
lda.summary
path = paste0("/home/stathis/Desktop/res/ref/data/results/lda/test/",unique(df.all$period)[j], "_",k,"_k_", Sys.time(),".csv")

write.csv(lda.summary, path)
rm(lda.summary, k, i, plot.l, plot.p.path, dtm, df, model, s, Data_corpus, name, path, title.text)
}
