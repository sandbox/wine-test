library(ggplot2)
library(reshape)
## creating the word cloud visualization
library(wordcloud)
library(RColorBrewer)

non.word.columns <- 1:3

treatment.ranks <- function(row, f) {
  sorted.cols = sort(f[row, -(non.word.columns)], decreasing=TRUE)
  as.integer(sub("X", "", colnames(sorted.cols)))
}
word.ranks <- function(berry, earthy) {
  berry.order <- data.frame(berry=1:length(berry),word=berry)
  berry.order <- berry.order[with(berry.order, order(word)), ]
  earthy.order <- data.frame(earthy=1:length(earthy),word=earthy)
  earthy.order <- earthy.order[with(earthy.order, order(word)), ]

  data.frame(berry=berry.order$berry,earthy=earthy.order$earthy,word=words[earthy.order$word,1],color=words[earthy.order$word,3])
}
rank.chart <- function(ranks, title="Blah", filename="word-rank.pdf") {
  graph.df <- melt(ranks)
  ggplot(graph.df, aes(factor(variable), value, group = word, colour = color, label = word)) +
    geom_line() +
      geom_text(data = subset(graph.df, variable == "earthy"), aes(x = factor(variable)), size = 3.5, hjust = 0) +
        geom_text(data = subset(graph.df, variable == "berry"), aes(x = factor(variable)), size = 3.5, hjust = 1.0) +
          theme_bw() +
            opts(title=title, legend.position = "none", panel.border = theme_blank(), axis.ticks = theme_blank()) +
              scale_colour_identity() + 
                scale_x_discrete(breaks = levels(graph.df$variable), labels = c("berry", "earthy")) +
                  scale_y_continuous(breaks = NA, trans = "reverse") + xlab(NULL) + ylab(NULL)
  ggsave(filename)
}
output.cloud <- function(wine, treatment, d, filename="wordcloud.png") {
  crit <- d$wine == wine & d$treatment == treatment
  pal <- brewer.pal(8,"Dark2")
  png(filename, width=1280,height=800)
  wordcloud(d$variable[crit],d$value[crit],
            scale=c(8,.2),
            min.freq=1,
            max.words=Inf,
            random.order=FALSE,
            ordered.colors=TRUE,
            rot.per=.0,
            colors=as.character(words$color))
  dev.off()
}

experiment.three <- read.table("actual/survey_3.csv", header = TRUE, sep=",")
words            <- read.table("samples/descriptors.csv", header = TRUE, sep=",")
raw.words        <- read.table("actual/survey_3.csv", header = TRUE, sep=",")[,-3]

exp.melt <- melt(experiment.three, id.vars = c("treatment", "wine"), fun=sum)
word.freq.table <- cast(wine + treatment ~ variable | value, data=exp.melt, fun.aggregate=sum)$`1`

wine.D.berry.ranks  <- treatment.ranks(1, word.freq.table)
wine.D.earthy.ranks <- treatment.ranks(4, word.freq.table)
wine.E.berry.ranks  <- treatment.ranks(2, word.freq.table)
wine.E.earthy.ranks <- treatment.ranks(3, word.freq.table)

by.treatment <- rowsum(experiment.three, experiment.three[,1])[,-(non.word.columns)]
by.wine <- rowsum(experiment.three, experiment.three[,2])[,-(non.word.columns)]

wine.D.ranks <- word.ranks(wine.D.berry.ranks, wine.D.earthy.ranks)
wine.E.ranks <- word.ranks(wine.E.berry.ranks, wine.E.earthy.ranks)

wine.berry.to.earthy.ranks <- word.ranks(wine.E.berry.ranks, wine.D.earthy.ranks)

berry.word <- treatment.ranks(1, data.frame(t(apply(data.frame(word.freq.table[1:2, ]),2, sum))))
earthy.word <- treatment.ranks(1, data.frame(t(apply(data.frame(word.freq.table[3:4, ]),2, sum))))
wine.berry.to.earthy.total.ranks <- word.ranks(berry.word, earthy.word)

colnames(raw.words) <- c(colnames(raw.words)[1:2],
                         as.character(words[as.integer(sub("X","",colnames(raw.words)[-(1:2)])),1]))
raw.words.melt <- melt(raw.words, id.vars = c("treatment", "wine"), fun=sum)
raw.words.melt.cast <- cast(wine + treatment ~ variable | value, data=raw.words.melt, fun.aggregate=sum)$`1`
word.freq.df <- melt(raw.words.melt.cast)

# charting the rank charts
rank.chart(wine.D.ranks, "Wine D Word Usage Rank Given Association", "actual/word-rank-wine-d.pdf")
rank.chart(wine.E.ranks, "Wine E Word Usage Rank Given Association", "actual/word-rank-wine-e.pdf")

rank.chart(wine.berry.to.earthy.ranks, "Word Usage Rank Given Association", "actual/word-rank-berry-v-earthy.pdf")

rank.chart(wine.berry.to.earthy.total.ranks, "Word Usage Rank Given Association", "actual/word-rank-berry-v-earthy-total.pdf")

#wine, treatment
output.cloud(1, 1, word.freq.df, "actual/Wine D as Berry.png")
output.cloud(1, 2, word.freq.df, "actual/Wine E as Berry.png")

output.cloud(2, 1, word.freq.df, "actual/Wine E as Earthy.png")
output.cloud(2, 2, word.freq.df, "actual/Wine D as Earthy.png")

# basic barcharts
raw.words.melt.cast

# overall total word choice
graph.freq.df <- melt(apply(raw.words.melt.cast, 2, sum))
graph.freq.df$variable <- factor(rownames(graph.freq.df))
graph.freq.df$variable <- factor(graph.freq.df$variable, levels=as.character(graph.freq.df[with(graph.freq.df, order(value)), ]$variable), ordered=TRUE)
colors <- as.character(words[order(graph.freq.df$variable),]$color)
ggplot(graph.freq.df, aes(variable, value, fill=variable, colour=variable)) +
  geom_bar(stat="identity") +
  theme_bw() +
  scale_colour_manual(values=colors) +
  scale_fill_manual(values=colors) +
  xlab("Descriptors") +
  ylab("Frequency of Usage") +
  opts(legend.position = "none", axis.title.x=theme_text(vjust=0))
ggsave("actual/Word Frequency.pdf", width=18,height=12)

# priming on word choice bar charts
raw.words.melt.cast$wine.letter <- factor(c("D", "E", "E", "D"))
raw.words.melt.cast$wine.type <- factor(c("Berry", "Berry", "Earthy", "Earthy"))
raw.words.melt.cast$coherence <- factor(c("Truth", "False", "Truth", "False"), levels=c("Truth", "False"), ordered=TRUE)
priming.graph.df <- melt(data.frame(raw.words.melt.cast[,-(c(1:3))]), id=15:17)
priming.graph.df$word.type <- c(rep("Earthy", 4),
                                rep("Neutral", 4),
                                rep("Neutral", 4),
                                rep("Earthy", 4),
                                rep("Berry", 4),
                                rep("Neutral", 4),
                                rep("Berry", 4),
                                rep("Earthy", 4),
                                rep("Earthy", 4),
                                rep("Berry", 4),
                                rep("Neutral", 4),
                                rep("Earthy", 4),
                                rep("Earthy", 4),
                                rep("Berry", 4))
priming.graph.colors <- as.character(words[mapply(function(x){ return(sub("\\.", " ", x)) }, levels(priming.graph.df$variable)), ]$color)

ggplot(priming.graph.df, aes(wine.type, value, fill=variable, colour=variable)) +
  geom_bar(stat="identity") +
  facet_grid(word.type ~ variable) +
  theme_bw() +
  scale_colour_manual(values=priming.graph.colors) +
  scale_fill_manual(values=priming.graph.colors) +
  xlab("What was told") +
  ylab("Frequency of Usage") +
  opts(legend.position = "none", axis.title.x=theme_text(vjust=0))
ggsave("actual/by-word-type.pdf")

# preference count visualizations and analysis
preferences <- experiment.three[experiment.three$pref == 1, ]
preferences$wine.type <- mapply(function(w,t){
  if(w==2 && t==1) return("Earthy")
  if(w==1 && t==1) return("Berry")
  if (w==1 && t==2) return("Berry")
  if (w==2 && t==2) return("Earthy")
},
  preferences$wine, preferences$treatment)

colnames(preferences) <- c("treatment", "wine", "pref",
                           as.character(words$word), "wine.type")

m.pref <- melt(preferences[,-(1:3)], id=16)

# probability of word use | preference
c.pref <- cast(wine.type ~ variable | value, data = m.pref, fun.aggregate=sum)$`1`
colnames(c.pref) <- c("wine.type", as.character(words$word))

v.pref <- cast(variable ~ wine.type | value, data = m.pref, fun.aggregate=sum)$`1`
v.pref$variable <- as.character(words$word)

n.pref <- data.frame(berry=(v.pref$Berry + 1)/(colSums(v.pref)["Berry"] + 15), earthy=(v.pref$Earthy + 1)/(colSums(v.pref)["Earthy"] + 15))
nb.model.pref <- log(n.pref)

m.pref$variable <- factor(m.pref$variable, levels=c("Black cherry", "Red fruits", "Juicy", "Plum", "Sweet", "Hearty", "Bright", "Spicy", "Peppery", "Complex", "Smokey", "Cedar", "Rustic", "Woodsy", "Earthy"), ordered=TRUE)

m.pref.color <- as.character(words[as.character(cast(variable ~ value, data=m.pref, fun.aggregate=sum)$variable), ]$color)

ggplot(m.pref, aes(variable, y=value, fill=variable, color=variable)) +
  facet_grid(. ~ wine.type) +
  geom_bar(stat="identity") +
  scale_colour_manual(values=m.pref.color) +
  scale_fill_manual(values=m.pref.color) +
  theme_bw() +
  opts(legend.position = "none", axis.title.x=theme_text(vjust=0))
ggsave("actual/words-given-pref.pdf")

ggplot(m.pref, aes(wine.type, y=value, fill=variable, color=variable)) +
  facet_grid(. ~ variable) +
  geom_bar(stat="identity") +
  scale_colour_manual(values=m.pref.color) +
  scale_fill_manual(values=m.pref.color) +
  theme_bw() +
  opts(legend.position = "none", axis.title.x=theme_text(vjust=0))
ggsave("actual/pref-given-words.pdf")

# preference counts when told the truth vs false
word.freq.table$wine.letter <- factor(c("D", "E", "E", "D"))
word.freq.table$wine.type <- factor(c("Berry", "Berry", "Earthy", "Earthy"))
word.freq.table$coherence <- factor(c("Truth", "False", "Truth", "False"), levels=c("Truth", "False"), ordered=TRUE)
ggplot(word.freq.table,
       aes(x=wine.letter,
           y=pref,
           fill=factor(wine.letter))) +
  facet_grid(. ~ coherence) +
  geom_bar(stat="identity") +
  theme_bw()
ggsave("actual/pref-count.pdf")
