library("ggplot2")

experiment.one <- read.table("samples/survey_1_2.csv", header = TRUE, sep=",")

# Tables for the follow the herd experiment
AB.herd <- matrix(c(
                    # control A       , treatment A added
                    experiment.one[c(1,3),3] - experiment.one[c(1,3), 6],
                    # control B       , treatment B
                    experiment.one[c(1,3),4]
                    ), nr=2, dimnames=list(c("control", "treatment"), c("A", "B")))

AC.herd <- matrix(c(
                    # control A        , treatment A
                    experiment.one[2,3], experiment.one[4,4],
                    # control C        , treatment C added
                    experiment.one[2,4], experiment.one[4,3] - experiment.one[4,6]
                    ), nr=2, dimnames=list(c("control", "treatment"), c("A", "C")))

# Tables for the follow the vip special access experiment
AB.special <- matrix(c(
                       # control A        , treatment A
                       experiment.one[1,3], experiment.one[5,3],
                       # control B , treatment B special
                       experiment.one[1,4], experiment.one[5,4]
                       ), nr=2, dimnames=list(c("control", "treatment"), c("A", "B")))

AC.special <- matrix(c(
                       # control A        , treatment A
                       experiment.one[2,3], experiment.one[6,3],
                       # control C        , treatment C special
                       experiment.one[2,4], experiment.one[6,4]
                       ), nr=2, dimnames=list(c("control", "treatment"), c("A", "C")))

# Running Analysis on the experiment 1 and 2 results
#   the fisher test produces a more accurate value for p than the chisq.test
#   feel free to change this to chisq.test though
AB.herd.result <- fisher.test(AB.herd)
AC.herd.result <- fisher.test(AC.herd)
AB.special.result <- fisher.test(AB.special)
AC.special.result <- fisher.test(AC.special)

# if p.value < some alpha, then there is a significant association between
#   seeing more votes for one wine and wine preference
#   => to some extent people follow the herd when making judgments of taste
alpha <- 0.05
if(AB.herd.result$p.value < alpha){ "some herdiness" }else{ "unlikely to have herdiness"}
if(AC.herd.result$p.value < alpha){ "some herdiness" }else{ "unlikely to have herdiness"}

#   having a vip special access deal and wine preference
#   => to some extent people are affected by the allusion of special treatment
if(AB.special.result$p.value < alpha){ "vip matters" }else{ "unlikely that vip matters"}
if(AC.special.result$p.value < alpha){ "vip matters" }else{ "unlikely that vip matters"}

dist.AB.herd <- AB.herd / apply(AB.herd, 1, sum)
dist.AC.herd <- AC.herd / apply(AC.herd, 1, sum)
dist.AB.special <- AB.special / apply(AB.special, 1, sum)
dist.AC.special <- AC.special / apply(AC.special, 1, sum)

# still unsure how to show the proportion on this
#   (additional votes) / (total real vote sum at the station)
# proportion is calculated from within the experiment
added.to.A.treatment <- experiment.one[3,6] / sum(experiment.one[3,3:5])
added.to.C.treatment <- experiment.one[4,6] / sum(experiment.one[4,3:5])

# These bar charts don't look very good.
barplot(dist.AB.herd, main="Follow the Herd? Distribution of Wine Preference",
        xlab="Wine Tasted",
        legend = rownames(AB.herd), beside=TRUE)
barplot(dist.AC.herd, main="Follow the Herd? Distribution of Wine Preference",
        xlab="Wine Tasted",
        legend = rownames(AC.herd), beside=TRUE)
barplot(dist.AB.special, main="VIP syndrome: Distribution of Wine Preference",
        xlab="Wine Tasted",
        legend = rownames(AB.special), beside=TRUE)
barplot(dist.AC.special, main="VIP syndrome: Distribution of Wine Preference",
        xlab="Wine Tasted",
        legend = rownames(AC.special), beside=TRUE)

# ggplot bar charts (need to add titles and other things legend markers)
nice.plot <- function(exptable, filename="barplot.pdf", fillvals=c("#00C0c3", "#fa736f"), added_votes=c(0,0,0,0)) {
  df = data.frame(prop=c(exptable[,1],
                    exptable[,2],
                    added_votes),
    group=rownames(exptable),
    wine=rep(rep(colnames(exptable), each=2), 2))

  # do things to change color here or potentially even save to a pdf
  # switch group and wine in this line to change grouping (group by treatment or group by wine)
  ggplot(df, aes(wine, prop, fill=wine, colour=wine)) +
    geom_bar(stat="identity") +
      facet_grid(. ~ group) +
        scale_colour_manual(values=c("#000000","#000000")) +
          scale_fill_manual(values=fillvals) +
            theme_bw()
  ggsave(filename)
}

# pick the wine colors as represented in the bar graph
wine.A <- "#00C0c3"
wine.B <- "#fa736f"
wine.C <- "#e79d31"

                                        #                             votes added to a
                                        #                           0,0.22,0,0.0
nice.plot(dist.AB.herd, "experiment_1_AB.pdf", c(wine.A, wine.B), c(0,added.to.A.treatment,0,0))
                                        #                             votes added to c
                                        #                           0,0.00,0,0.1
nice.plot(dist.AC.herd, "experiment_1_AC.pdf", c(wine.A, wine.C), c(0,0,0,added.to.C.treatment))
nice.plot(dist.AB.special, "experiment_2_AB.pdf", c(wine.A, wine.B), c(0,0,0,0))
nice.plot(dist.AC.special, "experiment_2_AC.pdf", c(wine.A, wine.C), c(0,0,0,0))

######
#  ACTUAL ANALYSIS FOR BAR CHARTS SINCE THE STUFF CAME OUT VERY DIFFERENTLY
######
act.one <- read.table("actual/survey_1_2.csv", header = TRUE, sep=",")

A.merlot.normal <- act.one[1, 3]
C.pinot.special <- act.one[1, 5]

A.merlot.normal.two <- act.one[2, 3]
C.crap.special <- act.one[2, 5]

A.merlot.treatment <- act.one[3, 3]
B.crap.treatment <- act.one[3, 5]

A.merlot.control <- act.one[4, 3]
B.crap.control <- act.one[4, 5]

# experiment 1
AB.herd <- matrix(c(
                    A.merlot.control, A.merlot.treatment,
                    B.crap.control, B.crap.treatment
                    ), nr=2, dimnames=list(c("control", "treatment"), c("A", "B")))

nice.plot(AB.herd, "actual/experiment_1_AB.pdf", c(wine.B, wine.A))

# experiment 2
exp.special <- data.frame(value=c(
                            A.merlot.normal, C.pinot.special,
                            A.merlot.normal.two, C.crap.special,
                            A.merlot.control, B.crap.control),
                          station=rep(c("station 1", "station 2", "station 4"), each=2),
                          wine=c("A", "B", "A", "B", "A", "B"))

ggplot(exp.special,
       aes(x=wine,
           y=value,
           fill=factor(wine))) +
  facet_grid(. ~ station) +
  geom_bar(stat="identity") +
  theme_bw() +
  opts(legend.position = "none", axis.title.x=theme_text(vjust=0))
ggsave("actual/experiment_2_ABC.pdf")
