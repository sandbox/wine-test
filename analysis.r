experiment.one <- read.table("samples/survey_1_2.csv", header = TRUE, sep=",")

# Tables for the follow the herd experiment
AB.herd <- matrix(c(
                    # control A       , treatment A added
                    experiment.one[c(1,3),3] - experiment.one[c(1,3), 5],
                    # control B       , treatment B
                    experiment.one[c(1,3),4]
                    ), nr=2, dimnames=list(c("control", "treatment"), c("A", "B")))

AC.herd <- matrix(c(
                    # control A        , treatment A
                    experiment.one[2,3], experiment.one[4,4],
                    # control C        , treatment C added
                    experiment.one[2,4], experiment.one[4,3] - experiment.one[4,5]
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
