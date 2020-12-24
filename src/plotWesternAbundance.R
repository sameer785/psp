require(ggplot2)
require(ggbeeswarm)
source("~/Google Drive/psp/functions/setTheme.R")


dat <- read.csv("~/Google Drive/psp/westerns/quantifications/Actb.csv",  stringsAsFactors = TRUE)
dat$genotype <- relevel(dat$genotype, ref = "WT")
datSum <-dat %>% group_by(genotype) %>% summarize(mean = mean(abundance), n = n())
wt.mean <- datSum %>% filter(genotype=="WT") %>% select(mean)
dat$abundance <- dat$abundance/wt.mean$mean

res.aov3 <- aov(abundance~genotype, data = dat)
summary(res.aov3)
pairwise.t.test(dat$abundance, dat$genotype, p.adjust.method = 'holm')


dodgeBy <- 0.5
gp <- ggplot(dat, aes(x=genotype, y=abundance, fill=genotype, color=genotype, group=genotype, shape=genotype))
gp <- gp + th
gp <- gp + stat_summary(fun=mean, geom="col", na.rm=TRUE, width=.5, size=.75, color='black', alpha=.75, show.legend = FALSE, position=position_dodge(dodgeBy))
gp <- gp + geom_beeswarm(size=5, color='black', stroke=.5, alpha=.5, show.legend = FALSE, dodge.width=dodgeBy)
gp <- gp + stat_summary(fun.data=mean_se, na.rm = TRUE, geom="errorbar", width=.15, size=2, color='black', alpha=.9, show.legend = FALSE, position=position_dodge(dodgeBy))
gp <- gp + stat_summary(fun=mean, na.rm=TRUE, geom="point", size=6, color='black', alpha=1, show.legend = TRUE, stroke=1.5, position=position_dodge(dodgeBy))
gp <- gp + ylab(expression(atop("Immunoreactivity", paste("% of WT"))))
gp <- gp + scale_color_brewer(palette = "Paired")
gp <- gp + scale_shape_manual(values=c(21,22,23))
gp <- gp + xlab("")
gp <- gp + theme(legend.position = c(0.5,.95))
gp <- gp + scale_y_continuous( breaks = scales::pretty_breaks(n = 10), expand = expansion(mult = c(0, .1)))
gp
#gp <- gp + coord_cartesian(expand = TRUE)
#gp <- gp + theme(axis.title.y = element_text(margin = margin(t = 0, r = -10, b = 0, l = 0)))
#gp <- gp + theme(axis.text.y=element_text(angle=0, hjust=1), axis.text.x=element_text(angle=0, hjust=1))




