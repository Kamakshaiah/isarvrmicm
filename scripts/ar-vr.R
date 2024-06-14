# https://www.clres.com/ca/pdepca01a.html
# IS-AR-VR-mktg_innovation [arvr mediation]

library(tm) #load text mining library


path <- readline() #D:\Research\PAPERS\WORKING\marketing\AR-VR-MR
# D:\Research\PAPERS\WORKING\AR-VR-MR\ 

pathch <- gsub('\\\\','//', path)
setwd(path)
getwd()

arvrfile <- read.csv(file.path(pathch, "data/main.csv"))
names(arvrfile)
arvrabs <- arvrfile["Abstract"]
length(t(arvrabs)) # 168

arvrcorp <- VCorpus((VectorSource(t(arvrabs))))

abstracts_corp <- tm_map(arvrcorp, stripWhitespace)
abstracts_corp <- tm_map(abstracts_corp, removePunctuation)
abstracts_corp <- tm_map(abstracts_corp, content_transformer(tolower))                          
abstracts_corp <- tm_map(abstracts_corp, removeWords, stopwords("english"))
abstracts_corp <- tm_map(abstracts_corp, removeNumbers)

# abstracts_corp <- tm_map(hc_corp, stemDocument, language = "english")

path <- readline() # D:\Research\PAPERS\WORKING\marketing\ARVR
# D:\Research\PAPERS\WORKING\AR-VR-MR
pathch <- gsub('\\\\','//', path)
setwd(path)
getwd()
dir.create('outputs')
# list.dirs()

pathchout <- file.path(pathch, 'outputs')
setwd(pathchout)
getwd()

write.csv(summary(abstracts_corp), 'corpus-summary.csv')

adtm <- DocumentTermMatrix(abstracts_corp)
tm::inspect(adtm[10:16, ] )
dim(adtm) # [1]  168 5037
adtm$dimnames

arvr_ <- adtm[, -c(1:25)]
dim(arvr_) # [1]  168 5012

arvrdfdense <- data.frame(as.matrix(removeSparseTerms(arvr_, 0.95)))
dim(arvrdfdense) #[1] 168 475
length(names(arvrdfdense))

write.csv(arvrdfdense, file.path(pathch, 'tm-data.csv'))


arvrdf <- subset(arvrdfdense, select = c(marketing, innovation, augmented, reality, virtual, 
                                         advertising, algorithms, applications, artificial, intelligence, 
                                         business, collaborative, community, cultural, database, databases, 
                                         decision, science, scientific, digital, distributed, computing, ecommerce, 
                                         economic, education, educational, electronic, engineering, environment, 
                                         environment, environments, geographic, information, systems, health, 
                                         infrastructure, innovation, innovative, internet, machine, learning, 
                                         management, market, media, medical, methodology, methods, mobile, mobility, 
                                         model, modeling, modelling, models, monitoring, motion, multimedia, 
                                         navigation, natural, nature, neural, network, networks, online, operation,
                                         quality, real, sales, consumer, satisfaction, spatial, strategy, strategies, 
                                         technological, technologies, technology, tourism, tourist, tourists))

length(names(arvrdf)) # 78


write.csv(arvrdf, file.path(pathch, 'df-for-analysis-long.csv'))

library(FactoMineR)

fit <- CA(data.frame(arvrdf))

path <- readline()
pathca <- gsub('\\\\','//', path)
setwd(pathca)
getwd()

write.csv(fit$row$coord, file.path(pathca, 'row-coord.csv'))
write.csv(fit$row$cos2, file.path(pathca, 'row-cos2.csv'))
write.csv(fit$row$contrib, file.path(pathca, 'row-contrib.csv'))
write.csv(fit$row$inertia, file.path(pathca, 'row-inertia.csv'))

write.csv(fit$col$coord, file.path(pathca, 'col-coord.csv'))
write.csv(fit$col$cos2, file.path(pathca, 'col-cos2.csv'))
write.csv(fit$col$contrib, file.path(pathca, 'col-contrib.csv'))
write.csv(fit$col$inertia, file.path(pathca, 'col-inertia.csv'))

plot.CA(fit, xlim = c(-0.2, 0), ylim = c(-0.16, 0)) #q11
plot.CA(fit, xlim = c(0, 0.5), ylim = c(-0.75, 0)) #q12
plot.CA(fit, xlim = c(0, 1), ylim = c(-0.75, 0)) #q2
plot.CA(fit, xlim = c(0.5, 1), ylim = c(0.5, 0.75)) #q21
plot.CA(fit, xlim = c(-0.5, 0), ylim = c(0, 0.5)) #q21
plot.CA(fit, xlim = c(-1, -0.5), ylim = c(0.5, 1.25)) #q21

# summary

desc_anal <- sapply(arvrdf, function(x) c(summary(x), type = class(x)))
write.csv(pathca, file = file.path(pathca, 'summary-df.csv'))

names(arvrdf)

# regression analysis (modeling)
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/#:~:text=The%20stepwise%20regression%20(or%20stepwise,model%20that%20lowers%20prediction%20error.


# FA

avrfadf <- subset(arvrdfdense, select = c(economic, environment, intelligence, technology, technologies,
                                          health, business, management, methods, models, decision, strategy,
                                          modelling, neural, networks, information, systems,
                                          database, databases, algorithms, education, engineering, 
                                          medical, science, applications, spatial, ecommerce, 
                                          innovation, online, augmented, reality, virtual, 
                                          geographic, navigation, mobile, online, sales, marketing, 
                                          collaborative, strategy))
# data_summary <- data.frame(unclass(summary(avrfadf)), check.names = FALSE)
# write.csv(data_summary, 'summary-stat.csv')
data_summary <- t(data.frame(do.call(cbind, lapply(avrfadf, summary))))
write.csv(data_summary, 'summary-stat.csv')

library(psych)

fafit <- fa(avrfadf, 4)
structure.diagram(fafit)

write.csv(loadings(fafit), '4-fa-loadings.csv')
write.csv(fafit$r.scores, '4-fa-r-scores.csv')
structure.diagram(fit)


library(lavaan)

model <- ' # direct effect
    
      f1 =~ marketing + innovation 
      f2 =~ collaborative + marketing 
      f3 =~ augmented + reality
      f4 =~ virtual + reality

    
    f2 ~ c*f1
    
    # mediator
    f3 ~ a*f1
    f4 ~ b*f3
    f2 ~ d*f4
    # f1 ~ e*f2
    
    # direct effect
    direct := c
    
    # indirect effect 
    indirect := a*b*d
    
    # total effect
    total := c + (a*b*d)

'

fit <- sem(model, data = avrfadf)
summary(fit)

# https://cran.r-project.org/web/packages/semptools/vignettes/semptools.html
write.csv(data.frame(parameterEstimates(fit)), 'cfa-parameters.csv')
write.csv(fitMeasures(fit), 'cfa-fit-measures.csv')

library(semPlot)
library(semptools)

p_pa <- semPaths(fit, whatLabels = "est",
                 sizeMan = 10, 
                 edge.label.cex = 1.15, layout = 'spring',
                 ) # spring col = 'pink'  style = 'lisrel'

p_pa2 <- mark_sig(p_pa, fit)
plot(p_pa2)


library(seminr)

meas_mod <- constructs(
  composite("cm", c('collaborative', 'marketing')),
  composite("mi", c('marketing', 'innovation')),
  composite("arvr", c('augmented', 'reality', 'virtual', 'reality')),
  composite("ist", c('information', 'systems', 'technology', 'neural', 'networks', 'database', 'databases', 'algorithms')), 
  composite("apro", c('methods', 'models', 'decision', 'strategy', 'modelling'))
  )

struc_mod <- relationships(
  paths(from = c("mi"), to = c("cm")), 
  paths(from = c("arvr"), to = c("mi")),
  paths(from = c("arvr"), to = c("cm")),
  paths(from = c("ist"), to = c("mi")), 
  paths(from = c("ist"), to = c("cm")), 
  paths(from = c("apro"), to = c("cm")), 
  paths(from = c("apro"), to = c("mi"))
)

fit <- estimate_pls(avrfadf, 
                    measurement_model = meas_mod, 
                    structural_model = struc_mod, 
                    missing = mean_replacement,
                    missing_value = "-99"
                    )
plot(fit)

sumfit <- summary(fit)
sumfit$loadings^2
setwd(file.path(getwd(), 'plssem'))
# class(sumfit$loadings)
write.csv(sumfit$paths, 'plssem-path-coef.csv')
write.csv(sumfit$reliability, 'plssem-realiability.csv')
write.csv(sumfit$loadings, 'plssem-loadings.csv')

bootstrmod <- bootstrap_model(fit, nboot=10)
sumbootstrmod <- summary(bootstmod, apha = 0.05)
write.csv(sumfit$loadings^2, 'indicator-validity.csv') # indicator validity
write.csv(as.matrix(sumfit$validity$vif_items), 'indicator-collinearity.csv') # collinearity
write.csv(sumfit$reliability, 'model-realiability.csv')
write.csv(sumfit$validity$fl_criteria, 'disc-val-flcri.csv')
write.csv(sumbootstrmod$bootstrapped_HTMT, 'disc-val-htmt.csv')

plot(bootstmod)

write.csv(sumbootstrmod$bootstrapped_total_paths, 'bootstr-paths.csv')
write.csv(sumbootstrmod$bootstrapped_loadings, 'bootstr-loadings.csv')

# # mediation
# 
# sumfit$total_indirect_effects
# 
# specific_effect_significance(
#   bootstrmod, 
#   from = "mi",
#   through = c('ist'),
#   to = "cm",
#   alpha = 0.05
# )

# # interaction
# meas_mod <- constructs(
#   composite("cm", c('collaborative', 'marketing')),
#   composite("mi", c('marketing', 'innovation')),
#   composite("arvr", c('augmented', 'reality', 'virtual', 'reality')),
#   composite("ist", c('information', 'systems', 'technology', 'neural', 'networks', 'database', 'databases', 'algorithms')), 
#   composite("apro", c('methods', 'models', 'decision', 'strategy', 'modelling')),
#   interaction_term(iv="mi", moderator = "ist"), 
#   interaction_term(iv = "mi", moderator = "arvr"), 
#   interaction_term(iv="mi", moderator = "apro") 
# )
# 
# 
# 
# struc_mod <- relationships(
#   paths(from = c("mi"), to = c("cm")), 
#   paths(from = c("arvr"), to = c("mi", "cm")),
#   paths(from = c("ist"), to = c("mi", "cm")), 
#   paths(from = c("apro"), to = c("cm", "mi")), 
#   paths(from = c("mi", "ist", "mi*ist"), to = c("cm")), 
#   paths(from = c("mi", "arvr", "mi*arvr"), to = c("cm")), 
#   paths(from = c("mi", "apro", "mi*arvr"), to = c("cm")) 
# )
# 
# fit <- estimate_pls(avrfadf, 
#                     measurement_model = meas_mod, 
#                     structural_model = struc_mod, 
#                     missing = mean_replacement,
#                     missing_value = "-99"
# )
# plot(fit)
# 
# bootstrmod <- bootstrap_model(fit, nboot=10)
# sumbootstrmod <- summary(bootstmod, apha = 0.05)
# 
# # mediation
# sumfit <- summary(fit)
# sumfit$total_indirect_effects
# 
# specific_effect_significance(
#   bootstrmod, 
#   from = "mi",
#   through = c('ist'),
#   to = "cm",
#   alpha = 0.05
# )
