#suspectdf <- downloaded_2004allde_mp[ which( downloaded_2004allde_mp$drugcharacterization==1) , ]
#suspect20040331df <- downloaded_2004allde_mp[ which( downloaded_2004allde_mp$quarterdate == 'Wed Mar 31 00:00:00 EST 2004' ) , ]

# noslash <- sub( '//' , ';', s_de_as_ns$activesubstancename , fixed=TRUE)
# noslash <- sub( '\\' , ';', s_de_as_ns$activesubstancename , fixed=TRUE)
# s_de_as_ns[, 1] <- noslash
# s_de_as_nf_all <- aggregate(s_de_as_ns$freq ~ s_de_as_ns$activesubstancename + s_de_as_ns$reactionmeddrapt, FUN='sum')

suspectfreqdf <- aggregate(s_de_as$freq ~ s_de_as$activesubstancename + s_de_as$reactionmeddrapt, FUN='sum')
names( suspectfreqdf) <- c('d' , 'e', 'A')
suspectfreqdf <- suspectfreqdf[ which( suspectfreqdf$e != ""), ]

drugfreqdf <- aggregate( suspectfreqdf$A ~ suspectfreqdf$d, FUN='sum')
eventfreqdf <- aggregate( suspectfreqdf$A ~ suspectfreqdf$e, FUN='sum')
ABCD  <- sum( reportidcounts$freq )

names( drugfreqdf) <- c('d' , 'AB')
names( eventfreqdf) <- c('e' , 'AC')
suspectfreqdf <- ( merge(suspectfreqdf, drugfreqdf, by=c('d')))
suspectfreqdf <- ( merge(suspectfreqdf, eventfreqdf, by=c('e')))
suspectfreqdf <- data.frame( suspectfreqdf, C = suspectfreqdf$AC - suspectfreqdf$A ,CD = ABCD-suspectfreqdf$AB)
suspectfreqdf <- data.frame( suspectfreqdf, PRR = (suspectfreqdf$A / suspectfreqdf$AB) / (suspectfreqdf$C/suspectfreqdf$CD))
#drugfreqdf <- data.frame( drugfreqdf, d_frac = drugfreqdf$n/totalfreq)
#eventfreqdf <- data.frame( eventfreqdf, e_frac = eventfreqdf$n/totalfreq)
# curinds <- which( s_de_as_all$`s_de_as$activesubstancename` == 'PAROXETINE HYDROCHLORIDE')
# curinds2 <- which(  s_de_as_all$`s_de_as$activesubstancename` == 'PAROXETINE HYDROCHLORIDE')
# asnames <- unique(s_de_as_all$`s_de_as$activesubstancename`)
