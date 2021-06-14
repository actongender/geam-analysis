print_xtab <- function(data, var1, var2, hlabels=c("tophead", "lefthead"), caption=" "){

    xt <-  xtabs(formula = ~ data[[var1]] + data[[var2]])
    
    xtab <- xtable(addmargins(xt))
    
    # chqui squared 
    cst <- tryCatch({
        chisq.test(data[[var1]], data[[var2]])            
    }, 
    error = function(e){
        cst <- data.frame(statistic=NA, p.value=NA, parameter=NA)
        return(cst)
    }) 
    
    #fisher test
    fpval <- tryCatch({
        fst <- fisher.test(data[[var1]], data[[var2]])
        round(fst$p.value, 3)
        
    }, 
    error = function(e){
        return(" WARNING! ")
    })
    
    
    stat_chi    <- paste0("Chi-squared=", round(cst$statistic,3), " · p.val=", round(cst$p.value,3) ," · df=", cst$parameter)
    stat_fisher <- paste0("Fisher's p.val=", fpval)
    
    stat_chi_fisher <- paste0(stat_chi, " · ",  stat_fisher)
    
    
    tmp <- xtab %>% 
        kbl(booktabs=T, caption=caption, position="h") %>% 
        add_header_above(header=c(" ", setNames(2, hlabels[1]), " ")) %>% 
        pack_rows(setNames(hlabels[2], 1), 1,nrow(xtab)) %>% 
        add_footnote(c(stat_chi, stat_fisher), notation="number", )
    
    return(tmp)
}