gray.palette <- function(n, lower=.3, upper=.7){
    s <- seq(lower, upper, length=n)
    rgb(matrix(rep(s, each=3), ncol=3, byrow=T))
}
ggplot.aldmck <- function(result, groupVar=NULL, addStim = FALSE, weights=c("all", "positive", "negative"), xlab=NULL, main = NULL, ylab=NULL,  ...){
    w <- match.arg(weights)
    shapes <- c(15,16,18, 24, 25, 0,1,2,3,4,5,6,7)
    v <- result$respondents
    if(!is.null(groupVar)){
        v$stimulus <- groupVar
    }
    v <- na.omit(v)
    xl <- ifelse(is.null(xlab), "Ideal Points", xl)
    yl <- ifelse(is.null(ylab), "Density", yl)
    main <- ifelse(is.null(main), "", main)
    if(is.null(groupVar)){
        if(w == "all"){
        g <- ggplot(v, aes(x=idealpt)) +  xlab(xl) + ylab(yl) + ggtitle(main) +stat_density(geom="line") + theme_bw()
        }
        if(w == "positive"){
            posv <- v[which(v$weight > 0),]
            xl <- paste0(xl, " (n=", nrow(posv), ")")
            g <- ggplot(posv, aes(x=idealpt)) +  xlab(xl) + ylab(yl) + ggtitle(main) + stat_density(geom="line")  + theme_bw()
        }
        if(w == "negative"){
            negv <- v[which(v$weight < 0),]
            xl <- paste0(xl, " (n=", nrow(negv), ")")
            g <- ggplot(negv, aes(x=idealpt)) +  xlab(xl) + ylab(yl) + ggtitle(main)+ stat_density(geom="line")  + theme_bw()
        }
    }
    else{
        ng <- length(table(v$stimulus))
        if(w == "all"){
            props <- table(v$stimulus)/sum(table(v$stimulus))
            bd <- by(v$idealpt, list(v$stimulus), density)
            for(i in 1:length(bd)){
                bd[[i]]$y <- bd[[i]]$y*props[i]
                bd[[i]]$stimulus <- factor(i, levels=1:length(props), levels(v$stimulus))
            }
            bd <- lapply(bd, function(z)data.frame("idealpt" = z$x, "Density"=z$y, "stimulus"=z$stimulus))
            bd <- do.call(rbind, bd)
            g <- ggplot(bd, aes(x=idealpt, y=Density, group=stimulus, color=stimulus)) + geom_line() + scale_color_manual(values=gray.palette(ng)) +
                 xlab(xl) + ylab(yl) + ggtitle(main) + theme_bw()
        }
        if(w == "positive"){
            posv <- v[which(v$weight > 0),]
            xl <- paste0(xl, " (n=", nrow(posv), ")")
            props <- table(posv$stimulus)/sum(table(posv$stimulus))
            bd <- by(posv$idealpt, list(posv$stimulus), density)
            for(i in 1:length(bd)){
                bd[[i]]$y <- bd[[i]]$y*props[i]
                bd[[i]]$stimulus <- factor(i, levels=1:length(props), levels(posv$stimulus))
            }
            bd <- lapply(bd, function(z)data.frame("idealpt" = z$x, "Density"=z$y, "stimulus"=z$stimulus))
            bd <- do.call(rbind, bd)
            g <- ggplot(bd, aes(x=idealpt, y=Density, group=stimulus, color=stimulus)) + geom_line() + scale_color_manual(values=gray.palette(ng)) +
                 xlab(xl) + ylab(yl) + ggtitle(main) + theme_bw()
        }
        if(w == "negative"){
            negv <- v[which(v$weight < 0),]
            xl <- paste0(xl, " (n=", nrow(negv), ")")
            props <- table(negv$stimulus)/sum(table(negv$stimulus))
            bd <- by(negv$idealpt, list(negv$stimulus), density)
            for(i in 1:length(bd)){
                bd[[i]]$y <- bd[[i]]$y*props[i]
                bd[[i]]$stimulus <- factor(i, levels=1:length(props), levels(negv$stimulus))
            }
            bd <- lapply(bd, function(z)data.frame("idealpt" = z$x, "Density"=z$y, "stimulus"=z$stimulus))
            bd <- do.call(rbind, bd)
            g <- ggplot(bd, aes(x=idealpt, y=Density, group=stimulus, color=stimulus)) + geom_line() + scale_color_manual(values=gray.palette(ng)) +
                 xlab(xl) + ylab(yl) + ggtitle(main) + theme_bw()
        }
    }
    if(addStim){
    tmp <- na.omit(result$stimuli)
    if(!is.null(groupVar)){
        tmp <- tmp[which(names(tmp) %in% unique(groupVar))]
        n <- names(tmp)
        p <- data.frame("idealpt" = tmp, "stimulus" = factor(n, levels=n[order(tmp)]))
        g <- g + geom_point(data=p, aes(y=0, group=stimulus, pch=stimulus, col=stimulus, size=2.5)) +
            scale_shape_manual(values=shapes[1:nrow(p)]) + theme_bw() + scale_size(2.5, guide=FALSE)
    }
    else{
        n <- names(tmp)
        p <- data.frame("idealpt" = tmp, "stimulus" = factor(n, levels=n[order(tmp)]))
        g <- g + geom_point(data=p, aes(y=0, group=stimulus, pch=stimulus, col=stimulus, size=2.5)) +
            scale_shape_manual(values=shapes[1:nrow(p)]) + scale_color_manual(values=gray.palette(nrow(p))) +
            theme_bw() + scale_size(2.5, guide=FALSE)

    }
    }
    g
}

boot.aldmck <- function(data, ..., boot.args=list(), plot=FALSE){
    dots.args <- match.call(expand.dots = FALSE)$`...`
    boot.fun <- function(data, inds, ...){
        tmp <- data[inds, ]
        dot.args$data <- tmp
        out <- do.call("aldmck", dot.args)
        out$stimuli
    }
    boot.args$data <- data
    boot.args$statistic=boot.fun
    boot.args$dot.args=dot.args
    b <- do.call("boot", boot.args)
    out <- data.frame(
            "stimulus"= factor(names(b$t0), levels=names(b$t0)[order(b$t0)]),
            "idealpt" = b$t0,
            "sd" = apply(b$t, 2, sd))
    rownames(out) <- NULL
    out$lower <- out$idealpt - 1.96*out$sd
    out$upper <- out$idealpt + 1.96*out$sd
    out <- out[order(out$idealpt), ]
    class(out) <- c("aldmck_ci", "data.frame")
    return(out)
}

plot.aldmck_ci <- function(x, ...){
    g <- ggplot(x, aes(x=idealpt, y=stimulus)) + geom_point() + geom_errorbarh(aes(xmin = lower, xmax=upper), height=0) + theme_bw()
    return(g)
}

bamPrep <- function(x, nmin=1, missing=NULL, self=1, midpt=NULL){
    x <- as.matrix(x)
    if(!is.numeric(x[,1])){stop("x must be a numeric data frame or matrix")}
    x[which(x %in% missing, arr.ind=T)] <- NA
    if(is.null(midpt)){
        x <- apply(x, 2, function(z)z-(min(z, na.rm=TRUE) + diff(range(z, na.rm=TRUE))/2))
    }
    else{
        x <- apply(x, 2, function(z)z-midpt)
    }
    nonmiss <- apply(x, 1, function(z)sum(!is.na(z)))
    x <- x[which(nonmiss >= nmin), ]
    out <- list(stims = x[,-self], self= x[,self])
    class(out) <- c("bamPrep", "list")
    out
}

BAM <- function(data, polarity, zhat=TRUE, ab=FALSE, resp.idealpts=FALSE, ...){
if(!("bamPrep" %in% class(data)))stop("Data should be output from the bamPrep function")
args <- match.call(expand.dots = FALSE)$`...`
if(!("n.chains" %in% names(args)))args$n.chains = 2
if(!("n.adapt" %in% names(args)))args$n.adapt = 10000
if(!("n.sample" %in% names(args)))n.sample = 5000
if(!("inits" %in% names(args))){
    orig <- aldmck(na.omit(data$stims), respondent=0, polarity=polarity, verbose=FALSE)
    args$inits <- function(){list(zhatstar = orig$stimuli + rnorm(length(orig$stimuli), 0, 1))}
}
args$file <- system.file("templates/BAM_JAGScode.bug", package="asmcjr")
args$data <- list('z'=data$stims, q = ncol(data$stims), N=nrow(data$stims))
mod.sim <- do.call("jags.model", args)

if(zhat & !ab){
    samples <- coda.samples(mod.sim,'zhat',  n.sample,  thin=1)
    zhat <- samples
    zhat.sum <- summary(zhat)
    zhat.ci <- data.frame("stimulus" = factor(colnames(data$stims), levels=colnames(data$stims)[order(zhat.sum$statistics)]),
                          "idealpt" = zhat.sum$statistics[,1],
                          "sd" = zhat.sum$statistics[,2],
                          "lower" = zhat.sum$quantiles[,1],
                          "upper" = zhat.sum$quantiles[,5])
    rownames(zhat.ci) <- NULL
    class(zhat.ci) <- c("aldmck_ci", "data.frame")
    res.list = list(zhat=zhat, zhat.ci = zhat.ci)
    }
if(ab & !zhat){
    samples <- coda.samples(mod.sim, c('a', 'b'),  n.sample,  thin=1)
    a <- samples[,grep("^a", colnames(samples[[1]]))]
    b <- samples[,grep("&b", colnames(samples[[1]]))]
    res.list = list(a=a, b=b)
}
if(ab & zhat){
    samples <- coda.samples(mod.sim, c('zhat', 'a', 'b'),  n.sample,  thin=1)
    zhat <- samples[,grep("^z", colnames(samples[[1]]))]
    zhat.sum <- summary(zhat)
    zhat.ci <- data.frame("stimulus" = factor(colnames(data$stims), levels=colnames(data$stims)[order(zhat.sum$statistics)]),
                          "idealpt" = zhat.sum$statistics[,1],
                          "sd"= zhat.sum$statistics[,2],
                          "lower" = zhat.sum$quantiles[,1],
                          "upper" = zhat.sum$quantiles[,5])
    rownames(zhat.ci) <- NULL
    class(zhat.ci) <- c("aldmck_ci", "data.frame")
    a <- samples[,grep("^a", colnames(samples[[1]]))]
    b <- samples[,grep("^b", colnames(samples[[1]]))]
    res.list  = list(zhat=zhat, zhat.ci = zhat.ci, a=a, b=b)
}
if(resp.idealpts){
    amat <- do.call(rbind, res.list$a)
    bmat <- do.call(rbind, res.list$b)
    diffs <- t(apply(amat, 1, function(x)data$self-x))
    resp.ideals <- diffs /bmat
    resp.ideal.summary <-  t(apply(resp.ideals, 2, quantile, c(.025, .5, .975), na.rm=T))
    resp.ideal.summary <- as.data.frame(resp.ideal.summary)
    names(resp.ideal.summary) <- c("lower", "median", "upper")
    res.list$resp.samples=resp.ideals
    res.list$resp.summary = resp.ideal.summary
}
invisible(res.list)
}

