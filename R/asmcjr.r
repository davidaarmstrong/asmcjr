utils:::globalVariables(c("x", "y", "group", "pch", "idealpt", "Density", "stimulus",  "lower", "upper"))
gray.palette <- function(n, lower=.3, upper=.7){
    s <- seq(lower, upper, length=n)
    rgb(matrix(rep(s, each=3), ncol=3, byrow=T))
}
ggplot.resphist <- function(result, groupVar = NULL, addStim = FALSE,
    scaleDensity=TRUE, weights = c("all", "positive", "negative"),
    xlab = NULL, main = NULL, ylab = NULL, whichRes = NULL,
    dim = NULL, ...){
    w <- match.arg(weights)
    shapes <- c(15,16,18, 24, 25, 0,1,2,3,4,5,6,7)
    if(class(result) == "aldmck"){
        v <- result$respondents
    }
    if(class(result) == "blackbox"){
        if(is.null(dim)){stop("For blackbox, 'dim' must be specified\n")}
        if(is.null(whichRes)){wres <- dim}
        else{wres <- whichRes}
        v <- data.frame("idealpt" = result$individuals[[wres]][,dim], "weight" = 1)
    }
    gv <- NULL
    if(!is.null(groupVar)){
        v$stimulus <- groupVar
        gv <- c(gv, "stimulus")
    }
    if(!is.null(facetVar)){
        v$facet <- facetVar
        gv <- c(gv, "facet")
    }
    v <- na.omit(v)
    xl <- ifelse(is.null(xlab), "Ideal Points", xlab)
    yl <- ifelse(is.null(ylab), "Density", ylab)
    main <- ifelse(is.null(main), "", main)
    if(is.null(groupVar)& is.null(facetVar)){
        if(w == "all"){
#        g <- ggplot(v, aes(x=idealpt)) +  xlab(xl) + ylab(yl) + ggtitle(main) +stat_density(geom="line") + theme_bw()
        }
        if(w == "positive"){
            posv <- v[which(v$weight > 0),]
            xl <- paste0(xl, " (n=", nrow(posv), ")")
#            g <- ggplot(posv, aes(x=idealpt)) +  xlab(xl) + ylab(yl) + ggtitle(main) + stat_density(geom="line")  + theme_bw()
        }
        if(w == "negative"){
            negv <- v[which(v$weight < 0),]
            xl <- paste0(xl, " (n=", nrow(negv), ")")
#            g <- ggplot(negv, aes(x=idealpt)) +  xlab(xl) + ylab(yl) + ggtitle(main)+ stat_density(geom="line")  + theme_bw()
        }
    }
    else{
        v$newg <- apply(v[,gv,drop=FALSE], 1, paste, collapse=";")
        ng <- length(table(v$newg))
        if(w == "all"){
            props <- table(v$newg)/sum(table(v$newg))
            bd <- by(v$idealpt, list(v$newg), density)
            lens <- sapply(bd, function(z)length(z$x))
            w0 <- which(lens == 0)
            if(length(w0) > 0){
                for(j in length(w0):1){bd[[w0[j]]] <- NULL}
            }
            for(i in 1:length(bd)){
                if(scaleDensity)bd[[i]]$y <- bd[[i]]$y*props[i]
                bd[[i]]$newg <- names(bd)[1]
            }
            bd <- lapply(bd, function(z)data.frame("idealpt" = z$x, "Density"=z$y, "newg"=z$newg))
            bd <- do.call(rbind, bd)
            t1 <- do.call(rbind, strsplit(as.character(bd$newg), split=";", fixed=T))
            t1 <- as.data.frame(t1)
            names(t1) <- c(gv)
            bd <- cbind(bd, t1)
#            g <- ggplot(bd, aes(x=idealpt, y=Density, group=stimulus, color=stimulus)) + geom_line() + scale_color_manual(values=gray.palette(ng)) +
#                 xlab(xl) + ylab(yl) + ggtitle(main) + theme_bw()
        }
        if(w == "positive"){
            posv <- v[which(v$weight > 0),]
            xl <- paste0(xl, " (n=", nrow(posv), ")")
            props <- table(posv$stimulus)/sum(table(posv$stimulus))
            bd <- by(posv$idealpt, list(posv$stimulus), density)
            lens <- sapply(bd, function(z)length(z$x))
            w0 <- which(lens == 0)
            if(length(w0) > 0){
                for(j in length(w0):1){bd[[w0[j]]] <- NULL}
            }
            for(i in 1:length(bd)){
                if(scaleDensity)bd[[i]]$y <- bd[[i]]$y*props[i]
                bd[[i]]$stimulus <- factor(i, levels=1:length(bd), labels=names(bd))
                }
            bd <- lapply(bd, function(z)data.frame("idealpt" = z$x, "Density"=z$y, "stimulus"=z$stimulus))
            bd <- do.call(rbind, bd)
 #           g <- ggplot(bd, aes(x=idealpt, y=Density, group=stimulus, color=stimulus)) + geom_line() + scale_color_manual(values=gray.palette(ng)) +
 #               xlab(xl) + ylab(yl) + ggtitle(main) + theme_bw()
        }
        if(w == "negative"){
            negv <- v[which(v$weight < 0),]
            xl <- paste0(xl, " (n=", nrow(negv), ")")
            props <- table(negv$stimulus)/sum(table(negv$stimulus))
            bd <- by(negv$idealpt, list(negv$stimulus), density)
            lens <- sapply(bd, function(z)length(z$x))
            w0 <- which(lens == 0)
            if(length(w0) > 0){
                for(j in length(w0):1){bd[[w0[j]]] <- NULL}
            }
            for(i in 1:length(bd)){
                if(scaleDensity)bd[[i]]$y <- bd[[i]]$y*props[i]
                bd[[i]]$stimulus <- factor(i, levels=1:length(bd), labels=names(bd))
            }
            bd <- lapply(bd, function(z)data.frame("idealpt" = z$x, "Density"=z$y, "stimulus"=z$stimulus))
            bd <- do.call(rbind, bd)
#            g <- ggplot(bd, aes(x=idealpt, y=Density, group=stimulus, color=stimulus)) + geom_line() + scale_color_manual(values=gray.palette(ng)) +
#                 xlab(xl) + ylab(yl) + ggtitle(main) + theme_bw()
        }
    }
    if(addStim){
    tmp <- na.omit(result$stimuli)
    if(!is.null(groupVar)){
        tmp <- tmp[which(names(tmp) %in% unique(groupVar))]
        n <- names(tmp)
        p <- data.frame("idealpt" = tmp, "stimulus" = factor(n, levels=n[order(tmp)]))
#        g <- g + geom_point(data=p, aes(y=0, group=stimulus, pch=stimulus, col=stimulus, size=2.5)) +
#            scale_shape_manual(values=shapes[1:nrow(p)]) + theme_bw() + scale_size(2.5, guide=FALSE)
    }
    else{
        n <- names(tmp)
        p <- data.frame("idealpt" = tmp, "stimulus" = factor(n, levels=n[order(tmp)]))
#        g <- g + geom_point(data=p, aes(y=0, group=stimulus, pch=stimulus, col=stimulus, size=2.5)) +
#            scale_shape_manual(values=shapes[1:nrow(p)]) + scale_color_manual(values=gray.palette(nrow(p))) +
#            theme_bw() + scale_size(2.5, guide=FALSE)
    }
    }
    if(!is.null(facetVar)){
#        g <- g + facet_wrap(~facet)
    }
    # return(g)
    if(exists("bd")){
        out <- list(data=bd)
    }
    else{
        out <- list(data=v)
    }
    if(exists("p")){
        out$stim_data <- p
    }
    return(out)
}

boot.aldmck <- function(data, ..., boot.args=list(), plot=FALSE){
    dot.args <- as.list(match.call(expand.dots = FALSE)$`...`)
    boot.fun <- function(data, inds, dot.args, ...){
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
    return(list(sumstats=out, bootres=b))
}

boot.blackbox <- function(data, missing, dims=1, minscale, verbose=FALSE, posStimulus = 1, R=100){
        dot.args <- as.list(match.call(expand.dots = FALSE)$`...`)
        orig <- blackbox(data, missing=missing, dims=dims, minscale=minscale, verbose=verbose)
        if(orig$individuals[[dims]][posStimulus, 1] < 0){
            orig$individuals[[dims]][,1] <- -orig$individuals[[dims]][,1]
        }
        sample.dat <- lapply(1:R, function(i)data[,sample(1:ncol(data), ncol(data), replace=TRUE)])
        for(i in 1:length(sample.dat))colnames(sample.dat[[i]]) <- 1:ncol(sample.dat[[i]])
        out <- array(dim=c(nrow(data), dims, R))
        for(i in 1:R){
            tmp <- blackbox(sample.dat[[i]], missing=missing, dims=dims, minscale=minscale, verbose=verbose)
            if(tmp$individuals[[dims]][posStimulus, 1] < 0){
                tmp$individuals[[dims]][,1] <- -tmp$individuals[[dims]][,1]
            }
            out[,,i] <- as.matrix(tmp$individuals[[dims]])
        }
        class(out) <- "bootbb"
        invisible(out)
    }
boot.blackbox_transpose <- function(data, missing, dims=1, minscale, verbose=FALSE, posStimulus = 1, R=100){
    out <- array(dim=c(ncol(data), dims, R))
    for(i in 1:R){
        tmp <- data[sample(1:nrow(data), nrow(data), replace=TRUE),]
        result <- blackbox_transpose(tmp,missing=missing,
                    dims=dims, minscale=minscale, verbose=verbose)
        if(result$stimuli[[dims]][posStimulus,2] > 0)
            result$stimuli[[dims]][,2] <- -1 *
            result$stimuli[[dims]][,2]
    out[,,i] <- as.matrix(result$stimuli[[dims]][,2:((2+dims)-1)])
    }
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
print.aldmck_ci <- function(x, ..., digits=3){
    x$idealpt <- sprintf(paste0("%.", digits, "f"), x$idealpt)
    x$sd <- sprintf(paste0("%.", digits, "f"), x$sd)
    x$lower <- sprintf(paste0("%.", digits, "f"), x$lower)
    x$upper <- sprintf(paste0("%.", digits, "f"), x$upper)
    print.data.frame(x)
}

BAM <- function(data, polarity, zhatSave=TRUE, abSave=FALSE, resp.idealpts=FALSE, n.sample = 2500, ...){
if(!("bamPrep" %in% class(data)))stop("Data should be output from the bamPrep function")
args <- as.list(match.call(expand.dots = FALSE)$`...`)
if(!("n.chains" %in% names(args)))args$n.chains = 2
if(!("n.adapt" %in% names(args)))args$n.adapt = 10000
if(!("inits" %in% names(args))){
    orig <- aldmck(na.omit(data$stims), respondent=0, polarity=polarity, verbose=FALSE)
    args$inits <- function(){list(zhatstar = orig$stimuli + rnorm(length(orig$stimuli), 0, 1))}
}

args$file <- system.file("templates/BAM_JAGScode.bug", package="asmcjr")
lower <- rep(-100, ncol(data$stims))
upper <- rep(100, ncol(data$stims))
upper[polarity] <- 0
args$data <- list('z'=data$stims, q = ncol(data$stims), N=nrow(data$stims), lower=lower, upper=upper)
mod.sim <- do.call("jags.model", args)

if(zhatSave & !abSave){
    samples <- coda.samples(mod.sim,'zhat',  n.sample,  thin=1)
    zhat <- samples
    for(i in 1:length(zhat)){colnames(zhat[[i]]) <- colnames(data$stims)}
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
if(abSave & !zhatSave){
    samples <- coda.samples(mod.sim, c('a', 'b'),  n.sample,  thin=1)
    a <- samples[,grep("^a", colnames(samples[[1]]))]
    b <- samples[,grep("&b", colnames(samples[[1]]))]
    res.list = list(a=a, b=b)
}
if(abSave & zhatSave){
    samples <- coda.samples(mod.sim, c('zhat', 'a', 'b'),  n.sample,  thin=1)
    zhat <- samples[,grep("^z", colnames(samples[[1]]))]
    for(i in 1:length(zhat)){colnames(zhat[[i]]) <- colnames(data$stims)}
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

diffStims <- function(x, stims, digits=3, ...){
    if("mcmc.list" %in% class(x)){
        x <- do.call("rbind", x)
    }
    if(!(is.matrix(x) | is.data.frame(x)))stop("x must be a matrix or data frame of MCMC or resampled values\n")
    x <- as.matrix(x)
    if(!is.numeric(stims)){
        stims <- match(stims, colnames(x))
    }
    combs <- combn(stims, 2)[,,drop=FALSE]
    D <- matrix(0, ncol=ncol(combs), nrow=ncol(x))
    D[cbind(combs[1,], 1:ncol(combs))] <- -1
    D[cbind(combs[2,], 1:ncol(combs))] <- 1
    diffs <- x %*%D
    probs <- colMeans(diffs > 0)
    comps <- paste("Pr(", colnames(x)[combs[2, ]], " > ", colnames(x)[combs[1,]], ")", sep="")
    result <- data.frame('Comparison'=comps, 'Probability'=sprintf(paste0("%.", digits, "f"), probs))
    result
}

aldmckSE <- function(obj, data, ...){
    tmp <- na.omit(cbind(obj$respondents[,1:2], data))
    alpha <- tmp[,1]
    beta <- tmp[,2]
    z <- tmp[,3:ncol(tmp)]
    zhat <- obj$stimuli
    sigmaj <- rep(0,length(zhat))
    #generate sigma j
    for (j in 1:length(zhat)){
        for (i in 1:length(alpha)){
            sigmaj[j] <- sigmaj[j]+((alpha[i] + beta[i]*zhat[j]) - z[i,j])^2
        }}
    for (i in 1:length(zhat)){
        sigmaj[i] <- sqrt((sigmaj[i]/length(alpha)))
    }
    sigmaj
}

ggplot.blackbox <- function(result, dims, whichRes=NULL, groupVar=NULL, issueVector=NULL,
    data=NULL, missing=NULL, rug=FALSE, xlab=NULL, main = NULL, ylab=NULL, nudgeX=NULL, nudgeY=NULL,...){
    wres <- ifelse(is.null(whichRes), max(dims), whichRes)
    dimdat <- result$individuals[[wres]][,dims]
    names(dimdat) <- c("x", "y")
    if(is.null(groupVar)){
        g <- ggplot(dimdat, aes(x=x, y=y)) + geom_point(shape=1, col="gray65") + theme_bw()
    }
    else{
        dimdat$group = groupVar
        dimdat$pch = substr(as.character(dimdat$group), 1, 1)
        ng <- length(unique(na.omit(groupVar)))
        g <- ggplot(dimdat, aes(x=x, y=y)) +
            geom_point(aes(group=group, col=group), alpha=0) +
            geom_text(aes(group=group, col=group, label=pch), show.legend=FALSE) +
            scale_color_manual(values=gray.palette(ng)) +
            guides(colour = guide_legend("Grouping", override.aes = list(size = 2, alpha = 1))) +
            theme_bw()
    }
    if(rug){
        g <- g+geom_rug(show.legend=FALSE)
    }
    if(is.null(xlab)){
        xlab <- paste0("Dimension ", dims[1])
    }
    if(is.null(ylab)){
        ylab <- paste0("Dimension ", dims[2])
    }
    g <- g+ylab(ylab) + xlab(xlab)
    if(!is.null(issueVector)){
        if(is.null(data))stop("If you want to plot issue vectors, you need to specify the data\n")
        if(is.character(issueVector)){
            iss <- match(issueVector, colnames(data))
            if(length(iss) != length(issueVector))stop("At least one of the specified issues didn't match names in the data\n")
        }
        else{
            iss <- issueVector
        }
        dv <- data[,iss, drop=FALSE]
        dv[which(dv %in% missing, arr.ind=TRUE)] <- NA
        dv <- do.call("data.frame", lapply(1:ncol(dv), function(x)as.factor(dv[,x])))
        names(dv) <- colnames(data)[iss]
        op <- list()
        for(i in 1:ncol(dv)){
            op[[i]] <- polr(dv[,i] ~ x + y, data=dimdat, method="probit")
        }
        b <- sapply(op, function(x)x$coef)
        Nvals <- apply(b, 2, function(x)x/sqrt(sum(x^2)))
        scale.fac <- min(apply(dimdat[,1:2], 2, function(x)diff(range(x, na.rm=TRUE)))/2)
        for(i in 1:ncol(Nvals)){
            tmp <- data.frame(x=c(0, scale.fac*Nvals[1,i]), y=c(0, scale.fac*Nvals[2,i]))
            g <- g + geom_line(data=tmp, arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed"), size=1.1) +
                geom_line(data=-tmp, arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), size=1.1)
            }
        if(is.null(nudgeX)){nudgeX <- rep(0, ncol(Nvals))}
        if(is.null(nudgeY)){nudgeY <- rep(0, ncol(Nvals))}
        colnames(Nvals) <- colnames(data)[iss]
        nvals <- t(-Nvals*scale.fac)
        nvals <- as.data.frame(nvals)
        g <- g + geom_text(data=nvals, aes(x=x, y=y, label=rownames(nvals), size=2),
             nudge_x = nudgeX, nudge_y=nudgeY, show.legend=FALSE)
        }
    return(g)
}

doubleCenter <- function(x){
    p <- dim(x)[1]
    n <- dim(x)[2]
    -(x-matrix(apply(x,1,mean),nrow=p,ncol=n) -
          t(matrix(apply(x,2,mean),nrow=n,ncol=p)) + mean(x))/2
}
doubleCenterRect <- function(x){
    n <- nrow(x)
    q <- ncol(x)
    -(x-matrix(apply(x,1,mean), nrow=n, ncol=q) -
          t(matrix(apply(x,2,mean), nrow=q, ncol=n)) + mean(x))/2
}


BMDS <- function(data, posStims, negStims, z, fname=NULL, n.sample = 2500, ...){
    args <- as.list(match.call(expand.dots = FALSE)$`...`)
    if(!("n.chains" %in% names(args)))args$n.chains = 2
    if(!("n.adapt" %in% names(args)))args$n.adapt = 10000
    if(!("inits" %in% names(args))){
        z.init <- array(runif(2*nrow(z), -5, 5), dim=dim(z))
        z.init[which(!is.na(z))] <- NA
        z.init[posStims[1], 1] <- abs(z.init[posStims[1], 1] )
        z.init[posStims[2], 2] <- abs(z.init[posStims[2], 2] )
        z.init[negStims[1], 1] <- -abs(z.init[negStims[1], 1] )
        z.init[negStims[2], 2] <- -abs(z.init[negStims[2], 2] )
        args$inits <- function(){list(z=z.init)}
    }

    m1 <- "model{
        for (i in 1:(N-1)){
            for (j in (i+1):N){
                dstar[i,j] ~ dlnorm(mu[i,j],tau)
                mu[i,j] <- log(sqrt((z[i,1]-z[j,1])*(z[i,1]-z[j,1])+(z[i,2]-z[j,2])*(z[i,2]-z[j,2])))
            }
        }
        tau ~ dunif(0,10)"
    nZ <- nrow(data)
    d1const <- c(posStims[1], negStims[1])
    d2const <- c(posStims[2], negStims[2])
    d1const2 <- d2const2 <- c("T(0, )", "T(,0)")
    d1const2 <- d1const2[order(d1const)]
    d1const <- sort(d1const)
    d2const2 <- d2const2[order(d2const)]
    d2const <- sort(d2const)
    for(i in 1:nZ){
        if(is.na(z[i,1])){
            m1 <- paste(m1, "\nz[", i,", 1] ~ dnorm(0,.01)", ifelse(i %in% d1const, d1const2[which(d1const == i)], ""), sep="")
        }
        if(is.na(z[i,2])){
            m1 <- paste(m1, "\nz[", i,", 2] ~ dnorm(0,.01)", ifelse(i %in% d2const, d2const2[which(d2const == i)], ""), sep="")
        }
    }
    m1 <- paste(m1, "\n}",sep="")
    if(is.null(fname))stop("Must specify a file name to write the code to")
    cat(m1, file=fname)
    data <- as.matrix(data)
    args$file <- fname
    args$data <- list('N'=nrow(data), dstar = as.matrix(max(data)-data), z=z)
    mod.sim <- do.call("jags.model", args)
    samples <- coda.samples(mod.sim,'z',  n.sample,  thin=1)
    zhat <- samples
    zhat.sum <- summary(zhat)
    zhat.ci <- data.frame("stimulus" = c(outer(colnames(data), c(" D1", " D2"), paste0)),
                              "idealpt" = zhat.sum$statistics[,1],
                              "sd" = zhat.sum$statistics[,2],
                              "lower" = zhat.sum$quantiles[,1],
                              "upper" = zhat.sum$quantiles[,5])
    rownames(zhat.ci) <- NULL
    res.list = list(zhat=zhat, zhat.ci = zhat.ci)
    invisible(res.list)
}

mlsmu6 <- function(input, ndim=2, cutoff=5, tol=0.0005, maxit=50, id=NULL){
    rn <- rownames(input)
    cn <- colnames(input)
    input <- as.matrix(input)
    iter <- NULL
    keep <- which(rowSums(!is.na(input))>=cutoff)
    T <- input[keep,]
    id <- id[keep]
    #
    np <- nrow(T)
    nq <- ncol(T)
    xrow <- sapply(1:np,function(i) length(rep(1,nq)[!is.na(T[i,])]))
    xcol <- sapply(1:nq,function(j) length((1:np)[!is.na(T[,j])]))
    #
    T <- (100-T)/50
    TT <- T
    TT[is.na(TT)] <- mean(T,na.rm=TRUE)
    TTSQ <- T*T
    TTSQ[is.na(T)] <- (mean(T,na.rm=TRUE))**2
    TEIGHT <- as.numeric(TT)
    dim(TEIGHT) <- c(np,nq)
    TTSQDC <- doubleCenterRect(TTSQ)
    #
    xsvd <- svd(TTSQDC)
    zz <- xsvd$v[,1:ndim]
    xx <- rep(0,np*ndim)
    dim(xx) <- c(np,ndim)
    for (i in 1:ndim){
    xx[,i] <- xsvd$u[,i]*sqrt(xsvd$d[i])
    }
    #
    sumaj <- function(i){
    jjj <- 1:nq
    j <- jjj[!is.na(T[i,])]
    s <- (xx[i,1]-zz[j,1])^2 + (xx[i,2]-zz[j,2])^2  ### Note this needs to be expanded if estimating more than two dimensions
    s=sqrt(s)
    sx=TEIGHT[i,j]
    sum((s-sx)^2)
    }
    #
    sumvector <- sapply(1:np,sumaj)
    suma <- sum(sumvector)
    xxx <- xx
    zzz <- zz
    kp=0
    ktp=0
    sumalast <- 100000.00
    SAVEsumalast <- sumalast
    #
    loop <- done <- 1
    while(done >= tol & loop <= maxit){
    xxx[,1:ndim] <- 0
    zzz[,1:ndim] <- 0
    kp=kp+1
    ktp=ktp+1
    #
    for(i in 1:np){
        for(j in 1:nq){
        if(!is.na(T[i,j])){
            s=0
            for(k in 1:ndim)
            s <- s+(xx[i,k]-zz[j,k])^2
            xc <- ifelse(s==0, 1.0, TEIGHT[i,j]/sqrt(s))
            for(k in 1:ndim)
            zzz[j,k]=zzz[j,k]+(xx[i,k]-xc*(xx[i,k]-zz[j,k]))/xcol[j]
        }
        }
    }
    for(k in 1:ndim){
        for(i in 1:np){
        sw <- 0.0
        for(j in 1:nq){
            if(!is.na(T[i,j])){
            s=0
            for(kk in 1:ndim)
                s <- s+(xx[i,kk]-zzz[j,kk])^2
            xc <- ifelse(s==0, 1.0, TEIGHT[i,j]/sqrt(s))
            xxx[i,k]=xxx[i,k]+(zzz[j,k]-xc*(zzz[j,k]-xx[i,k]))
            }
        }
        xxx[i,k]=xxx[i,k]/xrow[i]
        }
    }
    xx <- xxx
    zz <- zzz
    sumvector <- sapply(1:np,sumaj)
    suma <- sum(sumvector)
    #
    iter <- c(iter, suma)
    done=((sumalast-suma)/suma)
    sumalast=suma
    loop <- loop + 1
    }
    #
    #
    #
    # FLIP SPACE IF POLARITY WRONG
    if(xx[1,1] < 0){
    xx[,1] <- -1 * xx[,1]
    zz[,1] <- -1 * zz[,1]
    }
    rownames(xx) <- rn
    rownames(zz) <- cn
    xx <- as.data.frame(xx)
    zz <- as.data.frame(zz)
    names(xx) <- names(zz) <- paste0("Dim", 1:ndim)
    if(!is.null(id)){xx$id <- id}
    ret <- list(inds=xx, stims=zz, iter=cbind(1:length(iter), iter))
    colnames(ret$iter) <- c("iter", "ErrorSS")
    class(ret) <- "mlsmu6"
    return(ret)
}
#

plot.mlsmu6 <- function(x, ..., selected.stims=NULL, ind.id.size=3, stim.id.size=6){
    dfi <- x$inds
    if(is.null(dfi$id)){
        dfi$id <- "x"
    }
    dfi$id <- as.character(dfi$id)
    dfii <- sort(unique(dfi$id))
    dfs <- x$stims
    dfs$id <- rownames(dfs)
    if(!is.null(selected.stims)){
        dfs <- dfs[which(rownames(dfs) %in% selected.stims), ]
    }
    df <- rbind(dfi, dfs)
    levs <- unique(df$id)
    levs1 <- dfii
    levs2 <- dfs$id
    df$id <- factor(df$id, c(sort(levs1), sort(levs2)))
    ggplot(df, aes(x=Dim1, y=Dim2, label=id, colour=id, size=id)) +
           geom_text(show.legend=FALSE) +
            scale_colour_manual(values=c("gray25", "gray50", rep("black", nrow(dfs))), guide="none") +
            scale_size_manual(values=c(rep(ind.id.size, length(dfii)), rep(stim.id.size, nrow(dfs)))) +
            scale_shape_discrete(name="ID", breaks=dfii, labels=dfii) +
            theme_bw() + theme(aspect.ratio=1)


}



bayesunfold <-
function(input, dims = 2, nsamp = 2000, burnin = 1000, slice.starts = c("lbfgs", "random"), print.lbfgs="console", print.slice="console", ...) 
{
#
set_globals <- function(nslice,nburn,nrowX,ncolX,NS,N,NDIM,UNFOLD,NMISSING,X,CONSTRAINTS) {
   res <-.C("copyFromR",
     as.integer(nslice),
     as.integer(nburn),
     as.integer(nrowX),
     as.integer(ncolX),
     as.integer(NS),
     as.integer(N),
     as.integer(NDIM),
     as.integer(UNFOLD),
     as.integer(NMISSING),
     as.double(X),
     as.double(CONSTRAINTS))
}
do_lbfgs <- function(kpnp,kpnq,yrotate,rmatrix){
     .C("mainlbfgs",
     as.integer(kpnp),
     as.integer(kpnq),
     as.double(yrotate),
     as.double(rmatrix))
}
do_logposterior <- function(theta,XCOORDS,sumsquared,SIGMAPRIOR){
     .C("keithrules",
     as.double(theta),
     as.double(XCOORDS),
     as.double(sumsquared),
     as.double(SIGMAPRIOR))
}
do_sliceu <- function(theta,thetanow2,theta1000,ssenow,XTRUE,thetaLeft,thetaRight,WW,PP,XCOORDS,SIGMAPRIOR){
     .C("sliceunfolding",
     as.double(theta),
     as.double(thetanow2),
     as.double(theta1000),
     as.double(ssenow),
     as.double(XTRUE),
     as.double(thetaLeft),
     as.double(thetaRight),
     as.double(WW),
     as.integer(PP),
     as.double(XCOORDS),
     as.double(SIGMAPRIOR))
}
#
#
#
#
#
#  DELETE ROWS WITH LESS THAN 5 THERMOMETER RESPONSES
#
T <- input
#
#  I. FEELING THERMOMETERS
#
T <- ANES1968[,1:12]
T <- as.matrix(T)
T[T < 0 | T > 100] <- NA
#
nstimuli <- ncol(T)
#
vote.turnout <- ANES1968[,13]
presidential.vote <- ANES1968[,14]
#
#  Delete rows (respondents) with less than (nstimuli - 7) thermometer ratings
#
cutoff <- nstimuli - 7
keep <- rowSums(!is.na(T))>=cutoff
presidential.vote <- presidential.vote[keep]
T <- T[keep,]
T <- (100-T)/50
#T <- (100-T)/50 + 0.02
#
nrowX <- nrow(T)
ncolX <- ncol(T)
nburn <- burnin
nslice <- nsamp
NS <- 2
N <- NS*(nrowX+ncolX) - ((NS*(NS+1))/2)
NDIM <- NS*(nrowX+ncolX) - (NS-1)
UNFOLD <- 1
#
#  ROWS MUST HAVE LESS THAN 7 MISSING ENTRIES
#  COLUMNS MUST HAVE AT LEAST 1/4 NON-MISSING ENTRIES (THIS IS HARD WIRED IN THE C CODE)
#
NMISSING <- 7
#
TT <- T
TT[is.na(TT)] <- -999.0
X <- as.vector(t(TT))
#
#  CONSTRAINTS
#
CONSTRAINTS <- rep(1,NS*(nrowX+ncolX))
#
if (NS==1){
  CONSTRAINTS[NS*(nrowX+ncolX)] <- 0
}
if (NS==2){
  CONSTRAINTS[(NS*(nrowX+ncolX)-NS):(NS*(nrowX+ncolX))] <- 0
}
if (NS==3){
  CONSTRAINTS[(NS*(nrowX+ncolX)-4):(NS*(nrowX+ncolX))] <- 0
  CONSTRAINTS[(NS*(nrowX+ncolX)-6)] <- 0
}
#
#  SMACOF (METRIC UNFOLDING) FOR COMPARISON PURPOSES
#
weights <- matrix(1, nrow=nrowX, ncol=ncolX)
weights[is.na(T)] <- 0
#
SMACOF.result <- smacofRect(T, ndim=NS, circle = "none", weightmat=weights, itmax=10000, ...)
#
# Flip SMACOF results
#
#SMACOF.result$conf.col[,1] <- -1 * SMACOF.result$conf.col[,1]
#SMACOF.result$conf.col[,2] <- -1 * SMACOF.result$conf.col[,2]
#SMACOF.result$conf.row[,1] <- -1 * SMACOF.result$conf.row[,1]
#SMACOF.result$conf.row[,2] <- -1 * SMACOF.result$conf.row[,2]
#
zmetric <- as.numeric(t(SMACOF.result$conf.col))
xmetric <- as.numeric(t(SMACOF.result$conf.row))
rmatrix <- c(zmetric,xmetric)
rmatrix[(NS*(nrowX+ncolX)-NS):(NS*(nrowX+ncolX))] <- 0
yrotate <- rep(0,(NS*(nrowX+ncolX)))

#
#  L-BFGS
#
set_globals(nslice,nburn,nrowX,ncolX,NS,N,NDIM,UNFOLD,NMISSING,X,CONSTRAINTS)
#
if(print.lbfgs != "console"){
    sink(print.lbfgs)
    lbfgs.result <- do_lbfgs(nrowX,ncolX,yrotate,rmatrix)
    sink()
}
else{
    lbfgs.result <- do_lbfgs(nrowX,ncolX,yrotate,rmatrix)
}
lbfgs.coords <- lbfgs.result[[3]]
dim(lbfgs.coords) <- c(NS,(nrowX+ncolX))
X3 <- t(lbfgs.coords)
X3.new <- X3
#
#  Rotation
#
 A <- matrix(c(cos(-30*pi/180), -sin(-30*pi/180), sin(-30*pi/180), cos(-30*pi/180)), nrow=2, ncol=2, byrow=TRUE)
#
 X3.new <- X3 %*% A
lbfgs.coords.new <- as.vector(t(X3.new))

#
#lbfgs.result[[3]] <- lbfgs.coords.new
#
#
lbfgs.stimuli <- X3.new[1:ncolX,]
lbfgs.individuals <- X3.new[(ncolX+1):(nrowX+ncolX),]
#
#  BAYESIAN UNFOLDING
#
theta <- c(runif(NDIM,min=-.5,max=.5)) # Random starts for Slice Sampler
theta <- lbfgs.result[[3]] # Non-random starts for Slice Sampler
theta2 <- theta
theta1000 <- rep(0,nslice*NDIM)
dim(theta1000) <-c(nslice*NDIM,1)
ssenow <-rep(0,(2*(nslice+nburn)))
dim(ssenow) <-c((2*(nslice+nburn)),1)
XTRUE <- lbfgs.result[[3]]
XTRUE <- lbfgs.coords.new
thetaL <- rep(-99.0, NDIM)
thetaR <- rep(99.0, NDIM)
dim(thetaL) <- dim(thetaR) <- c(NDIM, 1)
thetaL[NDIM] <- 0.10
thetaR[NDIM] <- 0.50
WW <- 1.0
PP <- 3.0
XCOORDS <- rep(0,(nrowX+ncolX)*NS)
SIGMAPRIOR <- 100.0
#
if(print.slice != "console"){
    sink(print.slice)
    result4 <- do_sliceu(theta,theta2,theta1000,ssenow,XTRUE,thetaL,thetaR,WW,PP,XCOORDS,SIGMAPRIOR)
    sink()
}
else{
    result4 <- do_sliceu(theta,theta2,theta1000,ssenow,XTRUE,thetaL,thetaR,WW,PP,XCOORDS,SIGMAPRIOR)
}
#
#  length(result4[[3]]) = NDIM*nslice
#
#  CALCULATE THE MEANS FROM THE SLICE SAMPLER --  mean(result4[[4]][2501:4000]) is the mean of the variance term
#
sigma_squared_hat <- mean(result4[[4]][2501:4000])
sigma_squared_hat_sd <- sd(result4[[4]][2501:4000])
#
#  SAMPLES
#
samples <- matrix(result4[[3]], ncol=NDIM, byrow=TRUE)
samples <- cbind(samples, 0)
z.mean <- colMeans(samples[,1:(ncolX*NS)])
z.mat <- matrix(z.mean, byrow=TRUE, ncol=NS)
p <- procrustes(z.mat, lbfgs.stimuli, translation=TRUE)

### Results without procrustes rotation
stims <- vector("list", nsamp)
for(j in 1:nsamp){
    stims[[j]] <- matrix(samples[j,1:(ncolX*NS)], ncol=NS, byrow=TRUE)
}
stim.array <- array(as.numeric(unlist(stims)), dim=c(ncolX, NS, nsamp))
stim.mean <- aaply(stim.array, c(1,2), mean, na.rm=T)
stim.lower <- aaply(stim.array, c(1,2), quantile, .025, na.rm=T)
stim.upper <- aaply(stim.array, c(1,2), quantile, .975, na.rm=T)

## arrays the data as first column in columns 1:ncolX and 
## second column of stimuli in (ncolX+1):(ncolX*NS)
stim.samples <- matrix(c(stim.array), ncol=ncolX*NS, byrow=TRUE)

rownames(stim.mean) <- rownames(stim.lower) <- rownames(stim.upper) <- colnames(input)
colnames(stim.samples) <- c(sapply(1:2, function(m)paste(colnames(input), m, sep=".")))


individuals <- vector("list", nsamp)
for(j in 1:nsamp){
    individuals[[j]] <- matrix(samples[j,-(1:(ncolX*NS))], ncol=NS, byrow=TRUE)
}
indiv.array <- array(as.numeric(unlist(individuals)), dim=c(nrowX, NS, nsamp))
indiv.mean <- aaply(indiv.array, c(1,2), mean, na.rm=T)
indiv.lower <- aaply(indiv.array, c(1,2), quantile, .025, na.rm=T)
indiv.upper <- aaply(indiv.array, c(1,2), quantile, .975, na.rm=T)

## arrays the data as first column in columns 1:nrowX and 
## second column of stimuli in (nrowX+1):(nrowX*NS)

indiv.samples <- matrix(c(indiv.array), ncol=nrowX*NS, byrow=TRUE)

stim.samples <- list(stim.samples)
class(stim.samples) <- "mcmc.list"

indiv.samples <- list(indiv.samples)
class(indiv.samples) <- "mcmc.list"

orig.res = list(stim.samples = stim.samples,
	indiv.samples = indiv.samples,
	stimuli = list(mean = stim.mean, lower=stim.lower, upper=stim.upper),
	individuals = list(mean = indiv.mean, lower=indiv.lower, upper=indiv.upper))

### Results With Procrustes Roatation 
stims <- vector("list", nsamp)
for(j in 1:nsamp){
    tmp <- matrix(samples[j,1:(ncolX*NS)], ncol=NS, byrow=TRUE)
    stims[[j]] <-  tmp %*% p$R
    # for(i in 1:NS){
    # stimuli[[i]] <- rbind(stimuli[[i]], tmpR[,i])
    # }  
}
stim.array <- array(as.numeric(unlist(stims)), dim=c(ncolX, NS, nsamp))
stim.mean <- aaply(stim.array, c(1,2), mean, na.rm=T)
stim.lower <- aaply(stim.array, c(1,2), quantile, .025, na.rm=T)
stim.upper <- aaply(stim.array, c(1,2), quantile, .975, na.rm=T)

## arrays the data as first column in columns 1:ncolX and 
## second column of stimuli in (ncolX+1):(ncolX*NS)
stim.samples <- matrix(c(stim.array), ncol=ncolX*NS, byrow=TRUE)

rownames(stim.mean) <- rownames(stim.lower) <- rownames(stim.upper) <- colnames(input)
colnames(stim.samples) <- c(sapply(1:2, function(m)paste(colnames(input), m, sep=".")))


individuals <- vector("list", nsamp)
for(j in 1:nsamp){
    tmp <- matrix(samples[j,-(1:(ncolX*NS))], ncol=NS, byrow=TRUE)
    individuals[[j]] <- tmp %*% p$R
    # for(i in 1:NS){
    # individuals[[i]] <- rbind(individuals[[i]], tmpR[,i])
    # }  
}
indiv.array <- array(as.numeric(unlist(individuals)), dim=c(nrowX, NS, nsamp))
indiv.mean <- aaply(indiv.array, c(1,2), mean, na.rm=T)
indiv.lower <- aaply(indiv.array, c(1,2), quantile, .025, na.rm=T)
indiv.upper <- aaply(indiv.array, c(1,2), quantile, .975, na.rm=T)

## arrays the data as first column in columns 1:nrowX and 
## second column of stimuli in (nrowX+1):(nrowX*NS)

indiv.samples <- matrix(c(indiv.array), ncol=nrowX*NS, byrow=TRUE)

stim.samples <- list(stim.samples)
class(stim.samples) <- "mcmc.list"

indiv.samples <- list(indiv.samples)
class(indiv.samples) <- "mcmc.list"

rotated.res = list(stim.samples = stim.samples,
	indiv.samples = indiv.samples,
	stimuli = list(mean = stim.mean, lower=stim.lower, upper=stim.upper),
	individuals = list(mean = indiv.mean, lower=indiv.lower, upper=indiv.upper))



BUobject <- list(retained.obs = keep, smacof.result = SMACOF.result, lbfgs.result = lbfgs.stimuli,
	sigma_squared_hat = sigma_squared_hat,
	sigma_squared_hat_sd = sigma_squared_hat_sd,	
    unrotated = orig.res, 
    rotated = rotated.res)

class(BUobject) <- "bayesunfold"
BUobject

}

plot.bayesunfold <- function(x, ..., which.res =c("rotated", "unrotated"), labels=c("color", "text"),  plot.stimuli = TRUE, plot.individuals=FALSE, selected.stim=NULL, selected.individuals = NULL, individual.id=NULL){
    w <- match.arg(which.res)
    lab <- match.arg(labels)
    if(w == "rotated"){
        res <- x$rotated
    }
    else{
        res <- x$unrotated
    }
    stims <- res$stimuli$mean
    indivs <- res$individuals$mean
    stim.data <- as_tibble(res$stimuli$mean, rownames="names") 
    names(stim.data)[2:3] <- c("D1", "D2")
    indiv.data <- as_tibble(res$individuals$mean, rownames=NULL)
    names(indiv.data)[1:2] <- c("D1", "D2")
    stim.data <- add_column(stim.data, D1.lower=res$stimuli$lower[,1], D2.lower=res$stimuli$lower[,2], 
        D1.upper = res$stimuli$upper[,1], D2.upper = res$stimuli$upper[,2])
    indiv.data <- add_column(indiv.data, D1.lower=res$individuals$lower[,1], D2.lower=res$individuals$lower[,2], 
        D1.upper = res$individuals$upper[,1], D2.upper = res$individuals$upper[,2])
    if(!is.null(individual.id)){
        indiv.data <- add_column(indiv.data, names=individual.id)
    }
    if(plot.stimuli & ! plot.individuals){
        if(lab == "text"){
            g <- ggplot(stim.data, aes(x=D1, y=D2)) + geom_text(aes(label=names, colour=names)) + guides(colour=FALSE)
        }
        if(lab == "color"){
            g <- ggplot(stim.data, aes(x=D1, y=D2)) + geom_point(aes(colour=names))
        }
        return(g)
    }
    if(plot.stimuli & plot.individuals){
        if(is.null(individual.id)){
            if(lab == "text"){
                g <- ggplot(stim.data, aes(x=D1, y=D2)) + geom_point(data=indiv.data, col="gray75", pch=1, cex=.5 ) + geom_text(aes(label=names, colour=names)) 
            }
            if(lab == "color"){
                g <- ggplot(stim.data, aes(x=D1, y=D2)) + geom_point(data=indiv.data, col="gray75", pch=1, cex=.5 ) + geom_point(aes(colour=names))
            }
        }
        else{
            if(lab == "text"){
                g <- ggplot(stim.data, aes(x=D1, y=D2)) + geom_point(data=indiv.data, aes(colour=names), pch=1, cex=.5 ) + geom_text(aes(label=names, colour=names)) 
            }
            if(lab == "color"){
                g <- ggplot(stim.data, aes(x=D1, y=D2)) + geom_point(data=indiv.data, aes(colour=names), pch=1, cex=.5 ) + geom_point(aes(colour=names))
            }
        }
        return(g)
    }
    if(!plot.stimuli & plot.individuals){
        if(is.null(individual.id)){
            g <- ggplot(indiv.data, aes(x=D1, y=D2)) + geom_point( pch=1, cex=.5 )
        }
        else{
            g <- ggplot(indiv.data, aes(x=D1, y=D2, colour=names)) + geom_point( pch=1, cex=.5 )
        }
        return(g)
    }
    if(!plot.stim & !plot.individuals){
        return(list(stim =stim.data, indiv=indiv.data))
    }
}