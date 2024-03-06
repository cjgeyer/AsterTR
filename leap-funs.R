
 ### not very general only fits the particular models used in
 ### U of M Sch of Stats Tech Rept 000
 ###
 ### pred     argument "pred" to aster function calls
 ### fam      argument "fam" to aster function calls
 ### data     argument "data" to aster function calls
 ### nsplit   most general model fit is quadratic in predictor variables
 ###              "z1", "z2", ... with the last digit being nsplit
 ### type     whether to use AIC or BIC
 ### cutoff   fit all models with AIC less than min(AIC) + cutoff,
 ###              and similarly with AIC replaced by BIC
 ### envir    environment from previous call of this function

 aster.leaps <- function(pred, fam, data, nsplit, response = "resp",
     type = c("AIC", "AICc", "BIC"), cutoff = 0,
     envir = new.env(hash = TRUE, parent = globalenv())) {

     type <- match.arg(type)

     stopifnot(is.data.frame(data))
     stopifnot(! is.null(data$root))
     stopifnot(! is.null(data$varb))
     stopifnot(! is.null(data$id))
     nind <- length(unique(data$id))
     stopifnot(! is.null(data$vtype))
     stopifnot(! is.null(data$uyear))
     stopifnot(is.numeric(nsplit))
     stopifnot(length(nsplit) == 1)
     stopifnot(nsplit == as.integer(nsplit))
     stopifnot(nsplit > 0)
     stopifnot(is.character(response))
     stopifnot(length(response) == 1)
     stopifnot(! is.null(data[[response]]))
     stopifnot(is.numeric(cutoff))
     stopifnot(length(cutoff) == 1)
     stopifnot(cutoff >= 0)
     stopifnot(is.environment(envir))
     if (! exists("minaic", envir = envir, inherits = FALSE)) {
         assign("minaic", Inf, envir = envir)
     }
     if (! exists("minbic", envir = envir, inherits = FALSE)) {
         assign("minbic", Inf, envir = envir)
     }
     if (! exists("mincic", envir = envir, inherits = FALSE)) {
         assign("mincic", Inf, envir = envir)
     }
     # if (exists("nsplit", envir = envir, inherits = FALSE)) {
     #     stopifnot(nsplit == get("nsplit", envir = envir))
     # } else {
     #     assign("nsplit", nsplit, envir = envir)
     # }

     lsfred <- ls(envir = envir, pattern = "^q[1-9A-Z]*l[1-9A-Z]*$")
     modstart <- length(lsfred)

 makenam <- function(splits) {
     digits <- c(1:9, LETTERS)
     stopifnot(! any(is.na(splits)))
     twos <- seq(along = splits)[splits == 2]
     twos <- paste(digits[twos], collapse = "")
     ones <- seq(along = splits)[splits == 1]
     ones <- paste(digits[ones], collapse = "")
     paste("q", twos, "l", ones, sep = "") 
 }

 nparm <- function(splits) {
     stopifnot(all(! is.na(splits)))
     stopifnot(all(splits %in% c(0, 1, 2)))
     n2 <- sum(splits == 2)
     n1 <- sum(splits == 1)
     return(n2 * (n2 + 1) / 2 + (n1 + n2) + 4)
 }

 domod <- function(splits) {
     stopifnot(all(! is.na(splits)))
     stopifnot(all(splits %in% c(0, 1, 2)))
     nam <- makenam(splits)
     if (exists(nam, envir = envir, inherits = FALSE)) {
         result <- get(nam, envir = envir)
     } else {
         lint <- seq(along = splits)[splits == 1]
         quat <- seq(along = splits)[splits == 2]
         form <- paste(response, "~ vtype + uyear")
         if (length(lint > 0)) {
             lint <- paste("z", lint, sep = "")
             lint <- paste(lint, collapse = " + ")
             form <- paste(form, "+", lint)
         }
         if (length(quat > 0)) {
             quat <- paste("z", quat, sep = "")
             quat <- paste(quat, collapse = ", ")
             form <- paste(form, " + poly(", quat,
                 ", degree = 2, raw = TRUE)", sep = "")
         }
         form <- as.formula(form)
         out <- aster(form, pred, fam, varb, id, root, data = data)
         stopifnot(out$converged)
         p <- length(out$coefficients)
         if (p != nparm(splits))
             stop("Oopsie from nparm")
         dev <- out$deviance
         aic <- dev + 2 * p
         bic <- dev + log(nind) * p
         cic <- aic + 2 * p * (p + 1) / (nind - p - 1)
         result <- list(dev = dev, p = p, aic = aic, bic = bic, cic = cic)
         assign(nam, result, envir = envir)
         minaic <- get("minaic", envir = envir)
         minbic <- get("minbic", envir = envir)
         mincic <- get("mincic", envir = envir)
         if (minaic > aic) assign("minaic", aic, envir = envir)
         if (minbic > bic) assign("minbic", bic, envir = envir)
         if (mincic > cic) assign("mincic", cic, envir = envir)
     }
     return(result)
 }

 leaper <- function(splits) {
     lcsup <- splits
     lcsup[is.na(splits)] <- 2
     gcsub <- splits
     gcsub[is.na(splits)] <- 0
     stopifnot(all(lcsup %in% c(0, 1, 2)))
     dev <- domod(lcsup)$dev
     p <- nparm(gcsub)
     aicbnd <- dev + 2 * p
     bicbnd <- dev + log(nind) * p
     cicbnd <- aicbnd + 2 * p * (p + 1) / (nind - p - 1)
     minaic <- get("minaic", envir = envir)
     minbic <- get("minbic", envir = envir)
     mincic <- get("mincic", envir = envir)
     bnd <- switch(type, AIC = aicbnd, BIC = bicbnd, AICc = cicbnd)
     minbnd <- switch(type, AIC = minaic, BIC = minbic, AICc = mincic)
     if (bnd > minbnd + cutoff || identical(lcsup, gcsub)) {
         result <- list(dev = dev, p = p, bnd = bnd, ref = splits)
     } else {
         isplit <- seq(along = splits)[is.na(splits)][1]
         split0 <- split1 <- split2 <- splits
         split0[isplit] <- 0
         split1[isplit] <- 1
         split2[isplit] <- 2
         l0 <- leaper(split0)
         l1 <- leaper(split1)
         l2 <- leaper(split2)
         if (l0$bnd <= min(l1$bnd, l2$bnd)) {
             result <- l0
         } else {
             if (l1$bnd <= l2$bnd) {
                 result <- l1
             } else {
                 result <- l2
             }
         }
     }
     return(result)
 }

     ### cheat and use known true model to speed things up
     split6 <- c(2, 2, rep(0, nsplit - 2))
     dout6 <- domod(split6)

     tout <- system.time(
         lout <- leaper(rep(NA, nsplit))
     )

     lsfred <- ls(envir = envir, pattern = "^q[1-9A-Z]*l[1-9A-Z]*$")
     mfred <- matrix(NA, nrow = length(lsfred), ncol = 5)
     for (i in seq(along = lsfred))
         mfred[i, ] <- unlist(get(lsfred[i], envir = envir))
     dimnames(mfred) <- list(lsfred, names(get(lsfred[1], envir = envir)))
     mfred <- switch(type, AIC = mfred[order(mfred[ , "aic"]), ],
         BIC = mfred[order(mfred[ , "bic"]), ],
         AICc = mfred[order(mfred[ , "cic"]), ])

     return(list(fits = mfred, time = tout, envir = envir,
         nfit = length(lsfred) - modstart))
 }

 ### not very general only fits the particular models used in
 ### U of M Sch of Stats Tech Rept 000
 ###
 ### string   a string coding a model
 ###              variables are "numbered" 1, 2, ..., 9, A, B, ... so
 ###                  each corresponds to a single character
 ###              the string is "q" followed by the character labels
 ###                  for the variables that are quadratic then "l"
 ###                  followed by the character labels for the variables
 ###                  that are linear
 ###              examples
 ###                  "q12l" -- 1 and 2 are quadratic, none are linear
 ###                  "q123456789Al" --- 1, ..., 10 are quadratic, none
 ###                      are linear
 ###                  "q13l4" -- 1 and 3 are quadratic, 4 is linear
 ### data     a data frame passed to the aster function as its data argument,
 ###              always "redata" in the Tech Rept
 ### response a character string naming the response variable,
 ###              always "resp1" or "resp2" in the Tech Rept

 redomod <- function(string, data, response = "resp") {
     stopifnot(is.character(string))
     stopifnot(length(string) == 1)
     foo <- strsplit(string, split = "")[[1]]
     idxq <- match("q", foo)
     idxl <- match("l", foo)
     stopifnot(length(idxq) == 1)
     stopifnot(length(idxl) == 1)
     lint <- foo[idxl:length(foo)]
     quat <- foo[1:(idxl - 1)]
     lint <- lint[-1]
     quat <- quat[-1]
     digits <- c(1:9, LETTERS)
     lint <- match(lint, digits)
     quat <- match(quat, digits)
     stopifnot(all(! is.na(lint)))
     stopifnot(all(! is.na(quat)))
     stopifnot(length(intersect(lint, quat)) == 0)

     form <- paste(response, "~ vtype + uyear")
     if (length(lint > 0)) {
         lint <- paste("z", lint, sep = "")
         lint <- paste(lint, collapse = " + ")
         form <- paste(form, "+", lint)
     }
     if (length(quat > 0)) {
         quat <- paste("z", quat, sep = "")
         quat <- paste(quat, collapse = ", ")
         form <- paste(form, " + poly(", quat,
             ", degree = 2, raw = TRUE)", sep = "")
     }
     form <- as.formula(form)

     return(aster(form, pred, fam, varb, id, root, data = data))
 }

