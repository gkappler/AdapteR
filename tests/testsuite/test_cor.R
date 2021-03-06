## demo("connecting")

############################################################
## First you pull all variable / data setup out of the example:
Renv <- new.env(parent = globalenv())
Renv$a <- 1:10
Renv$b <- 1:5

FLenv <- as.FL(Renv)


## if you want to use initF, you do it this way:
tmp <- initF.FLMatrix(10,TRUE)
Renv$bigobject <- tmp$R
FLenv$bigobject <- tmp$FL


## If you want to set up with database reference
## MAKE SURE you subset
eqnRtn <- FLMatrix(database          = "FL_DEMO",
                   table_name        = "finEquityReturns",
                   row_id_colname    = "TxnDate",
                   col_id_colname    = "TickerSymbol",
                   cell_val_colname  = "EquityReturn")

eqnRtn <- eqnRtn[,c('AAPL','HPQ','IBM','MSFT','ORCL')]
FLenv$eqnRtn <- eqnRtn
Renv$eqnRtn <- na.omit(as.matrix(FLenv$eqnRtn))

## You can add your own tests and search for tests for function cor in
## the test suite so far.
## move them here!
test_that("Correlation of equity returns, low precision, 1e-3",
          eval_expect_equal({
              corER <- cor(eqnRtn)
              ##print(corER)
              dim(eqnRtn)
          }, Renv, FLenv,
          tolerance=1e-3))

## You can add your own tests and search for tests for function cor in
## the test suite so far.
## move them here!
test_that("Correlation of equity returns, high precision",
          eval_expect_equal({
              corER <- cor(eqnRtn)
              ##print(corER)
              dim(eqnRtn)
          }, Renv, FLenv,
          expectation="corER"
          ))

############################################################
## R documentation example from stats::cor
## run expectations within test_that as blocks
test_that("Variance single column.",
          eval_expect_equal({
              vara <- var(a)  # 9.166667
              length(a)
          }, Renv, FLenv))

test_that("Correlation of two vectors. https://app.asana.com/0/136555696724838/143778401455751",{
    eval_expect_equal({
        varb <- var(b, b) # 2.5
    }, Renv, FLenv)
})

## TODO: change more from here
test_that("Correlation examples -- data needs pulling out!",{
    eval_expect_equal({
        ## Two simple vectors
        cor(1:10, 2:11) # == 1
    }, Renv, FLenv)
})


test_that("Correlation on longley dataset",{
    eval_expect_equal({
        ## Correlation Matrix of Multivariate sample:
        (Cl <- cor(longley))
        ## Graphical Correlation Matrix:
        symnum(Cl) # highly correlated
        ## Spearman's rho  and  Kendall's tau
        symnum(clS <- cor(longley, method = "spearman"))
        symnum(clK <- cor(longley, method = "kendall"))
        ## How much do they differ?
        i <- lower.tri(Cl)
        cor(cbind(P = Cl[i], S = clS[i], K = clK[i]))
        ## cov2cor() scales a covariance matrix by its diagonal
        ##           to become the correlation matrix.
        cov2cor # see the function definition {and learn ..}
        stopifnot(all.equal(Cl, cov2cor(cov(longley))),
                  all.equal(cor(longley, method = "kendall"),
                            cov2cor(cov(longley, method = "kendall"))))
    }, Renv, FLenv)
    ## TODO: add a better and unique description!
})

test_that("Missing Data, swiss dataset",{
    eval_expect_equal({
        ##--- Missing value treatment:
        C1 <- cov(swiss)
        range(eigen(C1, only.values = TRUE)$values) # 6.19        1921
        ## swM := "swiss" with  3 "missing"s :
        swM <- swiss
        colnames(swM) <- abbreviate(colnames(swiss), min=6)
        swM[1,2] <- swM[7,3] <- swM[25,5] <- NA # create 3 "missing"
        ## Consider all 5 "use" cases :
        (C. <- cov(swM)) # use="everything"  quite a few NA's in cov.matrix
        try(cov(swM, use = "all")) # Error: missing obs...
        C2 <- cov(swM, use = "complete")
        stopifnot(identical(C2, cov(swM, use = "na.or.complete")))
        range(eigen(C2, only.values = TRUE)$values) # 6.46   1930
        C3 <- cov(swM, use = "pairwise")
        range(eigen(C3, only.values = TRUE)$values) # 6.19   1938
    }, Renv, FLenv)
})

test_that("Swiss dataset, ...",{
    eval_expect_equal({
        ## swM := "swiss" with  3 "missing"s :
        swM <- swiss
        ## Kendall's tau doesn't change much:
        symnum(Rc <- cor(swM, method = "kendall", use = "complete"))
        symnum(Rp <- cor(swM, method = "kendall", use = "pairwise"))
        symnum(R. <- cor(swiss, method = "kendall"))
        ## "pairwise" is closer componentwise,
        summary(abs(c(1 - Rp/R.)))
        summary(abs(c(1 - Rc/R.)))
        ## but "complete" is closer in Eigen space:
        EV <- function(m) eigen(m, only.values=TRUE)$values
        summary(abs(1 - EV(Rp)/EV(R.)) / abs(1 - EV(Rc)/EV(R.)))
        0
    }, Renv, FLenv,
    "correlations in R only -- need pulling data out to above setup")
})
