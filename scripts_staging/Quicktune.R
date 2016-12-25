
# main function of this pkg
# input:
# @ xgb    | initial xgb model, coming from caret::train()
# @ metric | evaluation metric to be used for entire Quicktune process
# @


Quicktune <- function(xgb, metric){
    
    initial_perf <- xgb$resample[metric]
    tunegrid     <- xgb$results
    
    # construct OLS model
    tunemod <- TuneOLS(tunegrid)
    
    # PopulateNext
    
    
}