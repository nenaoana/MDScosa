# Equivalent of mdsmat8:

pd_mdsmat <- 
  function(
    data,
    party_var,
    issue_var,
    proximity_var,
    year_var,
    years=NULL,
    noweight = FALSE,
    row = FALSE,
    cell = FALSE,
    min = 1,
    imin = NULL
  )
  {   
    init_data = data
    
    # Check if weighting is specified correctly:
    if (noweight==TRUE)
    {
      if (row == TRUE | cell == TRUE) 
        stop('Only one of the options row, noweight, or cell may be specified')
    }
    if (row == TRUE & cell == TRUE)
      stop('Only one of the options row, noweight, or cell may be specified')
    if (row == FALSE & cell == FALSE & noweight == FALSE)
      stop('You must specify one of the options row, noweight, or cell')
    
    
    # Measure of similarity (coded -1/+1) --> measure of dissimilarity (coded 0/2)
    
    data$diss <- (data[,proximity_var]*-1)+1
    
    # Create lists of numbers and values for variables 'party' 'issue' and 'year'
    
    n_parties <- as.numeric(length(table(data[,party_var])))
    v_parties <- names(table(data[,party_var]))
    n_issues <- as.numeric(length(table(data[,issue_var])))
    v_issues <- names(table(data[,issue_var]))
    n_objects <- n_parties+n_issues
    
    n_years <- as.numeric(length(table(data[,year_var])))
    v_years <- names(table(data[,year_var]))
    
    if (!is.null(years)) {v_years <- years}
    
    # create variable "party_year"
    
    if (n_years == 1) {data$party_year <- data[,party_var]}
    else {
      i=1
      j=0
      for (x in v_years){
        for (y in v_parties){
          POBS <- nrow(data[data[,year_var] == x & data[,party_var] == y,])
          if (POBS >= min) {
            data$party_year[data[,year_var] == x & data[,party_var] == y] <- i + j
          }
          i = i +1
        }
        i = 1
        j = j + n_parties
      }
    }
    
    n_party_year <- as.numeric(length(table(data$party_year)))
    v_party_year <- names(table(data$party_year))
    
    
    # Delete parties and issues without observations:
    
    # Exclude parties with less than min observations:
    
    tab <- table(data$party_year)
    data <- data[data$party_year %in% names(tab)[tab>=min],]
    tab2 <- table(data$party_year)
    np_del <- setdiff(names(tab),names(tab2))
    
    del_p <- length(tab)-length(tab2)
    cat(c(del_p, " parties were deleted as they had less than ", min, " observations.","\n"))
    cat(c("The parties that were deleted are", np_del, "\n"))
    
    # Exclude issues with less than imin % observations:
    
    if (!is.null(imin)){
      imin <- imin/100
      ptab <- prop.table(table(data[,issue_var]))
      tabi <- table(data[,issue_var])
      data <- data[data[,issue_var] %in% names(ptab)[ptab>imin],]
      tabi2 <- table(data[,issue_var])
      
    }
    else {
      ptab <- table(data[,issue_var])
      tabi <- table(data[,issue_var])
      data <- data[data[,issue_var] %in% names(ptab)[ptab>1],]
      tabi2 <- table(data[,issue_var])
    }
    del_i <- length(tabi)-length(tabi2)
    ni_del <- setdiff(names(tabi),names(tabi2))
    
    cat(c(del_i, " issues were deleted as they had less than ", imin*100, "% observations.","\n"))
    cat(c("The issues that were deleted are", ni_del, "\n"))
    
    # Recreate values and dimensions after deletion
    
    n_parties2 <- as.numeric(length(table(data[,party_var])))
    v_parties2 <- names(table(data[,party_var]))
    n_issues2 <- as.numeric(length(table(data[,issue_var])))
    v_issues2 <- names(table(data[,issue_var]))
    n_objects2 <- n_parties2+n_issues2
    n_years2 <- as.numeric(length(table(data[,year_var])))
    v_years2 <- names(table(data[,year_var]))
    n_party_year2 <- as.numeric(length(table(data$party_year)))
    v_party_year2<- names(table(data$party_year))
    
    # Initialize distance matrix:
    MAT <- matrix(nrow = n_objects2, ncol = n_objects2)
    
    # Create matrix:
    
    row = n_issues2 + 1
    for (x in v_party_year2) {
      column = 1
      for (y in v_issues2) {
        MDISS <- mean(data$diss[data$party_year == x & data[,issue_var] == y], na.rm = TRUE)
        MAT[row,column] <- MDISS
        MAT[column,row] <- MDISS
        column = column + 1
      }
      column = 1
      row = row + 1 
    }
    
    MAT[is.nan(MAT)] <- 10
    MAT[is.na(MAT)] <- 9
    MAT[MAT == 10] <- NA
    
    colnames(MAT) <- c(v_issues2,v_party_year2)
    #colnames(MAT) <- paste("v",colnames(MAT), sep="")
    
    # Create weight matrix:
    
    # Initialize matrix:
    WMAT <- matrix(0, nrow = n_objects2, ncol = n_objects2)
    
    # Fill in:
    
    # CELL option on => weights are computed separately for each election (100% = one election)
    
    if (cell==TRUE) {
      
      # One election (n_year == 1):
      
      if (n_years2 == 1){
        row = n_issues2 + 1
        n_obs = nrow(data)
        for (x in v_parties2){
          column = 1
          for (y in v_issues2){
            OBS <- nrow(data[data[,party_var] == x & data[,issue_var] == y,])
            WMAT[row,column] <- OBS/n_obs
            WMAT[column,row] <- OBS/n_obs
            column = column + 1
          }
          row = row + 1
        }
        colnames(WMAT) <- c(v_issues2,v_parties2)
        colnames(WMAT) <- paste("c",colnames(WMAT), sep="")
      }
      
      # More than one election (n_year > 1):
      
      
      if (n_years2>1) {
        row = n_issues2 + 1
        for (i in v_years2){
          n_obs = nrow(data[data[,year_var] == i,])
          for (x in v_parties2){
            OBSmin <- nrow(data[data[,party_var] == x & data[,year_var] == i,])
            if (OBSmin > min) {
              column = 1
              for (y in v_issues2){
                OBS <- nrow(data[data[,party_var] == x & data[,issue_var] == y & data[,year_var] == i,])
                WMAT[row,column] <- OBS/n_obs
                WMAT[column,row] <- OBS/n_obs
                column = column + 1
              }
              row = row + 1
            }
          }
        }
      }
      
    }
    # ROW option on => weights are computed separately for each election (100% = one party in one election)
    
    if (row == TRUE) {
      row = n_issues2 + 1
      for (x in v_party_year2) {
        column = 1
        n_obs = 1
        n_obs = nrow(data[data$party_year == x,])
        for (y in v_issues2){
          OBS <- nrow(data[data$party_year == x & data[,issue_var] == y,])
          WMAT[row,column] <- OBS/n_obs
          WMAT[column,row] <- OBS/n_obs
          column = column + 1
        }
        column = 1
        row = row + 1
      }
      
    }
    
    # if (row == TRUE) {
    # row = n_issues + 1
    # for (i in v_years) {
    #   for (x in v_parties){
    #     column = 1
    #     n_obs = 1
    #     n_obs = nrow(GM[GM$partys_all == x & GM$year_election == i,])
    #     for (y in v_issues){
    #       OBS <- nrow(GM[GM$partys_all == x & GM$issue_agg_all == y & GM$year_election == i,])
    #       WMAT[row,column] <- OBS/n_obs
    #       WMAT[column,row] <- OBS/n_obs
    #       column = column + 1
    #     }
    #     column = 1
    #     row = row + 1
    #   }
    # }
    # 
    # }
    
    MDSMAT <- list(MAT,WMAT, n_issues2, n_party_year2, n_parties2)
    return(MDSMAT)
    
  }    