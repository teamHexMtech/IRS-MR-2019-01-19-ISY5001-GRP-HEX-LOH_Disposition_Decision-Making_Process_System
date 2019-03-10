# Commonality Test Tester

# to find check with nearby with the input from lotid and measstep from users



library(data.table)



dtpath <- '/home/iss-user/Downloads/rawData.csv'

lotlistpath <- '/home/iss-user/Downloads/lotList.csv'

processdatapath <- '/home/iss-user/Downloads/processData.csv'

processresultpath <- '/home/iss-user/Downloads/processResult.csv'



#read the raw data file

dt<- fread(dtpath)

lotlist <- fread(lotlistpath)

processdata <- fread(processdatapath, header=F)



lotid <- processdata[1,1][[1]]

measstepid <- processdata[1,2][[1]]

testerid <- lotlist[lot==lotid & measstep==measstepid]$Tester

loadboardid <- lotlist[lot==lotid&measstep==measstepid]$Loadboard

handlerid <- lotlist[lot ==lotid& measstep == measstepid]$HandlerID

#testprg <- lotlist[lot==lotid&measstep==measstepid]$TestprgName



#lotid?

#measstep? 

Commonality_HBIN <- function(dt, lotid, measstepid,testerid, loadboardid, handlerid){

  res <- data.table(lot = NA,currentTest = NA, Tester=NA, HandlerID = NA, Loadboard = NA, measstep = NA, violation =NA, p=NA, parameter=NA)

  res <- res[!is.na(lot)]

  temp <- dt[measstep == measstepid]

  lottime <- temp[lot == lotid]$timestamp

  temp[, thislottime:=lottime[[1]]]

  temp[, diffthislot:=abs(difftime(timestamp, thislottime))]

  tempNearLot <- unique(temp[, .(lot, timestamp, thislottime, diffthislot)])

  tempNearLot <- tempNearLot[order(diffthislot)]

  

  tempNearLot <- tempNearLot[1:11,]

  #####################

  

  tempthislot<- dt[lot==lotid & measstep == measstepid]

  HBINsel <- unique(tempthislot$HBIN)[unique(tempthislot$HBIN) != 1]

  templot <- temp[lot %in% unique(tempNearLot$lot)]



# loop different test 

  currtest <- c('Tester','Loadboard', 'HandlerID', 'TestprgName')

  for (j in 1:length(currtest)){

    # loop by HBIN level 

    for (i in 1:length(HBINsel)){

      #print(paste('j = ', j, ' & i = ', i, sep=''))

      #filter Test number for this HBIN

      tempP <- tempthislot[HBIN == HBINsel[i]][, N/sum(tempthislot$N)*100, by=LBIN_P_NAME][order(V1, decreasing =T)]

      tempP <- tempP[V1 > 0.05]

      

      # Individial Test number / Total N for this lot  (percentage of TN failed in this lot)

      temp2 <- templot[, N/sum(templot$N[templot$lot %in% lot & templot$measstep %in% measstep])*100, by=c(currtest[j], 'LBIN_P_NAME', 'HBIN', 'lot')]

      # choose only filtered test number from tempP

      temp2 <- temp2[LBIN_P_NAME %in% tempP[1:10,]$LBIN_P_NAME]

      # sum of percentage of TN (accross all socketnumber)

      temp2 <- temp2[, sum(V1), by=c(currtest[j], 'LBIN_P_NAME', 'lot')]

      

      

      if (nrow(temp2) > 0 & length(unique(temp2[,1])[[1]]) > 1 ){

        a <- dcast(temp2, paste(colnames(temp2)[1], '+',colnames(temp2)[3],'~',colnames(temp2)[2]), value.var = 'V1')

        # colnames[1] = e.g. loadboard, colnames[2] = LBIN_P_NAME, colnames[3] = lot

        a[is.na(a)] <- 0

        

        # remove single lot at Tester

        a.check <- a[, .N, by=c(currtest[j])]

        #a <- a[!(Tester  %in% a.check[N==1]$Tester)]

        acol <- colnames(a[, 3:ncol(a)])

        

        # check if column all zero after some tester was filtered out from previous execution

        a.check2 <- a[, lapply(.SD, sum, na.rm =T), .SDcols = acol]

        acolx <- acol[t(a.check2==0)]

        

        if(length(acolx!=0)){

          a <- a[,!acolx,with=F]

        }

        

        if(!(unique(tempthislot[, currtest[j], with=F])[[1]] %in% a.check[N==1][[1]])){

          # singularity check

          # if(ncol(a) > 3 & (nrow(a) > ncol(a[, 3:ncol(a)]))){

          #   acolx2 <- correlationcheck(as.matrix(a[, 3:ncol(a)]))

          # }else{acolx2 <- 0}

          # 

          # if(length(acolx2)!=0){

          #   a<-  a[, -c(acolx2 + 2), with =F]

          # }

          

          if(length(unique(a[[1]]))>1){

            if(ncol(a)>3){

              res.man <- aov(as.matrix(a[, 3:ncol(a)]) ~ a[[1]], data = a)

              res.man.sum <- summary.aov(res.man)

              response <- list()

              pvalue <- list()

              for(k in 1:length(res.man.sum)){

                response <- append(response , names(summary.aov(res.man))[[k]])

                pvalue <- append(pvalue, summary.aov(res.man)[[k]][,"Pr(>F)"][1])

              }

            }else if(ncol(a) == 3){

              res.man <- aov(a[[3]] ~ a[[1]], data = a)

              pvalue <- summary.aov(res.man)[[1]][1,5]

              response <- colnames(a)[3]

            }

            

            res <- rbind(res, cbind(lot = lotid,currentTest=currtest[j],Tester=testerid,HandlerID=handlerid,Loadboard=loadboardid, measstep = measstepid, violation = paste('HBIN',HBINsel[i],sep=''), p = unlist(pvalue), parameter = unlist(response)))

          }else{

            res <- rbind(res, cbind(lot = lotid,currentTest=currtest[j],Tester=testerid,HandlerID=handlerid,Loadboard=loadboardid,measstep = measstepid, violation = paste('HBIN',HBINsel[i],sep=''), p =NA, parameter = paste(colnames(a)[3:ncol(a)], 'failed parameters only found in this tester')))

            

          }

        }

        

        

      }

    }





  }

  return(res)

}



SocketCommonality_HBIN <- function(dt, lotid, measstepid,testerid, loadboardid, handlerid){

  res <- data.table()

  temp<- dt[lot==lotid]

  temp <- temp[Tester == testerid]

  temp <- temp[measstep == measstepid]

  temp <- temp[HandlerID == handlerid]

  temp <- temp[Loadboard == loadboardid]

  HBINsel <- unique(temp$HBIN)[unique(temp$HBIN) != 1]

  # loop by HBIN level

  for (i in 1:length(HBINsel)){

    tempP <- temp[HBIN == HBINsel[i]][, sum(N), by=LBIN_P_NAME][order(V1, decreasing =T)]

    temp2 <- temp[HBIN==HBINsel[i]][LBIN_P_NAME %in% tempP[1:10,]$LBIN_P_NAME]

    

    a <- dcast(temp2, LBIN_P_NAME ~ SocketNumber , value.var='N')

    a[is.na(a)] <- 0

    if(ncol(a) > 2){

      chisq <- chisq.test(a[, 2:ncol(a)])

      p.value <- chisq$p.value

      res <- rbind(res, cbind(lot = lotid,currentTest='Socket',Tester=testerid,HandlerID=handlerid,Loadboard=loadboardid, measstep = measstepid, violation = paste('HBIN',HBINsel[i],sep=''), p =p.value, parameter = NA))

     

    }  

  }

  

  return(res)

  

  # contrib <- 100*c$residuals^2/c$statistic

  # corrplot(c$residuals, is.cor = FALSE)

  # corrplot(contrib, is.cor=FALSE)

} 



keltempcheck_HBIN <- function(dt,lotid, measstepid, testerid, loadboardid, handlerid){

  res <- data.table()

  temp<- dt[lot==lotid]

  temp <- temp[Tester == testerid]

  temp <- temp[measstep == measstepid]

  temp <- temp[HandlerID == handlerid]

  temp <- temp[Loadboard == loadboardid]

  tempthislot<- dt[lot==lotid & measstep == measstepid]

  tempP <- temp[, N/sum(tempthislot$N)*100, by=LBIN_P_NAME][order(V1, decreasing =T)]

  tempP <- tempP[LBIN_P_NAME != '']

  tempP <- tempP[V1 > 0.05]

  if(nrow(tempP) > 0){

    kelvin <- tempP[grepl('KEL', LBIN_P_NAME)]

    temperature <- tempP[grepl('TEMP', LBIN_P_NAME)]

    if((nrow(kelvin) >0)|(nrow(temperature)>0) ) {

      res <- cbind(lot = lotid,currentTest='kelTemp',Tester=testerid,HandlerID=handlerid,Loadboard=loadboardid, measstep = measstepid, violation = 'kelTemp', p = 0, parameter = NA)

    }

  }

  return(res)

}



res1 <- Commonality_HBIN(dt, lotid, measstepid, testerid, loadboardid, handlerid)

res2 <- SocketCommonality_HBIN(dt, lotid, measstepid, testerid, loadboardid, handlerid)

res3 <- keltempcheck_HBIN(dt, lotid, measstepid, testerid, loadboardid, handlerid)



res <- rbind(res1, res2, res3)

res[, commonality:=ifelse(as.numeric(p)< 0.05, 'Y', 'N')]



res.sum <- res[, .(currentTest,commonality)][commonality == 'Y']

finalres <- data.table(SocketNumber=NA,Loadboard=NA,Tester=NA,Handler=NA,TestProgram=NA,Kelvintemp=NA)

finalres <- finalres[!is.na(SocketNumber)]



res.match <- c('Socket','Tester', 'Loadboard', 'HandlerID', 'TestprgName', 'kelTemp')  %in% res.sum[[1]]

finalres <- rbind(finalres, t(cbind(res.match)), use.names=F)

finalres[finalres == TRUE] <- 'Y'

finalres[finalres == FALSE] <- 'N'





write.csv(finalres, processresultpath)



