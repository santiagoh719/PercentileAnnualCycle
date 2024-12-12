MEP <- function(Nday_vec, PP_vec, Per, Window = 29L){

  checks <- makeAssertCollection()
  assert_numeric(Nday_vec,
                 lower = 0, upper = 366,
                 finite = TRUE, add = checks)
  assert_numeric(PP_vec, finite = TRUE, add = checks)
  assert_integer(Window, len = 1, add = checks)
  assert_numeric(Per, len = 1, finite = TRUE,add = checks)
  reportAssertions(checks)
  if(length(Nday_vec) != length(PP_vec)){
    stop('Nday_vec must have the same length of PP_vec')
  }

  df<- data.frame(Ndia= Nday_vec,
              PP= PP_vec)

  N<- 365
  aux<- data.frame(Nday=1:N,
               MovingPer=NA)

  n<- round(Window/2)

  for(k in 1:nrow(aux)){

    ### Ndia de 1 a n
    if(k %in% 1:n){

      distri<- df[df$Ndia<= (k+n) |
                    df$Ndia>=  ( (N-n)+k ), ]

      aux$MovingPer[k]<- quantile(distri$PP, probs= Per, na.rm= T )

    }

    ## Ndia entre n+1 y N-n
    else if(k %in% (n+1):(N-n)){

      distri<- df[df$Ndia %in% (k-n):(k+n),]

      aux$MovingPer[k]<- quantile(distri$PP, probs= Per, na.rm= T )

    }

    ## Ndia de (N-n)+1 a N
    else if(k %in% ((N-n)+1) : N ) {

      distri<- df[df$Ndia %in% (k-n):N |
                    df$Ndia <= (Window - length( (k-n):N)), ]

      aux$MovingPer[k]<- quantile(distri$PP, probs= Per, na.rm= T )

    }
  }
  if(any(is.na(aux$MovingPer))){
    vec_aux <- is.na(aux$MovingPer)
    id_na <- which(vec_aux)
    id_ok <- which(!vec_aux)
    for(ii in id_na){
      dist1 <- abs(ii - id_ok)
      dist2 <- abs(ii - id_ok-365)
      dist3 <- abs(ii - id_ok+365)
      if(min(dist1) < min(dist2) &
         min(dist1) < min(dist3)){
        aux$MovingPer[ii] <- aux$MovingPer[id_ok[which.min(dist1)]]
      }else if(min(dist2) < min(dist3)){
        aux$MovingPer[ii] <- aux$MovingPer[id_ok[which.min(dist2)]]
      }else{
        aux$MovingPer[ii] <- aux$MovingPer[id_ok[which.min(dist3)]]
      }
    }
  }
  return(aux)
}

GAM_AnnualCycle <- function(vec, N_basicfun =27L, fam ='gaussian'){
  checks <- makeAssertCollection()
  assert_numeric(vec, finite = TRUE,len = 365, add = checks)
  assert_integer(N_basicfun, len = 1, add = checks)
  assert_character(fam, len = 1, add = checks)
  reportAssertions(checks)

  Ndia <- 1:365
  df<-rbind(
    rbind(
      data.frame(Nday= Ndia-365,
                 PP= vec),
      data.frame(Nday= Ndia,
                 PP= vec)
    ),
    data.frame(Nday= Ndia+365,
               PP= vec)
  )

  aux<- data.frame(Nday= Ndia,
               GAM=NA)

  modelo<- gam(formula = PP ~ s(Nday, k=N_basicfun), data = df,
               family = fam, method='REML')
  aux$GAM<- modelo$fitted.values[366:730]

  return(aux)

}
