library(dplyr)
x

#1
mean(is.na(x))



sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

meano <- function(x) {
  mean(is.na(x))
}


#2
x / sum(x, na.rm = TRUE)

summa <- function(x){
  x / sum(x, na.rm = TRUE)
}

d <- rnorm(10)
str(d)
sum(d)

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}


ab <- c('dada', 'kaka', 'mama', 'baba', 'mjomba','mama','mama','mama')
ad <- c(5,1,3,9,5,4,4,1)
xzc <- cbind(ab,ad)
xzc <- as.data.frame(xzc)

ag <- c('dada', 'kaka', 'mama', 'baba', 'mjomba')
ah <- c(1,2,3,1,4)
xza <- cbind(ag,ah)
xza <- as.data.frame(xza)


xzc$ad <- as.numeric(xzc$ad)
xza$ag <- as.numeric(xza$ah)


kipimo <- function(x) {
  x <- [xzc$ab, xza$ag,xzc$ad,xza$ah]
  if(all(xzc$ab == xza$ag, xzc$ad >= xza$ah)) {
    return('KUBWA')
  } else {
    return('KAWAIDA')
  }
}




new_xyz <- xzc %>% 
  mutate(overweight = kipimo())

View(new_xyz)
skimr::skim(xzc)

missing_az <- function(x) {
  x <- xzc$ab
  y <- xzc$ad
  if (is.na(x)) {
    return(NULL)
  } else {return(y)}
}


xzc %>% 
  missing_az(ab %in% c('mama'))
  
kubwa_kuliko <- function(x) {
  if (x >= 2.5) { return('KUBWA')} else
  {return('NDOGO')}
}

kubwa_kuliko(2)  
  
  
  
