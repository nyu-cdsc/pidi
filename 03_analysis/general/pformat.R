# Function for formatting p values 

# Values less than .07 will have be rounded out to 3 digits, 
# values greater than .07 will round to 2 digits
# Values less than .001 default to < .001 
# Correct punctuation is included (i.e., < vs =)

# You can also obtain the minimum p of multiple non-sig p's 
# or the maximum of multiple significant p's 

pformat <- function(pval, multiple_p) {
  if (missing(multiple_p)){
    newp <- format.pval(pval, eps = .001, digits = 2)
    if (newp <= .001) {
      return(sub("<", "< ", newp))
    } else {
      newp <- ifelse(newp < .07,
                     round(as.numeric(newp), 3),
                     round(as.numeric(newp), 2))
      return(sub("0.", "= 0.", newp))
    }
  } else if (multiple_p == "min") {
    if (is.list(pval) == FALSE & is.vector(pval) == FALSE){
      return(warning("You need to supply a list of p values to find the minimum p value from a group of p values.\n  If you meant to format a single p value, leave the second argument blank"))
    }
    newp <- round(min(pval[pval > .05]), 2)
    return(sub("0.", "> 0.", newp))
  } else if (multiple_p == "max") {
    if (is.list(pval) == FALSE & is.vector(pval) == FALSE){
      return(warning("You need to supply a list of p values to find the maximum p value from a group of p values. \n  If you meant to format a single p value, leave the second argument blank"))
    }
    newp <- round(max(pval[pval <= .05]), 3)
    return(sub("0.", "< 0.", newp))
  } else {
    return(warning("The second argument `multiple_p` only takes two arguments: `min` and `max`.\n  If you wanted to minimum p-value out of the group, supply `min` for the second argument.\n  If you wanted to get the maximum p-value of out of group, supply `max`"))
  }
}