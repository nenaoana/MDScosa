pd_labelattr <- function
(data,
 party_var,
 issue_var,
 coordmat)
{
  labs <- c(attr(data, "label.table")[[party_var]],
            attr(data, "label.table")[[issue_var]])
  clabs <- as.integer(rownames(coordmat))
  mylabs <- names(labs[match(clabs,labs)])
  return(mylabs)
}
