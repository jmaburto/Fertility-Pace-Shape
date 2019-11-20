area_under_curve <- function(x, y) {
  delta_x <- diff(x)
  if (any(delta_x <= 0)) stop("area_under_curve: x should be ascending")
  rect_heights <- (y[-1] + y[-length(y)]) / 2
  return(sum(delta_x * rect_heights))
}

shape_rep2 <- function(rep, xmin = NULL, xmax = NULL, 
                      fertTable = FALSE) {
  if(class(rep) %in% "numeric") {
    mx <- rep
    x <- seq_along(mx) - 1
  }
  if(class(rep) %in% c("list", "data.frame")) {
    if(!all(c("x", "mx") %in% names(rep))) {
      stop("'rep' doesn't contain both x and mx")
    }
    x <- rep$x
    mx <- rep$mx
    if(length(x) != length(mx)) {
      stop("x and mx must be the same length")
    }
  }
  if(any(duplicated(x))) stop("all x must be unique values")
  if(any(diff(x) <= 0)) stop("much as we'd like to reverse aging, x must all be ascending")
  if(any(mx[!is.na(mx)] < 0)) stop("You appear to have minus-babies (check mx)")
  if(any(length(xmin) > 1, length(xmax) > 1)){
    stop("xmin and xmax must have length 1 or NULL")
  }
  ltdim <- length(x) 
  if(is.null(xmin)) xmin_fix <- x[min(which(mx > 0))]
  if(!is.null(xmin)) xmin_fix <- xmin
  if(is.null(xmax)) { 
    if(is.na(mx[ltdim])) {
      x_fix <- x
      ltdim_fix <- length(x_fix)
      xmax_fix <- max(x_fix)
      mx_fix <- mx
      x_sub <- x_fix[x_fix >= xmin_fix & x_fix <= xmax_fix]
      ltdim_sub <- length(x_sub)
      mx_sub <- mx_fix[x >= xmin_fix & x <= xmax_fix]
    }
    if(!is.na(mx[ltdim])) {
      x_fix <- c(x, x[ltdim] + (x[ltdim] - x[ltdim - 1]))
      ltdim_fix <- length(x_fix)
      xmax_fix <-  max(x_fix)
      mx_fix <- c(mx, NA)
      x_sub <- x_fix[x_fix >= xmin_fix & x_fix <= xmax_fix]
      ltdim_sub <- length(x_sub)
      mx_sub <- mx_fix[x_fix >= xmin_fix & x_fix <= xmax_fix]
    }
  }
  if(!is.null(xmax)){ 
    if(is.na(mx[which(x == xmax)])){
      x_fix <- x
      ltdim_fix <- length(x_fix)
      xmax_fix <- xmax
      mx_fix <- mx
      x_sub <- x_fix[x_fix >= xmin_fix & x_fix <= xmax_fix]
      ltdim_sub <- length(x_sub)
      mx_sub <- mx_fix[x_fix >= xmin_fix & x_fix <= xmax_fix]
    }
    if(!is.na(mx[which(x == xmax)])){
      x_fix <- x
      ltdim_fix <- length(x_fix)
      xmax_fix <- xmax
      mx_fix <- mx
      x_sub <- x_fix[x_fix >= xmin_fix & x_fix <= xmax_fix]
      ltdim_sub <- length(x_sub)
      mx_sub <- mx_fix[x >= xmin_fix & x <= xmax_fix]
      mx_sub[xmax_fix] <- NA
    }
  }
  if(ltdim_sub <= 2 ) {
    stop("must have > 2 values of mx to calculate shape")
  }
  lt_sub_int <- diff(x_sub)
  Bx_sub <- c(0, cumsum(mx_sub[seq(1, ltdim_sub-1, 1)]) * lt_sub_int)
  B <- max(Bx_sub)
  x_std <- (x_sub - xmin_fix) / (xmax_fix - xmin_fix)
  # standardised mx has mean of 1
  # last "fix" class is NA as no more offspring past the start of the class
  mx_std <- (mx_sub / B) * (xmax_fix - xmin_fix)
  Bxmin <- Bx_sub[which.min(x_std)]
  Bxmax <- Bx_sub[which.max(x_std)]
  Bx_std <- (Bx_sub - Bxmin) / (Bxmax - Bxmin) 
  auc_std <- area_under_curve(x_std, Bx_std)
  auc_flat <- 0.5
  shape <- auc_std - auc_flat
  if(!fertTable) return(shape)
  if(fertTable) { 
    fertTable <- data.frame(x = x_sub,
                            mx = mx_sub,
                            Bx = Bx_sub,
                            xStd = x_std,
                            mxStd = mx_std,
                            BxStd = Bx_std)
    return(list(shape = shape, fertTable = fertTable))
  }
}


pace_rep2 <- function(rep, xmin = NULL, xmax = NULL, fertTable = FALSE){
    if (class(rep) %in% "numeric") {
        mx <- rep
        x <- seq_along(mx) - 1
    }
    if (class(rep) %in% c("list", "data.frame")) {
        if (!all(c("x", "mx") %in% names(rep))) {
            stop("'rep' doesn't contain both x and mx")
        }
        x <- rep$x
        mx <- rep$mx
        if (length(x) != length(mx)) {
            stop("x and mx must be the same length")
        }
    }
    shape <- shape_rep(rep = data.frame(x = x, mx = mx), 
                       xmin = xmin, xmax = xmax, fertTable = TRUE)
    shape_xmin <- min(shape$fertTable$x)
    shape_xmax <- max(shape$fertTable$x)
    pace <- (shape_xmax - shape_xmin) * (0.5 - shape$shape)
    if (!fertTable) return(pace)
    if (fertTable) return(list(pace = pace, fertTable = shape$fertTable))
}

