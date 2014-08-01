function (pts1, pts2, poly, nsim, s, quiet = FALSE)
{
    kmax <- rep(0, length = length(s))
    kmin <- rep(1e+34, length = length(s))
    diff <- matrix(0, ncol = length(s), nrow = nsim)
    q.2.5 <- rep(0, length = length(s))
    q.97.5 <- rep(0, length = length(s))
    for (isim in (1:nsim)) {
        if (!quiet)
            cat("Doing labelling ", isim, "/", nsim, "\n")
        labpts <- rLabel(pts1, pts2)
        k1sim <- khat(labpts[[1]], poly, s)
        k2sim <- khat(labpts[[2]], poly, s)
        diffk <- k1sim - k2sim
        kmax <- pmax(kmax, diffk)
        kmin <- pmin(kmin, diffk)
        diff[isim, ] <- diffk
    }
    for (i in 1:length(s)) {
        q.2.5[i] <- quantile(diff[, i], probs = 0.025)
        q.97.5[i] <- quantile(diff[, i], probs = 0.975)
    }
    list(lower = kmin, upper = kmax, q.2.5 = q.2.5, q.97.5 = q.97.5)
}
