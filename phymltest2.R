phymltest <- 
function (seqfile, format = "interleaved", itree = NULL, exclude = NULL, 
          execname = NULL, append = TRUE) 
{
  os <- Sys.info()[1]
  if (is.null(execname)) {
    execname <- switch(os, Linux = {
      if (file.exists("/usr/bin/phyml")) "/usr/bin/phyml" else "phyml_3.0.1_linux32"
    }, Darwin = "phyml_3.0.1_macintel", Windows = "phyml_3.0.1_win32")
  }
  if (is.null(execname)) 
    stop("you must give an executable file name for PHYML")
  N <- length(.phymltest.model)
  format <- match.arg(format, c("interleaved", "sequential"))
  fmt <- rep("", N)
  if (format != "interleaved") 
    fmt[] <- "-q"
  boot <- rep("-b 0", N)
  mdl <- paste("-m", rep(c("JC69", "K80", "F81", "F84", "HKY85", 
                           "TN93", "GTR"), each = 4))
  tstv <- rep("-t e", N)
  inv <- rep(c("", "-v e"), length.out = N)
  alpha <- rep(rep(c("-c 1", "-a e"), each = 2), length.out = N)
  tree <- rep("", N)
  if (!is.null(itree)) 
    tree[] <- paste("-u ", itree)
  cmd <- paste(execname, "-i", seqfile, fmt, boot, mdl, tstv, 
               inv, alpha, tree, "--append ")
  outfile <- paste(seqfile, "_phyml_stats.txt", sep = "")
  if (!append) {
    unlink(outfile)
    unlink(paste(seqfile, "_phyml_tree.txt", sep = ""))
  }
  imod <- 1:N
  if (!is.null(exclude)) 
    imod <- imod[!.phymltest.model %in% exclude]
  for (i in imod) system(cmd[i])
  l <- readLines(outfile)
  l <- grep("Log-likelihood:", l, value = TRUE)
  if (dd <- length(l) - length(imod)) 
    l <- l[-(1:dd)]
  loglik <- as.numeric(sub(". Log-likelihood:", "", l))
  names(loglik) <- .phymltest.model[imod]
  class(loglik) <- "phymltest"
  loglik
}