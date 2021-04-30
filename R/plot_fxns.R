# gamma_plot <- function (x, selection = "median", cols = set_gamma_colors(x),
#                         xname = NULL, together = FALSE, LDATS = FALSE)
# {
#   oldpar <- par(no.readonly = TRUE)
#   on.exit(par(oldpar))
#   if (LDATS) {
#     par(fig = c(0, 1, 0, 0.3), new = TRUE)
#   }
#   else if (together) {
#     par(fig = c(0, 1, 0, 0.52), new = TRUE)
#   }
#   else {
#     par(fig = c(0, 1, 0, 1))
#   }
#   rhos <- x$rhos
#   nrhos <- ncol(rhos)
#   if (!is.null(nrhos)) {
#     if (selection == "median") {
#       spec_rhos <- ceiling(apply(rhos, 2, median))
#     }
#     else if (selection == "mode") {
#       spec_rhos <- apply(rhos, 2, modalvalue)
#     }
#     else {
#       stop("selection input not supported")
#     }
#   } else {
#     spec_rhos <- NULL
#   }
#   x$control$timename <- NULL
#   seg_mods <- multinom_TS(x$data, x$formula, spec_rhos, x$timename,
#                           x$weights, x$control)
#   nsegs <- length(seg_mods[[1]])
#   t1 <- min(x$data[, x$timename])
#   t2 <- max(x$data[, x$timename])
#   if (is.null(xname)) {
#     xname <- x$timename
#   }
#   par(mar = c(4, 5, 1, 1))
#   plot(1, 1, type = "n", bty = "L", xlab = "", ylab = "",
#        xaxt = "n", yaxt = "n", ylim = c(0, 1), xlim = c(t1 -
#                                                           1, t2 + 1))
#   yax <- round(seq(0, 1, length.out = 5), 3)
#   axis(2, at = yax, las = 1)
#   axis(1)
#   mtext(side = 2, line = 3.5, cex = 1.25, "Proportion")
#   mtext(side = 1, line = 2.5, cex = 1.25, xname)
#   ntopics <- ncol(as.matrix(x$data[[x$control$response]]))
#   seg1 <- c(0, spec_rhos[-length(rhos)])
#   seg2 <- c(spec_rhos, t2)
#   time_obs <- rep(NA, nrow(x$data))
#   pred_vals <- matrix(NA, nrow(x$data), ntopics)
#   sp1 <- 1
#   for (i in 1:nsegs) {
#     mod_i <- seg_mods[[1]][[i]]
#     spec_vals <- sp1:(sp1 + nrow(mod_i$fitted.values) -
#                         1)
#     pred_vals[spec_vals, ] <- mod_i$fitted.values
#     time_obs[spec_vals] <- mod_i$timevals
#     sp1 <- sp1 + nrow(mod_i$fitted.values)
#   }
#   for (i in 1:ntopics) {
#     points(time_obs, pred_vals[, i], type = "l", lwd = 3,
#            col = cols[i])
#   }
#   if (!is.null(spec_rhos)) {
#     rho_lines(spec_rhos)
#   }
# }
#
#
#
# rho_plot <- function (x)
# {
#
#   rhos <- as.data.frame(x$rhos) %>%
#     tidyr::pivot_longer(everything(.), names_to = "changepoint", values_to = "estimate", names_prefix = "V") %>%
#     mutate(changepoint  = as.factor(changepoint))
#
#   start <- min(x$data$year)
#   end <- max(x$data$year)
#
#   ggplot(rhos, aes(estimate, group = changepoint, fill  = changepoint)) +
#     geom_histogram(alpha = .5, position = "identity") +
#     theme_bw() +
#     scale_color_viridis_d() +
#     xlim(c(start, end))
#
# }
#
# plot_lda_comp <- function(fitted_lda, specl = F) {
#
#   lda_betas <- data.frame(t(fitted_lda[[1]]@beta))
#
#   colnames(lda_betas) <- c(1:ncol(lda_betas))
#
#   lda_betas$species <- (unlist(fitted_lda[[1]]@terms))
#
#   lda_betas <- tidyr::pivot_longer(lda_betas, -species, names_to = "topic", values_to = "beta")
#
#   lda_betas$beta <- exp(lda_betas$beta)
#
#   betas_plot <- ggplot(lda_betas, aes(species, beta, fill = topic)) +
#     geom_col(position = "stack") +
#     theme_void() +
#     scale_fill_viridis_d(end = .7)
#
#   if(specl) {
#     betas_plot <- betas_plot +
#       theme(axis.text.x = element_text(angle = 0))
#   }
#
#
#   return(betas_plot)
# }
#
# plot_lda_year <- function(fitted_lda, covariate_data) {
#
#   if(is.list(fitted_lda)) {
#     fitted_lda <- fitted_lda[[1]]
#   }
#
#   lda_preds <- data.frame(fitted_lda@gamma)
#
#   colnames(lda_preds) <- c(1:ncol(lda_preds))
#
#   stopifnot(nrow(lda_preds) == length(covariate_data))
#
#   lda_preds$year <- covariate_data
#
#   lda_preds <- tidyr::pivot_longer(lda_preds, -year, names_to = "topic",values_to = "proportion")
#
#
#   pred_plot <- ggplot(lda_preds, aes(year, proportion, color = topic)) +
#     geom_line(size = 2) +
#     theme_bw() +
#     scale_color_viridis_d(end = .7)
#
#   return(pred_plot)
# }
