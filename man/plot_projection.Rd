% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-projection.R
\name{plot_projection}
\alias{plot_projection}
\title{Plot PCA Projection}
\usage{
plot_projection(
  data.prcomp,
  dims = 1:2L,
  classes = NULL,
  scores = NULL,
  col = NULL,
  samples = NULL,
  pt_cex = 2.5,
  ...
)
}
\arguments{
\item{data.prcomp}{A prcomp class object. Typically the object
returned by \code{\link[=prcomp2]{prcomp2()}}.}

\item{dims}{\code{integer(2)}. Which dimensions to plot.}

\item{classes}{Optional. A vector indicating the classes of
samples used for coloring the points. Must be the same length
as the number of samples.}

\item{scores}{Optional. Statistical scores to pass through for the
coloring of the points during plotting. If a training data
set is passed and \code{scores = NULL},
then KS-distances (scores) will be calculated under the
hood and used to determine point color. Can be either a
single value (e.g. "red") or a vector of color values the
same length as the number of observations.
This overrides the point color assigned by \code{classes} above.}

\item{col}{\code{character(1)}. The color of the points. Can be either
a single value (e.g. "red") or a vector of color values the
same length as the number of observations.
This parameter overrides the point colors determined by
both the \code{classes} and \code{scores} parameters above.}

\item{samples}{Optional. A vector of sample IDs to mark
on the projection plot. Specified samples are marked
with a hollow diamond (see \code{\link[=pch]{pch()}}).}

\item{pt_cex}{\code{numeric(1)}. Character expansion for the points.}

\item{...}{Additional arguments passed to \code{\link[=plot_pca_dims]{plot_pca_dims()}}.}
}
\description{
Plot the samples from a PCA projection in a 2-dimensional scatter plot.
}
\examples{
pca <- center_scale(pcapkg:::log10_ft(simdata), center = TRUE, scale = FALSE) |>
  feature_matrix() |>
  prcomp2()

# Define color of points
plot_projection(pca, col = "green")

# Use classes to define point colors
plot_projection(pca, classes = simdata$class_response)

# Mark specific samples
plot_projection(pca, samples = rownames(simdata)[1:5L])
}
\seealso{
Other PCA plots: 
\code{\link{plot_pca_dims}()},
\code{\link{plot_rotation}()},
\code{\link{screeplot_auc}()}
}
\author{
Stu Field, Michael R. Mehan
}
\concept{PCA plots}
