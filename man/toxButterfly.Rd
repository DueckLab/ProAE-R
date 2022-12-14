% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toxButterfly.R
\name{toxButterfly}
\alias{toxButterfly}
\title{Create symmetrical PRO-CTCAE bar plots over time between two groups/arms.}
\usage{
toxButterfly(
  dsn,
  proctcae_var,
  id_var,
  cycle_var,
  arm_var,
  arm_labels = c("Arm A", "Arm B"),
  cycle_order = T,
  missing = T,
  plot_data = F,
  plot_limit = NA,
  colors = 1
)
}
\arguments{
\item{dsn}{A data.frame object with PRO-CTCAE data}

\item{proctcae_var}{A character string. Name of the PRO-CTCAE Variable to be
graphed by the function. Can be a composite, scale or indicator PRO-CTCAE
variable and it does not need to be labeled.}

\item{id_var}{A character string.Name of ID variable differentiating each
PRO-CTCAE survey/participant entered as a quoted string.}

\item{cycle_var}{A character string. Name of cycle variable associated with
time.}

\item{arm_var}{A character string. Name of arm variable differentiating
treatment groups. Must be character or factor class.}

\item{arm_labels}{A vector with two elements of character type. Labels of the
arm/group variable if different from the current labels. The first will be
left side of the butterfly and the second the right side of the plot.}

\item{cycle_order}{Logical. T for "top-bottom" displaying the first time at
the top versus F for "bottom-top" the first level will be at the bottom.
Defaults to \code{TRUE}.}

\item{missing}{Logical. Determines how the missing data will be displayed in
the graph. T would exclude the missing data from the bar chart and would
display the N along the side of the plot. F would include the missing data
into the bar plot as the first level. Defaults to \code{TRUE}.}

\item{plot_limit}{A number. Limit the number of cycles to be plotted up to
and including a given cycle number. All available cycle time points are
plotted if no cycle number is provided. Defaults to \code{NA}.}

\item{colors}{A number. Specify the coloring scheme of symptom grades within
frequency bars. Options include: 1 = Blue and red color shading, 2 =
qualitative color shades (color blind friendly), 3 = black and white.
Defaults to 1.}

\item{return_data}{Logical. Return the data used to construct plots as element
of the returned object as well as all of the elements used to create all
plots created by the function. Defaults to \code{FALSE}.}
}
\value{
A plot object. The returned object is a symmetrical butterfly plot made
  up by two stacked sideways barplots a legend and labels. When
  \code{return_data} is set to \code{TRUE} the function returns a list of
  objects. The first object is the butterfly figure the next objects are the
  plots or parts of the plots that make up the butterfly and the last items
  in the list are data elements used to create the parts of the plot.
}
\description{
Data format should be in 'long' format, where each PRO-CTCAE item is a
	  variable/column. PRO-CTCAE item must have format type of yn_2_fmt,
	  frq_5_fmt, int_5_fmt, or sev_5_int.
}
\examples{
# --- Simulate example PRO-CTCAE data
\donttest{
acute_butterfly = toxButterfly(dsn = ProAE::tox_acute,
 id_var = "id",
 proctcae_var = "PROCTCAE_78A_SCL",
 cycle_var = "time",
 arm_var = "arm",
 colors = 1,
 plot_data = TRUE,
 missing = FALSE)
acute_butterfly
}
}
