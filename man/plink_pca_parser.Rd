% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_plink_pca.R
\name{plink_pca_parser}
\alias{plink_pca_parser}
\title{Class for PLINK PCA parsers}
\description{
An object of class plink_pca_parser helps you easily parse
results from a PLINK PCA.
}
\section{Super class}{
\code{eegr::POPGEN Parser} -> \code{PLINK PCA Parser}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PLINK PCA Parser-new}{\code{plink_pca_parser$new()}}
\item \href{#method-PLINK PCA Parser-get_variance_explained}{\code{plink_pca_parser$get_variance_explained()}}
\item \href{#method-PLINK PCA Parser-get_coordinates}{\code{plink_pca_parser$get_coordinates()}}
\item \href{#method-PLINK PCA Parser-clone}{\code{plink_pca_parser$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PLINK PCA Parser-new"></a>}}
\if{latex}{\out{\hypertarget{method-PLINK PCA Parser-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{plink_pca_parser$new(eigenval_path, eigenvec_path, popmap_path)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{eigenval_path}}{Path to PLINK output with .eigenval extension}

\item{\code{eigenvec_path}}{Path to PLINK output with .eigenvec extension}

\item{\code{popmap_path}}{Path to .tsv with headerless columns for ID and pop}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PLINK PCA Parser-get_variance_explained"></a>}}
\if{latex}{\out{\hypertarget{method-PLINK PCA Parser-get_variance_explained}{}}}
\subsection{Method \code{get_variance_explained()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{plink_pca_parser$get_variance_explained(pc, signif = 3, as_percent = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pc}}{Vector of which principal components to return for}

\item{\code{signif}}{Number of significant figures}

\item{\code{as_percent}}{Logical. Return as percent (default) or fraction?}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PLINK PCA Parser-get_coordinates"></a>}}
\if{latex}{\out{\hypertarget{method-PLINK PCA Parser-get_coordinates}{}}}
\subsection{Method \code{get_coordinates()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{plink_pca_parser$get_coordinates(pc = c(1, 2), include_pops = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pc}}{Vector of which principal components to return}

\item{\code{include_pops}}{Include sample population in output table}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PLINK PCA Parser-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PLINK PCA Parser-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{plink_pca_parser$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
