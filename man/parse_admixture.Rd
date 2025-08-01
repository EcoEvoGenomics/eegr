% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_admixture.R
\name{parse_admixture}
\alias{parse_admixture}
\title{Instantiate new ADMIXTURE parser}
\usage{
parse_admixture(admixture_dir, sample_ids, popmap_path)
}
\arguments{
\item{admixture_dir}{Character singleton. Directory of Admixture output.}

\item{sample_ids}{Character vector. The IDs of samples input to Admixture.}

\item{popmap_path}{Path to .tsv with headerless columns for ID and pop}
}
\value{
An object of class admixture_parser
}
\description{
Instantiate new ADMIXTURE parser
}
