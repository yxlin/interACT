% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-class.R
\docType{class}
\name{Behaviour-class}
\alias{Behaviour-class}
\title{An S4 class of Approaching/Crossing Road User Behaviour.}
\description{
A Behaviour object is to describe the behaviour of a road user, his/her
speed, direction, and position etc.
}
\section{Slots}{

\describe{
\item{\code{VX}}{a vector. Get position signal}

\item{\code{VY}}{a vector. Travelling along the X axis}

\item{\code{VXdot}}{a vector. -VSpeed. Save also the speed signal for later use}

\item{\code{VHeading}}{a vector. Always heading west}

\item{\code{VbYieldingeHMI}}{a logical vector. From eHMI section.}

\item{\code{idxCrossingOnsetSample}}{a integer vector.}

\item{\code{MCornerMatrices}}{a 3-D array}
}}

