% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/references.R
\docType{data}
\name{referencesDatabase}
\alias{referencesDatabase}
\alias{reference}
\alias{referenceSets}
\title{References}
\format{
Each reference is a data.frame including 4 columns:
\describe{
\item{TAX}{The taxon to which each reference bone belongs.}
\item{EL}{The skeletal element.}
\item{Measure}{The type of measurement taken on the bone.}
\item{Standard}{The value of the measurement taken on the bone.
All the measurements are expressed in millimetres.}
}




}
\usage{
reference

referenceSets

referencesDatabase
}
\description{
Several osteometrical references are provided in \pkg{zoolog} to enable
researchers to use the one of their choice. The user can also use their
own osteometrical reference if preferred.
}
\section{Data Source}{

Currently, the references include reference values for the main domesticates
and their agriotypes (\emph{Bos}, \emph{Ovis}, \emph{Capra},
\emph{Sus}), and other less frequent species, such as red deer and donkey,
drawn from the following publications and resources:
\describe{
\item{\strong{Cattle - \emph{Bos}}}{\describe{
\item{Nieto}{
\emph{Bos taurus}.
Female cow dated to the Early Bronze Age (Minferri, Catalonia),
in \insertCite{nieto2018element;textual}{zoolog}.}
\item{Basel}{
\emph{Bos taurus}.
Inv.nr. 2426 (Hinterwälder; female; 17 years old;
live weight: 340 kg; withers height: 113 cm),
from \insertCite{stopp2018Basel;textual}{zoolog}.}
\item{Johnstone}{
\emph{Bos taurus}.
Standard values from means of cattle measures from Period II (Late Iron
Age to Romano-British transition) of Elms Farm, Heybridge
\insertCite{johnstone2002late}{zoolog}.}
\item{Degerbøl}{
\emph{Bos primigenius}.
Female aurochs from \insertCite{degerbol1970urus;textual}{zoolog}.}
}}
\item{\strong{Sheep - \emph{Ovis}}}{\describe{
\item{Davis}{
\emph{Ovis aries}.
Mean values of measurements from a group of adult female Shetland sheep
skeletons from a single flock
\insertCite{davis1996measurements}{zoolog}.}
\item{Clutton}{
\emph{Ovis aries}.
Mean measurements from a group of male Soay sheep of known age
\insertCite{clutton1990osteology}{zoolog}.}
\item{Basel}{
\emph{Ovis musimon}.
Inv.nr. 2266 (male; adult),
from \insertCite{stopp2018Basel;textual}{zoolog}.}
\item{Uerpmann}{
\emph{Ovis orientalis}.
Field Museum of Chicago catalogue number: FMC 57951 (female; western Iran)
from \insertCite{uerpmann1994animal;textual}{zoolog}.}
}}
\item{\strong{Goat - \emph{Capra}}}{\describe{
\item{Basel}{
\emph{Capra hircus}.
Inv.nr. 1597 (male; adult), from
\insertCite{stopp2018Basel;textual}{zoolog}.}
\item{Clutton}{
\emph{Capra hircus}.
Mean measurements from a group of goats of unknown age and sex
\insertCite{clutton1990osteology}{zoolog}.}
\item{Uerpmann}{
\emph{Capra aegagrus}.
Measurements based on female and male \emph{Capra aegagrus},
Natural History Museum in London number: BMNH 651 M and L2
(Taurus Mountains in southern Turkey)
from \insertCite{uerpmann1994animal;textual}{zoolog}.}
}}
\item{\strong{Pig - \emph{Sus}}}{\describe{
\item{Albarella}{
\emph{Sus domesticus}.
Mean measurements from a group of Late Neolithic pigs from
Durrington Walls, England
\insertCite{albarella2005neolithic}{zoolog}.}
\item{Basel}{
\emph{Sus scrofa}.
Inv.nr. 1446 (male; 2-3 years old; life weight: 120 kg)
from \insertCite{stopp2018Basel;textual}{zoolog}.}
\item{Hongo}{
\emph{Sus scrofa}.
Averaged left and right measurements of a female wild board from near
Elaziğ, Turkey. Museum of Comparative Zoology, Harvard University,
specimen #51621 \insertCite{hongo2000faunal}{zoolog}.}
\item{Payne}{
\emph{Sus scrofa}.
Measurements based on a sample of modern wild boar, \emph{Sus scrofa libycus},
(male and female; Kizilcahamam, Turkey) from
\insertCite{payne1988components;textual}{zoolog}, Appendix 2.}
}}
\item{\strong{Red deer - \emph{Cervus}}}{\describe{
\item{Basel}{
\emph{Cervus elaphus}.
Inv.nr. 2271 (male; adult) from
\insertCite{stopp2018Basel;textual}{zoolog}.}
}}
\item{\strong{Fallow deer - \emph{Dama}}}{\describe{
\item{Haifa}{
\emph{Dama mesopotamica}.
Adult female modern specimen from Israel (id #1047), curated in
Archaeozoology Laboratory at the University of Haifa
\insertCite{Harding2021}{zoolog}.}
}}
\item{\strong{Gazelle - \emph{Gazella}}}{\describe{
\item{Haifa}{
\emph{Gazella gazella}.
Adult female modern specimen from Israel (id #1037), curated in
Archaeozoology Laboratory at the University of Haifa
\insertCite{Harding2021}{zoolog}.}
}}
\item{\strong{Equid - \emph{Equus}}}{\describe{
\item{Haifa}{
\emph{Equus asinus}.
Adult male modern specimen from Israel (id #1076), curated in
Archaeozoology Laboratory at the University of Haifa
\insertCite{Harding2021}{zoolog}.}
\item{Johnstone}{
\emph{Equus caballus}.
3 years old Icelandic mare (all bones fused, female) died in 1961,
\insertCite{johnstone2004biometric}{zoolog}. Skeleton
held at the Zoologische Staatssammlung Munich in Germany.
Specimen ID 1961/29.}
}}
\item{\strong{European rabbit - \emph{Oryctolagus}}}{\describe{
\item{Nottingham}{
\emph{Oryctolagus cuniculus}.
Adult male European rabbit from Audley End, Essex, UK, curated in the
reference collection at University of Nottingham Arch department (ID RS139)
\insertCite{Ameen2021}{zoolog}.}
}}
}

The \pkg{zoolog} variable \code{referencesDatabase} collects all these
references. It is structured as a named list of named lists, following the
hierarchy described above:\if{html}{\out{<div class="r">}}\preformatted{str(referencesDatabase, max.level = 2)
}\if{html}{\out{</div>}}\preformatted{## List of 14
##  $ Bos taurus           :List of 3
##   ..$ Nieto    :'data.frame':	68 obs. of  4 variables:
##   ..$ Basel    :'data.frame':	50 obs. of  4 variables:
##   ..$ Johnstone:'data.frame':	24 obs. of  4 variables:
##  $ Bos primigenius      :List of 1
##   ..$ Degerbol:'data.frame':	50 obs. of  4 variables:
##  $ Ovis aries           :List of 2
##   ..$ Davis  :'data.frame':	23 obs. of  4 variables:
##   ..$ Clutton:'data.frame':	71 obs. of  4 variables:
##  $ Ovis orientalis      :List of 2
##   ..$ Basel   :'data.frame':	36 obs. of  4 variables:
##   ..$ Uerpmann:'data.frame':	50 obs. of  4 variables:
##  $ Capra hircus         :List of 2
##   ..$ Basel  :'data.frame':	35 obs. of  4 variables:
##   ..$ Clutton:'data.frame':	60 obs. of  4 variables:
##  $ Capra aegagrus       :List of 1
##   ..$ Uerpmann:'data.frame':	50 obs. of  4 variables:
##  $ Sus domesticus       :List of 1
##   ..$ Albarella:'data.frame':	42 obs. of  4 variables:
##  $ Sus scrofa           :List of 3
##   ..$ Basel:'data.frame':	41 obs. of  4 variables:
##   ..$ Hongo:'data.frame':	96 obs. of  4 variables:
##   ..$ Payne:'data.frame':	33 obs. of  4 variables:
##  $ Cervus elaphus       :List of 1
##   ..$ Basel:'data.frame':	14 obs. of  4 variables:
##  $ Dama mesopotamica    :List of 1
##   ..$ Haifa:'data.frame':	60 obs. of  4 variables:
##  $ Gazella gazella      :List of 1
##   ..$ Haifa:'data.frame':	63 obs. of  4 variables:
##  $ Equus asinus         :List of 1
##   ..$ Haifa:'data.frame':	48 obs. of  4 variables:
##  $ Equus caballus       :List of 1
##   ..$ Johnstone:'data.frame':	73 obs. of  4 variables:
##  $ Oryctolagus cuniculus:List of 1
##   ..$ Nottingham:'data.frame':	58 obs. of  4 variables:
}
}

\section{Reference Sets}{

The references' database is organized per taxon. However, in general the
zooarchaeological data to be analysed includes several taxa. Thus, the
reference dataframe should include one reference standard for each relevant
taxon.
The \pkg{zoolog} variable \code{referenceSets} defines four possible
references:\if{html}{\out{<div class="r">}}\preformatted{referenceSets
}\if{html}{\out{</div>}}\tabular{lllllllllllllll}{
    \tab Bos taurus \tab Bos primigenius \tab Ovis aries \tab Ovis orientalis \tab Capra hircus \tab Capra aegagrus \tab Sus domesticus \tab Sus scrofa \tab Cervus elaphus \tab Dama mesopotamica \tab Gazella gazella \tab Equus asinus \tab Equus caballus \tab Oryctolagus cuniculus \cr
   NietoDavisAlbarella \tab Nieto \tab  \tab Davis \tab  \tab  \tab  \tab Albarella \tab  \tab  \tab  \tab  \tab  \tab  \tab  \cr
   Basel \tab Basel \tab  \tab  \tab Basel \tab Basel \tab  \tab  \tab Basel \tab Basel \tab  \tab  \tab  \tab  \tab  \cr
   Combi \tab Nieto \tab  \tab Clutton \tab  \tab Clutton \tab  \tab  \tab Basel \tab Basel \tab Haifa \tab Haifa \tab Haifa \tab Johnstone \tab Nottingham \cr
   Groningen \tab  \tab Degerbol \tab  \tab Uerpmann \tab  \tab Uerpmann \tab  \tab Hongo \tab  \tab  \tab  \tab  \tab  \tab  \cr
}


Each row defines a reference set consisting of a reference source for
each taxon (column). The function
\code{\link{AssembleReference}} allows us to build the reference set
taking the selected taxon-specific references from the
\code{referencesDatabase}.

The \pkg{zoolog} variable \code{reference} is a named list including the
references defined by \code{referenceSets}:\if{html}{\out{<div class="r">}}\preformatted{str(reference)
}\if{html}{\out{</div>}}\preformatted{## List of 4
##  $ NietoDavisAlbarella:'data.frame':	133 obs. of  4 variables:
##   ..$ TAX     : Factor w/ 3 levels "bota","ovar",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..$ EL      : Factor w/ 27 levels "AS","CAL","FE",..: 4 4 4 4 4 4 4 4 4 11 ...
##   ..$ Measure : Factor w/ 24 levels "BFd","BFp","BT",..: 8 9 5 7 12 4 3 11 6 8 ...
##   ..$ Standard: num [1:133] 259 234 78.3 90.2 29 ...
##  $ Basel              :'data.frame':	176 obs. of  4 variables:
##   ..$ TAX     : Factor w/ 5 levels "BOTA","Ovis orientalis",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..$ EL      : Factor w/ 28 levels "Astragalus","Calcaneus",..: 14 14 14 14 5 5 5 13 13 13 ...
##   ..$ Measure : Factor w/ 26 levels "BFd","BFp","BG",..: 21 13 18 3 5 4 19 6 19 5 ...
##   ..$ Standard: num [1:176] 65.9 83 66.9 58.1 95.3 ...
##  $ Combi              :'data.frame':	556 obs. of  4 variables:
##   ..$ TAX     : Factor w/ 10 levels "bota","OVAR",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..$ EL      : Factor w/ 56 levels "AS","CAL","FE",..: 4 4 4 4 4 4 4 4 4 11 ...
##   ..$ Measure : Factor w/ 63 levels "BFd","BFp","BT",..: 8 9 5 7 12 4 3 11 6 8 ...
##   ..$ Standard: num [1:556] 259 234 78.3 90.2 29 ...
##  $ Groningen          :'data.frame':	246 obs. of  4 variables:
##   ..$ TAX     : Factor w/ 4 levels "Bos primigenius",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..$ EL      : Factor w/ 23 levels "Astragalus","Calcaneus",..: 13 13 13 5 5 5 5 5 12 12 ...
##   ..$ Measure : Factor w/ 45 levels "BFp","BG","BT",..: 14 12 2 8 9 4 3 13 8 5 ...
##   ..$ Standard: num [1:246] 69 70 60 359 309 97 89 46 320 100 ...
}

\code{reference$Combi} includes the most comprehensive reference for each
species so that more measurements can be considered. It is the default
reference for computing the \link[=LogRatios]{log ratios}.

If desired, the user can define their own combinations or can also use
their own references, which must be a dataframe with the format described
above.
}

\section{File Structure}{

\code{referencesDatabase}, \code{refereceSets}, and \code{reference} are exported variables
automatically loaded in memory. In addition, \pkg{zoolog} provides in the
\code{extdata} folder a set of semicolon separated files (csv), generating
them:
\describe{
\item{\code{referenceSets.csv}}{Defines \code{referenceSets}.}
\item{\code{referencesDatabase.csv}}{Defines the structure of
\code{referencesDatabase}.}
\item{...}{A csv file for each taxon-specific reference, as named in
\code{referencesDatabase.csv}.}
}\if{html}{\out{<div class="r">}}\preformatted{utils::read.csv2(system.file("extdata", "referencesDatabase.csv",
                             package = "zoolog"))
}\if{html}{\out{</div>}}\preformatted{##                              Genus                 Taxon     Source
## 1                   Cattle - *Bos*            Bos taurus      Nieto
## 2                   Cattle - *Bos*            Bos taurus      Basel
## 3                   Cattle - *Bos*            Bos taurus  Johnstone
## 4                   Cattle - *Bos*       Bos primigenius   Degerbol
## 5                   Sheep - *Ovis*            Ovis aries      Davis
## 6                   Sheep - *Ovis*            Ovis aries    Clutton
## 7                   Sheep - *Ovis*       Ovis orientalis      Basel
## 8                   Sheep - *Ovis*       Ovis orientalis   Uerpmann
## 9                   Goat - *Capra*          Capra hircus      Basel
## 10                  Goat - *Capra*          Capra hircus    Clutton
## 11                  Goat - *Capra*        Capra aegagrus   Uerpmann
## 12                     Pig - *Sus*        Sus domesticus  Albarella
## 13                     Pig - *Sus*            Sus scrofa      Basel
## 14                     Pig - *Sus*            Sus scrofa      Hongo
## 15                     Pig - *Sus*            Sus scrofa      Payne
## 16             Red deer - *Cervus*        Cervus elaphus      Basel
## 17            Fallow deer - *Dama*     Dama mesopotamica      Haifa
## 18             Gazelle - *Gazella*       Gazella gazella      Haifa
## 19                 Equid - *Equus*          Equus asinus      Haifa
## 20                 Equid - *Equus*        Equus caballus  Johnstone
## 21 European rabbit - *Oryctolagus* Oryctolagus cuniculus Nottingham
##                          Filename
## 1       referenceCattle_Nieto.csv
## 2       referenceCattle_Basel.csv
## 3   referenceCattle_Johnstone.csv
## 4    referenceCattle_Degerbol.csv
## 5        referenceSheep_Davis.csv
## 6      referenceSheep_Clutton.csv
## 7        referenceSheep_Basel.csv
## 8     referenceSheep_Uerpmann.csv
## 9         referenceGoat_Basel.csv
## 10      referenceGoat_Clutton.csv
## 11     referenceGoat_Uerpmann.csv
## 12     referencePig_Albarella.csv
## 13         referencePig_Basel.csv
## 14         referencePig_Hongo.csv
## 15         referencePig_Payne.csv
## 16     referenceRedDeer_Basel.csv
## 17        referenceDama_Haifa.csv
## 18     referenceGazelle_Haifa.csv
## 19       referenceEquid_Haifa.csv
## 20   referenceEquid_Johnstone.csv
## 21 referenceRabbit_Nottingham.csv
}
}

\section{Acknowledgement}{

We are grateful to Barbara Stopp and Sabine
Deschler-Erb for providing the Basel references for cattle, sheep, goat,
wild boar, and red deer
\insertCite{stopp2018Basel}{zoolog},
together with the permission to publish them as part of \pkg{zoolog}.

We thank also Francesca Slim and Dimitris Filioglou for providing the
references for aurochs, mouflon, wild goat, and wild boar
\insertCite{degerbol1970urus,uerpmann1994animal,hongo2000faunal}{zoolog}
in the Groningen set.

We thank Claudia Minniti for providing Johnstone's reference for cattle.
\insertCite{johnstone2002late}{zoolog}.

We are also grateful to Sierra Harding and Nimrod Marom for providing the
Haifa standard measurements for donkey, gazelle, and fallow deer
\insertCite{Harding2021}{zoolog}.

We thank Carly Ameen for providing references for horse
\insertCite{johnstone2004biometric}{zoolog} and rabbit
\insertCite{Ameen2021}{zoolog}.
}

\references{
\insertAllCited{}
}
\keyword{datasets}
