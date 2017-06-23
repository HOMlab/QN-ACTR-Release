Java Modelling Tools 
====================

Introduction
------------
The Java Modelling Tools (JMT) is a free open source suite for performance evaluation, 
capacity planning and modelling of computer and communication systems. The suite implements 
numerous state-of-the-art algorithms for the exact, asymptotic and simulative analysis of 
queueing network models, either with or without product-form solution. Models can be described 
either through wizard dialogs or with a graphical user-friendly interface. The suite includes 
also a workload analysis tool based on clustering techniques. The suite incorporates an XML 
data layer that enables full reusability of the computational engines.
The JMT suite is composed of six tools that support different analyses frequently used in 
capacity planning studies. The main features of each tool follows.
 
JSIMwiz: a discrete-event simulator for the analysis of queueing network models. An intuitive 
sequence of wizard windows helps specifying network properties. The simulation engine supports 
several probability distributions for characterizing service and inter-arrival times. 
Load-dependent strategies using arbitrary functions of the current queue-length can be 
specified. JSIM supports state-independent routing strategies, e.g., Markovian or round robin, 
as well as state-dependent strategies, e.g., routing to the server with minimum utilization, 
or with the shortest response time, or with minimum queue-length. The simulation engine 
supports several extended features not allowed in product-form models, namely, finite capacity 
regions (i.e., blocking), fork-join servers, and priority classes. The analysis of simulation 
results employs on-line transient detection techniques based on spectral analysis. 
What-if analyses, where a sequence of simulations is run for different values of parameters, 
are also possible.
 
JSIMgraph: a graphical user-friendly interface for the simulator engine used by JSIMwiz. It 
integrates the same functionalities of JSIMwiz with an intuitive graphical workspace. This 
allows an easy description of network structure, as well as a simplified definition of 
the execution features like blocking regions. Network topologies can be exported in vectorial 
or raster image formats.
 
JMVA: meant for the exact analysis of single or multiclass product-form queueing 
network models, either processing open, closed or mixed workloads. The classic MVA solution 
algorithm is used. Network structure is specified by textual wizards, with conversion 
functions from probabilities to average visit ratios (and viceversa). What-if analyses are 
allowed.
 
JMCH: it applies a simulation technique to solve a single station model, with finite (M/M/1/k)
or infinite queue (M/M/1), and shows the underlying Markov Chain. It is also possible to 
dynamically change the arrival rate and service time of the system.
 
JABA: a tool for the identification of bottlenecks in closed product-form networks using 
efficient convex hull algorithms. The tool supports models with up to three job classes. 
It is possible to identify potential bottlenecks corresponding to the different mixes of 
customer classes. Models with thousands of queues can be analyzed efficiently. The saturation 
sectors, i.e., the mixes of customer classes that saturate more than one resource 
simultaneously, are identified.
 
JWAT: supports the workload characterization phase, with emphasis on Web log data. 
Some standard formats for input file are provided (e.g., Apache HTTP log files), and 
customized formats may also be specified. The imported data can initially be analyzed using 
descriptive statistical techniques (e.g, means, correlations, pdf histograms, boxplots, 
scatterplots), either for univariate or multivariate data. Algorithms for data scaling, 
sample extraction, outlier filtering, k-means and fuzzy k-means clustering for identifying 
similarities in the input data are provided. These techniques allow to determine cluster 
centroids, and then estimate mean workload and service demands to be used for model 
parametrization. The tool includes also an interface to the similarity clustering tool CLUTO.


Compiling instructions
----------------------
- To compile Java Modelling Tools you need Java J2SE SDK 1.5 (or later revision) that can be found at 
   http://java.com/ and Apache ANT that can be found at http://ant.apache.org/ .

- To create an installer you need IzPack that can be downloaded at  http://www.izforge.com/izpack/ and
   configure the appropriate path in "build.properties" file.

- Simply open "build.xml" with ANT and select desidered target to build JMT from sources


License
-------
Copyright (C) 2006, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano 

This program is free software; you can redistribute it and/or modify it under the terms 
of the GNU General Public License as published by the Free Software Foundation; either 
version 2 of the License, or (at your option) any later version. 
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
See the GNU General Public License for more details. 
You should have received a copy of the GNU General Public License along with this program;
if not, write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, 
MA 02110-1301 USA

Java Modelling Tools includes the following third-party software that can be freely 
distributed according to the licenses contained in the license folder:
- FreeHEP VectorGraphics package, released under GNU LGPL License.
- Apache Jakarta-ORO, released under Apache Software License.
- JFEP, released under the Apache License, Version 2.0
- SUN JavaHelp System, released under Sun Microsystems, Inc. Binary Code License Agreement.
- JGoodies Looks, released under BSD open source License.
- JGraph, released under GNU LGPL License.
- Apache Log4j, released under Apache Software License.
- Ptolemy II Ptplot, released under BSD open source License.
- Apache Xerces Java Parser, released under Apache Software License.

