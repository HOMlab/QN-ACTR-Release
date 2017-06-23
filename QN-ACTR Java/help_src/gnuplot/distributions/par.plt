# Copyright (C) 2007, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
# Author: Bertoli Marco
#

pdfpar(x,a,k) = a * (k ** a) * x ** (-a-1)

set samples 1001
set terminal postscript enhanced color solid lw 4 "Times-Roman" 28
set output "par.ps"

set ytics 0.2
set yrange [0:2]


f(x,y,z) = pdfpar(x,y,z)
set key top

plot [0:5] \
    f(x, 3.0,0.5) title "{/Symbol a} = 3.0 k = 0.5", \
    f(x, 3.0,1.0) title "{/Symbol a} = 3.0 k = 1.0", \
    f(x, 2.0,1.0) title "{/Symbol a} = 2.0 k = 1.0", \
    f(x, 2.0,3.0) title "{/Symbol a} = 2.0 k = 3.0", \
    f(x, 1.0,4.0) title "{/Symbol a} = 1.0 k = 4.0"