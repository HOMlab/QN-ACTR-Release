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

pdfuniform(x,min,max) = (x >= min && x <= max) ? 1 / (max - min) : 0

set samples 1001
set terminal postscript enhanced color solid lw 4 "Times-Roman" 28
set output "uniform.ps"

set ytics 0.1
set xtics 1

set yrange [-0.005:1]

f(x,y,z) = pdfuniform(x,y,z)
set key top

plot [0:8] \
    f(x, 1.0, 3.0) title "min = 1, max = 3", \
    f(x, 2.0, 7.0) title "min = 2, max = 7"    
