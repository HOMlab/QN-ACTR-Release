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

set terminal postscript enhanced color solid lw 4 "Times-Roman" 28
set output "const.ps"

set ytics 0.2

set xrange[-0.1:6.1]
set yrange [-0.2:1.2]

set samples 6

set ytics .5
set xtics 1

set key top

plot [0:6]\
    "const1.txt" title "k = 1" with linespoints pt 7, \
    "const2.txt" title "k = 4" with linespoints pt 7
