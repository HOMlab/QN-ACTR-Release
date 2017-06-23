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

pdfhexp(x,p,l1,l2) = p * l1*exp(-l1*x) + (1-p) * l2 * exp(-l2 * x)

set samples 1001
set terminal postscript enhanced color solid lw 4 "Times-Roman" 28
set output "hyperexp.ps"

set ytics 0.2

set yrange [0:2]

f(x,y,z,w) = pdfhexp(x,y,z,w)
set key top

plot [0:4] \
    f(x, 0.5, 0.5, 4.0) title "p = 0.5 {/Symbol l}_1 = 0.5 {/Symbol l}_2 = 4.0", \
    f(x, 0.7, 1.0,1.0) title "p = 0.7 {/Symbol l}_1 = 1.0 {/Symbol l}_2 = 1.0", \
    f(x, 0.7, 1.0,6.0) title "p = 0.7 {/Symbol l}_1 = 1.0 {/Symbol l}_2 = 6.0", \
    f(x, 0.7,0.2,6.0) title "p = 0.7 {/Symbol l}_1 = 0.2 {/Symbol l}_2 = 6.0"
