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

pdfgamma(x,a,l) = (x**(a-1))/ (gamma(a)*(l**a)) * exp(-x/l)

set samples 1001
set terminal postscript enhanced color solid lw 4 "Times-Roman" 28
set output "gamma.ps"

set ytics 0.1

set yrange [0:1]

f(x,y,z) = pdfgamma(x,y,z)
set key top

plot [0:12] \
    f(x, 0.5, 5.0) title "{/Symbol a} = 0.5, {/Symbol l} = 5.0", \
    f(x, 1.0, 5.0) title "{/Symbol a} = 1.0, {/Symbol l} = 5.0", \
    f(x, 1.0, 1.5) title "{/Symbol a} = 1.0, {/Symbol l} = 1.5", \
    f(x, 2.0, 1.5) title "{/Symbol a} = 2.0, {/Symbol l} = 1.5", \
    f(x, 2.0, 0.5) title "{/Symbol a} = 2.0, {/Symbol l} = 0.5"
    
