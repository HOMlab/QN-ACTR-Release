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

pdfnorm(x,m,s) = (1 / (s*sqrt(2*pi))) * exp(-((x-m)**2)/(2*(s**2)))

set samples 1001
set terminal postscript enhanced color solid lw 4 "Times-Roman" 28
set output "norm.ps"

set ytics 0.2
#set yrange [0:1]


f(x,y,z) = pdfnorm(x,y,z)
set key top

plot [-6:6] \
    f(x, -2.0,1.0) title "{/Symbol m} = -2.0 {/Symbol s} = 1.0", \
    f(x, 0.0,1.0) title "{/Symbol m} = 0.0 {/Symbol s} = 1.0", \
    f(x, 0.0,2.0) title "{/Symbol m} = 0.0 {/Symbol s} = 2.0", \
    f(x, 0.0,0.5) title "{/Symbol m} = 0.0 {/Symbol s} = 0.5", \
    f(x, 2.0,1.0) title "{/Symbol m} = 2.0 {/Symbol s} = 1.0"