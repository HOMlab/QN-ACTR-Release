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

pdfexp(x,r) = r * exp(-r*x)

set samples 1001
set terminal postscript enhanced color solid lw 4 "Times-Roman" 28
set output "exp.ps"

set ytics 0.2

f(x,y) = pdfexp(x,y)
set key top

plot [0:6] \
    f(x, 0.5) title "{/Symbol l} = 0.5", \
    f(x, 1.0) title "{/Symbol l} = 1.0", \
    f(x, 2.0) title "{/Symbol l} = 1.5"
