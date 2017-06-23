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

pdfstudentt(x,n) = gamma((n+1)/2)/gamma(n/2) * 1/sqrt(n*pi) * (1 + x**2 / n)**(-(n+1)/2)

set samples 1001
set terminal postscript enhanced color solid lw 4 "Times-Roman" 28
set output "studentt.ps"

set ytics 0.1

set yrange [0:0.5]

f(x,y) = pdfstudentt(x,y)
set key top

plot [-5:5] \
    f(x, 1.0) title "{/Symbol n} =  1", \
    f(x, 2.0) title "{/Symbol n} =  2", \
    f(x, 5.0) title "{/Symbol n} =  5", \
    f(x, 30.0) title "{/Symbol n} = 30"
