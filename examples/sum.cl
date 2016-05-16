/* Copyright (C) 2011 X. Andrade
**
** FortranCL is free software: you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** FortranCL is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program.  If not, see <http://www.gnu.org/licenses/>.
**
** $Id$
**/

__kernel void sum(const int size, const __global float * vec1, __global float * vec2){
  int ii = get_global_id(0);

  if(ii < size) vec2[ii] += vec1[ii];

}
