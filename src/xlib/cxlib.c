//-----------------------------------------------------------------------------
//   Copyright 2012 Julian Schutsch
//
//   This file is part of ParallelSim
//
//   ParallelSim is free software: you can redistribute it and/or modify
//   it under the terms of the GNU Affero General Public License as published
//   by the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   ParallelSim is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU Affero General Public License for more details.
//
//   You should have received a copy of the GNU Affero General Public License
//   along with ParallelSim.  If not, see <http://www.gnu.org/licenses/>.
//-----------------------------------------------------------------------------

// Revision History
//   24.Mar 2012 Julian Schutsch
//     - Original version

#include "X11/Xlib.h"

// Macro wrappers which are difficult to reproduce in Ada
// and therefore are provided as functions instead
int _DefaultScreen(Display * display)
{
  return DefaultScreen(display);
}

Window _RootWindow(Display * display, int screen)
{
  return RootWindow(display,screen);
}

typedef XIMStyles* XIMStyles_Pnt;
char * XGetIMValues_1(XIM xim,XIMStyles_Pnt * im_supported_styles)
{
  return XGetIMValues(xim,XNQueryInputStyle,im_supported_styles,NULL);
}
// This is a varargs wrapper which tries to cover the most common use case
// for XCreateIC
XIC _XCreateIC_1(XIM xim,Window window,int inputstyle)
{
  return XCreateIC
    (xim,
    XNInputStyle,inputstyle,
    XNClientWindow,window,
    XNFocusWindow,window,
    NULL);
}

void EnableDebug(void)
{
  _Xdebug = 1;
}
