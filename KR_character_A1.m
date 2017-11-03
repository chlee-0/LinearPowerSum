(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];
Get["linearpowersum.m"];


Q[m_]:=linPS[{c,d},{y[1],y[1]^(-1)},m]


Q[m]//Print;
Q[m]^2//Print;
Q[m-1]//Print;
Q[m+1]//Print;
Q[m-1]*Q[m+1]//Print;
Q[m]^2-Q[m-1]*Q[m+1]/.{c->y[1]/(-(1/y[1])+y[1]),d->-(y[1]^-1/( -(1/y[1])+y[1]))}//Simplify//Print;
