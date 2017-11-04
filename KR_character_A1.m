(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];
Get["linearpowersum.m"];


Q[m_]:=linPS[{c,d},{y[1],y[1]^(-1)},m]


Q[m]//Print;
(*
linPS[{c,d},{y[1],1/y[1]},m]
*)
Q[m]^2//Print;
(*
linPS[{c^2,2 c d,d^2},{y[1]^2,1,1/y[1]^2},m]
*)
Q[m-1]//Print;
(*
linPS[{c/y[1],d y[1]},{y[1],1/y[1]},m]
*)
Q[m+1]//Print;
(*
linPS[{c y[1],d/y[1]},{y[1],1/y[1]},m]
*)
Q[m-1]*Q[m+1]//Print;
(*
linPS[{c^2,d^2,(c d (1+y[1]^4))/y[1]^2},{y[1]^2,1/y[1]^2,1},m]
*)
Q[m]^2-Q[m-1]*Q[m+1]//Print;
(*
-((c d (-1 + y[1]^2)^2)/y[1]^2)
*)
