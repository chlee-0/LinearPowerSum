(* ::Package:: *)

(* test if the input is of the form a m + b with a,b integers *)
ambQ[poly_]:=Block[{val1,val2,a,b},
val1 = PolynomialQ[poly,m];
val2 = Exponent[poly,m]<=1;
If[val1&&val2,And@@(IntegerQ/@CoefficientList[poly,m]),False]
]

(* data structure for linPS, meaning linear power sum *)
linPS[{},{},m_]=0;
linPS[coef_List,{1},m_]:=First[coef];
linPS[coef_List,alpha_List,m_Integer]:=Dot[coef,alpha^m]//Factor;
linPS[coef_List,alpha_List,m_]:=Block[{pos},
pos=Union[Position[coef,0],Position[alpha,0]];
linPS[Delete[coef,pos],Delete[alpha,pos],m]
]/;Union[Position[coef,0],Position[alpha,0]]!={};
(* handle \[Alpha]^m appearing multiple times *)
linPS[coef_List,alpha_List,m_]:=Block[{set},
set[1]=GatherBy[Flatten[Transpose[{coef,alpha}],{1}],Last];
set[2]=SortBy[set[1],Last];
set[3]=Map[Transpose,set[2]];
linPS@@Simplify@(Append[Transpose[Map[{Total[#[[1]]]//Expand,Mean[#[[2]]]//Expand}&,set[3]]],m])
]/;DuplicateFreeQ[Simplify[alpha]]==False;

(* convert Subscript[c, m]\[Alpha]^(a m+b) into Subscript[c, m]\[Alpha]^b(\[Alpha]^a)^m*)
linPS[coef_List,alpha_List,poly_]:=Block[{aa,bb},
{bb,aa}=PadRight[CoefficientList[poly,m],2];
linPS@@Simplify@(Append[Transpose[Table[{coef[[j]]alpha[[j]]^bb,alpha[[j]]^aa},{j,1,Length[coef]}]],m])
]/;(ambQ[poly]&&UnsameQ[poly,m]);

(* scalar multiplication *)
linPS/:Times[a_,linPS[coef_List,alpha_List,m_]]:=linPS[a coef,alpha,m];

(* addition *)
linPS/:Plus[0,x_linPS]:=x;
linPS/:Plus[x_linPS,0]:=x;
linPS/:Plus[linPS[c1_List,alpha1_List,j_],linPS[c2_List,alpha2_List,k_]]:=linPS@@{Join[c1,c2],Join[alpha1,alpha2],m}/;(ambQ[j]&&ambQ[k]);

(* product *)
linPS/:Times[0,x_linPS]:=0;
linPS/:Times[x_linPS,0]:=0;
linPS/:Times[linPS[c1_List,alpha1_List,j_],linPS[c2_List,alpha2_List,k_]]:=Block[{prod},
prod=Flatten@Outer[Times,#1,#2]&;
linPS@@{prod[c1,c2],prod[alpha1,alpha2],m}
]/;(ambQ[j]&&ambQ[k]);

(* exponentiation *)
linPS/:Power[linPS[coef_List,alpha_List,m_],0]:=linPS[{1},{1},m];
linPS/:Power[linPS[coef_List,alpha_List,m_],a_Integer]:=Times[Power[linPS[coef,alpha,m],a-1],linPS[coef,alpha,m]]/;a>=1;
