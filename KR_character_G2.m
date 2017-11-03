(* ::Package:: *)

SetDirectory[DirectoryName[$InputFileName]];
Get["linearpowersum.m"];


(* coefficients c, t *)
Lambda1 = {1,1/y[1],y[1],y[1]/y[2]^3,y[1]^2/y[2]^3,y[2]^3/y[1]^2,y[2]^3/y[1]};
Lambda2={y[1]/y[2]^2,1/y[2],y[1]/y[2],y[2],y[2]/y[1],y[2]^2/y[1]};
Lambda2prime=Lambda1;
ccoef={{1,-((2 y[1]^3 y[2]^4 (y[1]+y[2]+y[2]^2) (y[2]^2+y[1] (1+y[2])))/((-1+y[1])^2 (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3)^2))},{1/y[1],-((y[1] y[2]^4)/((-1+y[1])^2 (y[1]-y[2]) (-1+y[2]) (y[1]-y[2]^3) (y[1]^2-y[2]^3)))},{y[1],-((y[1]^5 y[2]^4)/((-1+y[1])^2 (y[1]-y[2]) (-1+y[2]) (y[1]-y[2]^3) (y[1]^2-y[2]^3)))},{y[1]/y[2]^3,y[1]^5/((-1+y[1]) (-1+y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3))},{y[1]^2/y[2]^3,y[1]^8/((-1+y[1]) (y[1]-y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3) (y[1]^2-y[2]^3)^2)},{y[2]^3/y[1]^2,y[2]^12/((-1+y[1]) (y[1]-y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3) (y[1]^2-y[2]^3)^2)},{y[2]^3/y[1],(y[1] y[2]^12)/((-1+y[1]) (-1+y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3))},{y[1]/y[2]^2,-(y[1]^12/((y[1]-y[2]) (-1+y[2]) (y[1]-y[2]^2)^2 (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^3))},{1/y[2],(y[1]^4 y[2])/((-1+y[1])^3 (y[1]-y[2]) (-1+y[2])^2 (y[1]-y[2]^2) (y[1]-y[2]^3)^3)},{y[1]/y[2],(y[1]^12 y[2])/((-1+y[1])^3 (y[1]-y[2])^2 (-1+y[2]) (y[1]-y[2]^2) (y[1]^2-y[2]^3)^3)},{y[2],(y[1]^4 y[2]^13)/((-1+y[1])^3 (y[1]-y[2]) (-1+y[2])^2 (y[1]-y[2]^2) (y[1]-y[2]^3)^3)},{y[2]/y[1],y[2]^13/((-1+y[1])^3 (y[1]-y[2])^2 (-1+y[2]) (y[1]-y[2]^2) (y[1]^2-y[2]^3)^3)},{y[2]^2/y[1],-(y[2]^24/((y[1]-y[2]) (-1+y[2]) (y[1]-y[2]^2)^2 (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^3))}};
tcoef[0]={{1,-((2 y[1]^4 y[2]^6 (y[1]^4-y[1]^3 y[2]-y[1]^4 y[2]+y[1]^2 y[2]^2+y[1]^3 y[2]^2+y[1]^4 y[2]^2+y[1]^2 y[2]^3+y[1]^3 y[2]^3-y[1] y[2]^4-6 y[1]^2 y[2]^4-y[1]^3 y[2]^4+y[1] y[2]^5+y[1]^2 y[2]^5+y[2]^6+y[1] y[2]^6+y[1]^2 y[2]^6-y[2]^7-y[1] y[2]^7+y[2]^8))/((-1+y[1])^2 (y[1]-y[2])^2 (-1+y[2])^2 (y[1]-y[2]^2)^2 (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3)^2))},{1/y[1],-((y[1]^2 y[2]^6 (y[1]^4+4 y[1]^3 y[2]^2+2 y[1]^2 y[2]^3+2 y[1]^3 y[2]^3+9 y[1]^2 y[2]^4+2 y[1] y[2]^5+2 y[1]^2 y[2]^5+4 y[1] y[2]^6+y[2]^8))/((-1+y[1])^2 (y[1]-y[2]) (-1+y[2]) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^3))},{y[1],-((y[1]^6 y[2]^6 (y[1]^4+4 y[1]^3 y[2]^2+2 y[1]^2 y[2]^3+2 y[1]^3 y[2]^3+9 y[1]^2 y[2]^4+2 y[1] y[2]^5+2 y[1]^2 y[2]^5+4 y[1] y[2]^6+y[2]^8))/((-1+y[1])^2 (y[1]-y[2]) (-1+y[2]) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^3))},{y[1]/y[2]^3,(y[1]^6 y[2] (2 y[1]^3+y[1]^4+2 y[1]^2 y[2]+4 y[1]^3 y[2]+9 y[1]^2 y[2]^2+4 y[1] y[2]^3+2 y[1]^2 y[2]^3+y[2]^4+2 y[1] y[2]^4))/((-1+y[1])^3 (-1+y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3)^3)},{y[1]^2/y[2]^3,(y[1]^9 y[2] (y[1]+2 y[1]^2+4 y[1] y[2]+2 y[1]^2 y[2]+9 y[1] y[2]^2+2 y[2]^3+4 y[1] y[2]^3+2 y[2]^4+y[1] y[2]^4))/((-1+y[1])^3 (y[1]-y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^2)},{y[2]^3/y[1]^2,(y[1] y[2]^13 (y[1]+2 y[1]^2+4 y[1] y[2]+2 y[1]^2 y[2]+9 y[1] y[2]^2+2 y[2]^3+4 y[1] y[2]^3+2 y[2]^4+y[1] y[2]^4))/((-1+y[1])^3 (y[1]-y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^2)},{y[2]^3/y[1],(y[1]^2 y[2]^13 (2 y[1]^3+y[1]^4+2 y[1]^2 y[2]+4 y[1]^3 y[2]+9 y[1]^2 y[2]^2+4 y[1] y[2]^3+2 y[1]^2 y[2]^3+y[2]^4+2 y[1] y[2]^4))/((-1+y[1])^3 (-1+y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3)^3)}};
tcoef[1]={{1,-((2 y[1]^4 y[2]^7 (y[1]^3+y[1]^4-y[1]^3 y[2]-y[1]^2 y[2]^2-y[1]^3 y[2]^2+y[1] y[2]^3+y[1]^3 y[2]^3-y[1] y[2]^4-y[1]^2 y[2]^4-y[1] y[2]^5+y[2]^6+y[1] y[2]^6))/((-1+y[1])^2 (y[1]-y[2])^2 (-1+y[2])^2 (y[1]-y[2]^2)^2 (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3)^2))},{1/y[1],-((y[1]^2 y[2]^7 (2 y[1]^3+3 y[1]^3 y[2]+6 y[1]^2 y[2]^2+y[1] y[2]^3+4 y[1]^2 y[2]^3+6 y[1] y[2]^4+3 y[1] y[2]^5+2 y[2]^6))/((-1+y[1])^2 (y[1]-y[2]) (-1+y[2]) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^3))},{y[1],-((y[1]^7 y[2]^7 (2 y[1]^3+3 y[1]^2 y[2]+6 y[1]^2 y[2]^2+4 y[1] y[2]^3+y[1]^2 y[2]^3+6 y[1] y[2]^4+3 y[2]^5+2 y[2]^6))/((-1+y[1])^2 (y[1]-y[2]) (-1+y[2]) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^3))},{y[1]/y[2]^3,(y[1]^7 (y[1]^2+2 y[1]^3+6 y[1]^2 y[2]+6 y[1] y[2]^2+3 y[1]^2 y[2]^2+2 y[2]^3+4 y[1] y[2]^3+3 y[2]^4))/((-1+y[1])^3 (-1+y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3)^3)},{y[1]^2/y[2]^3,(y[1]^10 (2 y[1]+y[1]^2+6 y[1] y[2]+3 y[2]^2+6 y[1] y[2]^2+4 y[2]^3+2 y[1] y[2]^3+3 y[2]^4))/((-1+y[1])^3 (y[1]-y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^2)},{y[2]^3/y[1]^2,(y[2]^14 (3 y[1]^2+2 y[1] y[2]+4 y[1]^2 y[2]+6 y[1] y[2]^2+3 y[1]^2 y[2]^2+6 y[1] y[2]^3+y[2]^4+2 y[1] y[2]^4))/((-1+y[1])^3 (y[1]-y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^2)},{y[2]^3/y[1],(y[1]^2 y[2]^14 (3 y[1]^3+4 y[1]^2 y[2]+2 y[1]^3 y[2]+3 y[1] y[2]^2+6 y[1]^2 y[2]^2+6 y[1] y[2]^3+2 y[2]^4+y[1] y[2]^4))/((-1+y[1])^3 (-1+y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3)^3)}};
tcoef[2]={{1,-((2 y[1]^4 y[2]^7 (y[1]^3+y[1]^4-y[1]^3 y[2]-y[1]^2 y[2]^2-y[1]^3 y[2]^2+y[1] y[2]^3+y[1]^3 y[2]^3-y[1] y[2]^4-y[1]^2 y[2]^4-y[1] y[2]^5+y[2]^6+y[1] y[2]^6))/((-1+y[1])^2 (y[1]-y[2])^2 (-1+y[2])^2 (y[1]-y[2]^2)^2 (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3)^2))},{1/y[1],-((y[1]^2 y[2]^7 (2 y[1]^3+3 y[1]^2 y[2]+6 y[1]^2 y[2]^2+4 y[1] y[2]^3+y[1]^2 y[2]^3+6 y[1] y[2]^4+3 y[2]^5+2 y[2]^6))/((-1+y[1])^2 (y[1]-y[2]) (-1+y[2]) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^3))},{y[1],-((y[1]^7 y[2]^7 (2 y[1]^3+3 y[1]^3 y[2]+6 y[1]^2 y[2]^2+y[1] y[2]^3+4 y[1]^2 y[2]^3+6 y[1] y[2]^4+3 y[1] y[2]^5+2 y[2]^6))/((-1+y[1])^2 (y[1]-y[2]) (-1+y[2]) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^3))},{y[1]/y[2]^3,(y[1]^7 (3 y[1]^3+4 y[1]^2 y[2]+2 y[1]^3 y[2]+3 y[1] y[2]^2+6 y[1]^2 y[2]^2+6 y[1] y[2]^3+2 y[2]^4+y[1] y[2]^4))/((-1+y[1])^3 (-1+y[2]) y[2] (y[1]-y[2]^2) (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3)^3)},{y[1]^2/y[2]^3,(y[1]^10 (3 y[1]^2+2 y[1] y[2]+4 y[1]^2 y[2]+6 y[1] y[2]^2+3 y[1]^2 y[2]^2+6 y[1] y[2]^3+y[2]^4+2 y[1] y[2]^4))/((-1+y[1])^3 (y[1]-y[2]) y[2] (y[1]-y[2]^2) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^2)},{y[2]^3/y[1]^2,(y[2]^15 (2 y[1]+y[1]^2+6 y[1] y[2]+3 y[2]^2+6 y[1] y[2]^2+4 y[2]^3+2 y[1] y[2]^3+3 y[2]^4))/((-1+y[1])^3 (y[1]-y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^3 (y[1]^2-y[2]^3)^2)},{y[2]^3/y[1],(y[1]^2 y[2]^15 (y[1]^2+2 y[1]^3+6 y[1]^2 y[2]+6 y[1] y[2]^2+3 y[1]^2 y[2]^2+2 y[2]^3+4 y[1] y[2]^3+3 y[2]^4))/((-1+y[1])^3 (-1+y[2]) (y[1]-y[2]^2) (y[1]-y[2]^3)^2 (y[1]^2-y[2]^3)^3)}};
ckeys=(Association@Map[Rule@@#&,ccoef]);
tkeys[j_]:=tkeys[j]=(Association@Map[Rule@@#&,tcoef[j]]);
c[la_]:=ckeys[la]
t[la_,j_]:=tkeys[j][la]


(* G2 datum *)
cartan[G,2]={{2,-1},{-3,2}};
inversecartan[G,2]={{2,1},{3,2}};
posrts[G,2]={rt[1,0],rt[0,1],rt[1,1],rt[1,3],rt[1,2],rt[2,3]};
weylgroup[G,2]={{},{1},{2},{1,2},{2,1},{1,2,1},{2,1,2},{1,2,1,2},{2,1,2,1},{1,2,1,2,1},{2,1,2,1,2},{2,1,2,1,2,1}};
(* weight lattice and root lattice *)
wt/:Plus[x_wt,y_wt]:=wt@@Simplify@(List@@x+List@@y)
wt/:Times[a_,x_wt]:=wt@@Simplify@(a*(List@@x))
rt/:Plus[x_rt,y_rt]:=rt@@Simplify@(List@@x+List@@y)
rt/:Times[a_,x_rt]:=rt@@Simplify@(a*(List@@x))
(* coordinate change between weight lattice and root lattice *)
convert[ty_,rk_][vector_rt]:=Block[{nvector},
nvector=List@@vector;
wt@@(cartan[ty, rk].nvector)
]
convert[ty_,rk_][vector_wt]:=Block[{nvector},
nvector=List@@vector;
rt@@(inversecartan[ty,rk].nvector)
]
(* Weyl group action on weight lattice *)
WeylR[ty_,rk_][{}][vector_wt]:=vector
WeylR[ty_,rk_][{a_Integer}][vector_wt]:=WeylR[ty,rk][{a}][vector]=Block[{func,s,nvector},
s[i_][v_]:=v-v[[i]]Transpose[cartan[ty,rk]][[i]];
nvector=List@@vector;
wt@@Simplify@(s[a][nvector])
]
WeylR[ty_,rk_][indexset_List][vector_wt]:=WeylR[ty,rk][indexset][vector]=WeylR[ty,rk][Delete[indexset,-1]][WeylR[ty,rk][{Last[indexset]}][vector]]
(* Weyl character formula for irreducible highest weight representations *)
char[ty_,rk_][la_wt]:=char[ty,rk][la]=Block[{rho,posR,denom,exp},
rho=wt@@ConstantArray[1,rk];
posR=Map[convert[ty,rk],posrts[ty,rk]];
exp[lam_wt]:=Inner[Power,Array[y,Length[lam]],List@@lam,Times];
denom=exp[rho]Product[1-exp[-\[Alpha]//Expand],{\[Alpha],posR}];
(Total@Map[(-1)^Length[#] exp[WeylR[ty,rk][#][rho+la]]&,weylgroup[ty,rk]]/denom)//Factor//Expand
]


(* KR character  : Q[a,m] for m=0,1 *)
Q[1,0]=1;
Q[1,1]=char[G,2][wt[1,0]]+char[G,2][wt[0,0]];
Q[2,0]=1;
Q[2,1]=char[G,2][wt[0,1]];


(* definition of Subscript[R, m]^(a) *)
RR[1,0][m_]:=RR[1,0][m]= Simplify[linPS@@{Lambda1/.ckeys,Lambda1,m}]
RR[2,j_][m_]:=RR[2,j][m]=Simplify[linPS@@{Lambda2/.ckeys,Lambda2,3m+j}+linPS@@{Lambda2prime/.tkeys[j],Lambda2prime,m}]
R[1, poly_]:=RR[1,0][poly]
R[2, poly_]:=Block[{ta=3,res},
res =Mod[Coefficient[poly,m,0],ta];
RR[2,res][(poly-res)/ta]
]/;ambQ[poly]


Print["checking the Q-system for R[1,m] : print 0 if true"]
Block[{p1,p2},
p1=R[1,m]^2-R[1,m-1]*R[1,m+1];
p2=R[2,3m];
p1-p2
]//Print


Print["checking the Q-system for R[2,3m] : print 0 if true"]
Block[{p1,p2},
p1=R[2,3m]^2-R[2,3m-1]*R[2,3m+1];
p2=R[1,m]^3;
p1-p2
]//Print


Print["checking the Q-system for R[2,3m+1] : print 0 if true"]
Block[{p1,p2},
p1=R[2,3m+1]^2-R[2,3m]*R[2,3m+2];
p2=R[1,m]^2*R[1,m+1];
p1-p2
]//Print


Print["checking the Q-system for R[2,3m+2] : print 0 if true"]
Block[{p1,p2},
p1=R[2,3m+2]^2-R[2,3m+1]*R[2,3m+3];
p2=R[1,m]*R[1,m+1]^2;
p1-p2
]//Print


Print["check initial conditions"];
Print["R[1,0]-Q[1,0] :"];
Print[R[1,0]-Q[1,0]//Simplify];
Print["R[1,1]-Q[1,1] :"];
Print[R[1,1]-Q[1,1]//Simplify];
Print["R[2,0]-Q[2,0] :"];
Print[R[2,0]-Q[2,0]//Simplify];
Print["R[2,1]-Q[2,1] :"];
Print[R[2,1]-Q[2,1]//Simplify];


(* decomposition of KR modules W[2,m] in type G2 *)
krdecomposeG2[2,m_]:=Block[{n=3,coef,x},
coef[{j0_,j1_,j2_}]:=(1+j1) (1+Min[Floor[1/3 j0],Floor[1/3 (-j0+j2)]+Floor[1/3 j0]]);
Select[Table[{coef[sol],sol[[2]]wt[1,0]+sol[[3]]wt[0,1]},{sol,FrobeniusSolve[{1,3,1},m]}],#[[1]]!=0&]
]
(* character of W[2,m] in terms of the lattice summation formula *)
P[2,m_]:=Block[{},
Expand[Total[(#[[1]]*char[G,2][#[[2]]])&/@krdecomposeG2[2,m]]]
]


Print["computing : {m, P[2,m]-R[2,m]}"];
Do[Print[{n,R[2,n]-P[2,n]//Simplify}],{n,0,56}]
