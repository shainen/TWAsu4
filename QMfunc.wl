(* ::Package:: *)

QMSpinsFromHam=Function[{length,hamQM,times,initspin,fisp},
Block[{
ivec=Flatten[KroneckerProduct@@(RotateLeft[IntegerDigits[#,2,2]]&/@initspin)],
diag=Eigensystem[N[hamQM]]
},
avgqm[op_,t_]:=((ivec\[Conjugate].diag[[2]]\[ConjugateTranspose]) E^(I t diag[[1]])).(diag[[2]].(op.(diag[[2]]\[ConjugateTranspose] .(E^(-I t diag[[1]]) (diag[[2]].ivec)))));
qS[ss_][sp_]:=KroneckerProduct[KroneckerProduct[IdentityMatrix[2^(ss-1)],PauliMatrix[sp]/2],IdentityMatrix[2^(length-ss)]];
avgSingle=Table[avgqm[qS[ss][sp],t],{sp,3},{ss,length},{t,times}]
]
];
