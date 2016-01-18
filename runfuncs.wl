(* ::Package:: *)

singleRun=Function[{startEq,newInits},
Block[{newstate=First@NDSolve`Reinitialize[start,newInits],sol},
NDSolve`Iterate[newstate,tmax];
sol=NDSolve`ProcessSolutions[newstate][[All,2]];
(Through[sol[#]]&/@times)\[Transpose]
]
];


TWASU2Spins:=(
start=First@NDSolve`ProcessEquations[Flatten[{eqall2,initsSingleSpin[1]}],Flatten[Table[cS[addl[ss]][sp],{ss,length},{sp,3}]],{t,0,tmax}];
fullTWA2=0;
Table[AddTo[fullTWA2,(*wignerWeight[[rr]]*) singleRun[start,Flatten[{initsSingleSpin[rr]}]]/runs];,{rr,runs}];
TWASingle=Partition[fullTWA2[[1;;3length]],3]\[Transpose]
)


TWASU4Spins:=(
start=First@NDSolve`ProcessEquations[Flatten[{eqall4,initsSingleSpin[1],initsBiSpin[1]}],Flatten[{Table[cS[addl[ss]][sp],{ss,length},{sp,3}],Table[cB[addl[ss]][sp1,sp2],{ss,bsites},{sp1,3},{sp2,3}]}],{t,0,tmax}];
fullTWA4=0;
Table[AddTo[fullTWA4,(*wignerWeight[[rr]]*) singleRun[start,Flatten[{initsSingleSpin[rr],initsBiSpin[rr]}]]/runs];,{rr,runs}];
TWASingle=Partition[fullTWA4[[1;;3length]],3]\[Transpose]
(*TWABi=Partition[Partition[fullTWA4[[3length+1;;3length+9numbvars]],9]\[Transpose],3];*)
)


TWASU4SpinsGau:=(
start=First@NDSolve`ProcessEquations[Flatten[{eqall4,initsSforB,initsB}],Flatten[{Table[cS[addl[ss]][sp],{ss,length},{sp,3}],Table[cB[addl[ss]][sp1,sp2],{ss,bsites},{sp1,3},{sp2,3}]}],{t,0,tmax}];
fullTWA4=0;
Table[AddTo[fullTWA4,(*wignerWeight[[rr]]*) singleRun[start,Flatten[{initsSforB,initsB}]]/runs];,{rr,runs}];
TWASingle=Partition[fullTWA4[[1;;3length]],3]\[Transpose]
(*TWABi=Partition[Partition[fullTWA4[[3length+1;;3length+9numbvars]],9]\[Transpose],3];*)
)


TWASU4SpinsWW:=(
start=First@NDSolve`ProcessEquations[Flatten[{eqall4,initsSingleSpin[1],initsBiSpin[1]}],Flatten[{Table[cS[addl[ss]][sp],{ss,length},{sp,3}],Table[cB[addl[ss]][sp1,sp2],{ss,bsites},{sp1,3},{sp2,3}]}],{t,0,tmax}];
fullTWA4=0;
Table[AddTo[fullTWA4,wignerWeight[[rr]]singleRun[start,Flatten[{initsSingleSpin[rr],initsBiSpin[rr]}]]/runs];,{rr,runs}];
TWASingle=Partition[fullTWA4[[1;;3length]],3]\[Transpose]
(*TWABi=Partition[Partition[fullTWA4[[3length+1;;3length+9numbvars]],9]\[Transpose],3];*)
)
