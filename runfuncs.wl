(* ::Package:: *)

singleRun=Function[{startEq,newInits},
Block[{newstate=First@NDSolve`Reinitialize[start,newInits],sol},
NDSolve`Iterate[newstate,tmax];
sol=NDSolve`ProcessSolutions[newstate][[All,2]];
data=(Through[sol[#]]&/@times)\[Transpose];
all=Partition[data,3];
spins=all[[1;;length]]\[Transpose];
cors=all[[length+1;;length+numbvars]]\[Transpose];
fobs=Table[(-1)^(i),{i,length}].(2 spins[[3]]);
bfobs2=fobs^2-Total[(2 spins[[3]])^2]+length;
bestfobs2=bfobs2+2Total[((2 spins[[3,#]])(2 spins[[3,#+1]]))&/@bsites-4cors[[3]]];
{spins,fobs,fobs^2,bfobs2,bestfobs2}
]
];


TWASU2Spins:=(
start=First@NDSolve`ProcessEquations[Flatten[{eqall2,initsSingleSpin[1]}],Flatten[Table[cS[addl[ss]][sp],{ss,length},{sp,3}]],{t,0,tmax}];
fullTWA2=0;
Table[AddTo[fullTWA2,(*wignerWeight[[rr]]*) singleRun[start,Flatten[{initsSingleSpin[rr]}]]/runs];,{rr,runs}];
(*qfi=(fullTWA2[[3]]-fullTWA2[[2]]^2)/length;
{fullTWA2[[1]],qfi}*)
fullTWA2
)


TWASU4Spins:=(
start=First@NDSolve`ProcessEquations[Flatten[{eqall4,initsSingleSpin[1],initsBiSpin[1]}],Flatten[{Table[cS[addl[ss]][sp],{ss,length},{sp,3}](*,Table[cB[addl[ss]][sp1,sp2],{ss,bsites},{sp1,3},{sp2,3}]*)}],{t,0,tmax}];
fullTWA4=0;
Table[AddTo[fullTWA4,(*wignerWeight[[rr]]*) singleRun[start,Flatten[{initsSingleSpin[rr],initsBiSpin[rr]}]]/runs];,{rr,runs}];
fullTWA4
(*TWASingle=Partition[fullTWA4[[1;;3length]],3]\[Transpose]*)
(*TWABi=Partition[Partition[fullTWA4[[3length+1;;3length+9numbvars]],9]\[Transpose],3];*)
)


TWASU4SpinsGau:=(
start=First@NDSolve`ProcessEquations[Flatten[{eqall4,initsSforB,initsB}],Flatten[{Table[cS[addl[ss]][sp],{ss,length},{sp,3}](*,Table[cB[addl[ss]][sp1,sp2],{ss,bsites},{sp1,3},{sp2,3}]*)}],{t,0,tmax}];
fullTWA4=0;
Table[AddTo[fullTWA4,(*wignerWeight[[rr]]*) singleRun[start,Flatten[{initsSforB,initsB}]]/runs];,{rr,runs}];
(*TWASingle=Partition[fullTWA4[[1;;3length]],3]\[Transpose]*)
fullTWA4
(*TWABi=Partition[Partition[fullTWA4[[3length+1;;3length+9numbvars]],9]\[Transpose],3];*)
)


TWASU4SpinsWW:=(
start=First@NDSolve`ProcessEquations[Flatten[{eqall4,initsSingleSpin[1],initsBiSpin[1]}],Flatten[{Table[cS[addl[ss]][sp],{ss,length},{sp,3}],Table[cB[addl[ss]][sp1,sp2],{ss,bsites},{sp1,3},{sp2,3}]}],{t,0,tmax}];
fullTWA4=0;
Table[AddTo[fullTWA4,wignerWeight[[rr]]singleRun[start,Flatten[{initsSingleSpin[rr],initsBiSpin[rr]}]]/runs];,{rr,runs}];
TWASingle=Partition[fullTWA4[[1;;3length]],3]\[Transpose]
(*TWABi=Partition[Partition[fullTWA4[[3length+1;;3length+9numbvars]],9]\[Transpose],3];*)
)


TWASU2Disc:=(
start=First@NDSolve`ProcessEquations[Flatten[{eqall2,initsS}],Flatten[Table[cS[addl[ss]][sp],{ss,length},{sp,3}]],{t,0,tmax}];
fullTWA2=0;
Table[AddTo[fullTWA2,(*wignerWeight[[rr]]*) singleRun[start,Flatten[initsS]]/runs];,{rr,runs}];
fullTWA2
)


TWASU4Disc:=(
start=First@NDSolve`ProcessEquations[Flatten[{eqall4,initsSforB,initsB}],Flatten[{Table[cS[addl[ss]][sp],{ss,length},{sp,3}],Table[cB[addl[ss]][sp1,sp1],{ss,bsites},{sp1,3}(*,{sp2,3}*)]}],{t,0,tmax}];
fullTWA4=0;
Table[AddTo[fullTWA4,singleRun[start,Flatten[{initsSforB,initsB}]]/runs];,{rr,runs}];
allSpins=fullTWA4[[1]];
imb=fullTWA4[[2]];
vars=fullTWA4[[#]]-imb^2&/@{3,4,5};
{allSpins,imb,vars}
(*TWASingle=Partition[fullTWA4[[1;;3length]],3]\[Transpose]*)
(*TWABi=Partition[Partition[fullTWA4[[3length+1;;3length+9numbvars]],9]\[Transpose],3];*)
)
