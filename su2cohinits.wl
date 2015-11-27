(* ::Package:: *)

(* ::Subsection:: *)
(*Inits*)


norm=2/Sqrt[3];


wigTheta[sp_]:=ProbabilityDistribution[1/2 Abs[(1+(-1)^(sp+1) Sqrt[3] Cos[\[Theta]])Sin[\[Theta]]/norm],{\[Theta],0,\[Pi]}]


funcMetric[sp_,rands_]:=(-1)^sp (1-2UnitStep[N[ArcCos[(-1)^sp 1/Sqrt[3]]]-rands])


wigSpin[\[Theta]_,\[Phi]_]:={1/2 Sqrt[3] Cos[\[Phi]] Sin[\[Theta]],1/2 Sqrt[3] Sin[\[Phi]] Sin[\[Theta]],1/2 Sqrt[3] Cos[\[Theta]]}


randomTheta=RandomVariate[wigTheta[#],{runs}]&/@initspin;


randomPhi=RandomReal[{0,2\[Pi]},{runs}]&/@initspin;


metric=Thread[funcMetric[initspin,randomTheta]];


metricperrun=Fold[Times,#]&/@metric;


randomSpin=wigSpin@@@({randomTheta,randomPhi}\[Transpose]);


initsSpCoh[rr_]:=MapThread[Equal,{Outer[cS[#1][#2][0]&,Range[length],Range[3]],randomSpin[[All,All,rr]]},2]
