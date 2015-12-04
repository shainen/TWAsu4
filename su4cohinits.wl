(* ::Package:: *)

wigSzBasis[r_,\[Theta]1_,\[Theta]2_]:={1/4 (1+Sqrt[5]+2 Sqrt[5] Cos[2 r]),1/4 (1-Sqrt[5] Cos[r]^2+Sqrt[5] Cos[\[Theta]1]^2 (1+2 Cos[2 \[Theta]2]) Sin[r]^2-Sqrt[5] Sin[r]^2 Sin[\[Theta]1]^2),1/4 (1-Sqrt[5] Cos[r]^2-Sqrt[5] Cos[\[Theta]1]^2 (-1+2 Cos[2 \[Theta]2]) Sin[r]^2-Sqrt[5] Sin[r]^2 Sin[\[Theta]1]^2),1/8 (2-2 Sqrt[5] Cos[2 r]+Sqrt[5] Cos[2 (r-\[Theta]1)]-2 Sqrt[5] Cos[2 \[Theta]1]+Sqrt[5] Cos[2 (r+\[Theta]1)])}


measure[r_,\[Theta]1_,\[Theta]2_]:=(12 Cos[r] Cos[\[Theta]1]^3 Sin[r]^5 Sin[\[Theta]1] Sin[2 \[Theta]2])/\[Pi]^3


singleSpinWeylSym[r_,\[Theta]1_,\[Theta]2_,\[Phi]1_,\[Phi]2_,\[Phi]3_]:={{Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]+Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3]),1/2 Sqrt[5] (Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]-Sin[\[Theta]1]^2))},{Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]+Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]),1/2 Sqrt[5] (Cos[r]^2-Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]+Sin[\[Theta]1]^2))}}


biSpinWeylSym[r_,\[Theta]1_,\[Theta]2_,\[Phi]1_,\[Phi]2_,\[Phi]3_]:={{1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2-\[Phi]3] Sin[r] Sin[\[Theta]2]),1/2 Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1]-Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2-\[Phi]3]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2])},{1/2 Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2-\[Phi]3]),1/2 Sqrt[5] Sin[r] (-Cos[r] Cos[\[Phi]1] Sin[\[Theta]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2-\[Phi]3] Sin[r] Sin[\[Theta]2]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]-Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3])},{1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]-Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]),1/16 Sqrt[5] (2+2 Cos[2 r]+Cos[2 (r-\[Theta]1)]-2 Cos[2 \[Theta]1]+Cos[2 (r+\[Theta]1)])}}


randomTheta=RandomReal[{0,\[Pi]/2},{3,runs,numbvars}];


randomPhi=RandomReal[{0,2\[Pi]},{3,runs,numbvars}];


randomSingleSpin=Transpose[singleSpinWeylSym@@Join[randomTheta,randomPhi],{4,3,1,2}];


randomBiSpin=Transpose[biSpinWeylSym@@Join[randomTheta,randomPhi],{3,4,1,2}];


initsSingleSpin[rr_] := MapThread[Equal, {Outer[{cS[#1][#2][0],cS[addl[#1+1]][#2][0]} &, bsites, Range[3]], randomSingleSpin[[rr]]}, 3]


initsBiSpin[rr_] := MapThread[Equal, {Outer[cB[#1][#2,#3][0] &, bsites, Range[3],Range[3]], randomBiSpin[[rr]]}, 3]


biSpinInitConfig=FromDigits[initspin[[#;;addl[#+1]]],2]-2&/@bsites;


wignerWeight:=(measure@@randomTheta)((wigSzBasis@@randomTheta)[[


eachTWA2 = Table[solv = NDSolveValue[Flatten[{eqall2, initsSpCoh[rr]}], Flatten[Table[cS[addl[ss]][sp], {ss, length}, {sp, 3, 3}]], {t, 0, tmax}]; {(Through[solv[#]] & /@ times)\[Transpose], Total[(Through[solv[#]] Through[solv[0]] & /@ times)\[Transpose]]/length}, {rr, runs}];
fullTWA2 = Total[norm^length metricperrun eachTWA2]/runs;


eachTWA4=Table[solv=NDSolveValue[Flatten[{eqall4,initsB}],Flatten[Table[cS[addl[ss]][sp],{ss,length},{sp,3,3}]],{t,0,tmax}];{(Through[solv[#]]&/@times)\[Transpose],Total[(Through[solv[#]]Through[solv[0]]&/@times)\[Transpose]]/length},{rr,runs}];
fullTWA4=Total[eachTWA4]/runs;
sqTWA4=Total[eachTWA4^2]/runs;


Dimensions[(measure@@randomTheta)\[Transpose]]


Dimensions[Product[(wigSzBasis@@randomTheta)[[biSpinInitConfig[[#]],All,#]]&/@Range[numbvars]]


Dimensions[((wigSzBasis@@randomTheta)[[biSpinInitConfig[[#]],All,#]]&)/@Range[numbvars]]


Dimensions[(measure@@randomTheta)(((wigSzBasis@@randomTheta)[[biSpinInitConfig[[#]],All,#]]&)/@Range[numbvars])\[Transpose]]


Times@@@((measure@@randomTheta)(((wigSzBasis@@randomTheta)[[biSpinInitConfig[[#]],All,#]]&)/@Range[numbvars])\[Transpose])


(measure@@randomTheta)


(((wigSzBasis@@randomTheta)[[biSpinInitConfig[[#]],All,#]]&)/@Range[numbvars])\[Transpose]
