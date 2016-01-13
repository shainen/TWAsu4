(* ::Package:: *)

measureR[r_]:=6Cos[r]Sin[r]^5


measure\[Theta]1[\[Theta]1_]:=4Cos[\[Theta]1]^3 Sin[\[Theta]1]


measure\[Theta]2[\[Theta]2_]:=Sin[2 \[Theta]2]


wigReferenceState[r_]:= 1/4 (1+Sqrt[5]+2 Sqrt[5] Cos[2 r])


wigGoNeg=N[ArcCos[-(5+Sqrt[5])/10]/2];


metric[r_]:=1-2HeavisideTheta[r-wigGoNeg]


normedWigWithMeasure[r_]:= 4 measureR[r] wigReferenceState[r]


wigNorm=N[Integrate[Abs[normedWigWithMeasure[vr]],{vr,0,\[Pi]/2}]];


posWigWithMeasure[r_]:= Abs[normedWigWithMeasure[r]]/wigNorm


probFromMeasure=ProbabilityDistribution[#[x],{x,0,\[Pi]/2}]&/@{posWigWithMeasure,measure\[Theta]1,measure\[Theta]2};


singleSpinWeylSymList[r_,\[Theta]1_,\[Theta]2_,\[Phi]1_,\[Phi]2_,\[Phi]3_]:={{{Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2])},{Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]+Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]+Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3]),-Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]+Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3]),-Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]+Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3])},{1/2 Sqrt[5] (Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]-Sin[\[Theta]1]^2)),1/2 Sqrt[5] (Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]-Sin[\[Theta]1]^2)),-(1/2) Sqrt[5] (Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]-Sin[\[Theta]1]^2)),-(1/2) Sqrt[5] (Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]-Sin[\[Theta]1]^2))}},{{Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]+Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]+Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]+Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]+Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2])},{Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]),-Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]),-Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3])},{1/2 Sqrt[5] (Cos[r]^2-Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]+Sin[\[Theta]1]^2)),1/2 Sqrt[5] (-Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]+Sin[\[Theta]1]^2)),1/2 Sqrt[5] (Cos[r]^2-Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]+Sin[\[Theta]1]^2)),1/2 Sqrt[5] (-Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]+Sin[\[Theta]1]^2))}}}


singleSpinWeylSym[ref_][r_,\[Theta]1_,\[Theta]2_,\[Phi]1_,\[Phi]2_,\[Phi]3_]:=N[singleSpinWeylSymList[r,\[Theta]1,\[Theta]2,\[Phi]1,\[Phi]2,\[Phi]3][[All,All,ref]]]


biSpinWeylSymList[r_, \[Theta]1_, \[Theta]2_, \[Phi]1_, \[Phi]2_, \[Phi]3_] := {{{1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2 - \[Phi]3] Sin[r] Sin[\[Theta]2]), 1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2 - \[Phi]3] Sin[r] Sin[\[Theta]2]), 1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2 - \[Phi]3] Sin[r] Sin[\[Theta]2]), 1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2 - \[Phi]3] Sin[r] Sin[\[Theta]2])}, {1/2 Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1] - Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2 - \[Phi]3]), 1/2 Sqrt[5] Sin[r] (-Cos[r] Sin[\[Theta]1] Sin[\[Phi]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2 - \[Phi]3]), 1/2 Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1] - Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2 - \[Phi]3]), 1/2 Sqrt[5] Sin[r] (-Cos[r] Sin[\[Theta]1] Sin[\[Phi]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2 - \[Phi]3])}, {1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[\[Theta]2] Cos[\[Phi]1 - \[Phi]3] Sin[r] Sin[\[Theta]1] + Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1 - \[Phi]3] Sin[r] Sin[\[Theta]1] - Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[\[Theta]2] Cos[\[Phi]1 - \[Phi]3] Sin[r] Sin[\[Theta]1] + Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1 - \[Phi]3] Sin[r] Sin[\[Theta]1] - Cos[r] Cos[\[Phi]2] Sin[\[Theta]2])}}, {{1/2 Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2 - \[Phi]3]), 1/2 Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2 - \[Phi]3]), -(1/2) Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2 - \[Phi]3]), -(1/2) Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2 - \[Phi]3])}, {1/2 Sqrt[5] Sin[r] (-Cos[r] Cos[\[Phi]1] Sin[\[Theta]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2 - \[Phi]3] Sin[r] Sin[\[Theta]2]), 1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1] - Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2 - \[Phi]3] Sin[r] Sin[\[Theta]2]), 1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1] - Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2 - \[Phi]3] Sin[r] Sin[\[Theta]2]), 1/2 Sqrt[5] Sin[r] (-Cos[r] Cos[\[Phi]1] Sin[\[Theta]1] + Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2 - \[Phi]3] Sin[r] Sin[\[Theta]2])}, {1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2] - Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1 - \[Phi]3]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[r] Sin[\[Theta]2] Sin[\[Phi]2] + Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1 - \[Phi]3]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[r] Sin[\[Theta]2] Sin[\[Phi]2] + Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1 - \[Phi]3]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2] - Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1 - \[Phi]3])}}, {{1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3] - Cos[\[Phi]1 - \[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3] - Cos[\[Phi]1 - \[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[r] Cos[\[Theta]2] Cos[\[Phi]3] + Cos[\[Phi]1 - \[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[r] Cos[\[Theta]2] Cos[\[Phi]3] + Cos[\[Phi]1 - \[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2])}, {1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1 - \[Phi]2] + Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1 - \[Phi]2] - Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1 - \[Phi]2] - Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]), 1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1 - \[Phi]2] + Cos[r] Cos[\[Theta]2] Sin[\[Phi]3])}, {1/16 Sqrt[5] (2 + 2 Cos[2 r] + Cos[2 (r - \[Theta]1)] - 2 Cos[2 \[Theta]1] + Cos[2 (r + \[Theta]1)]), -(1/16) Sqrt[5] (2 + 2 Cos[2 r] + Cos[2 (r - \[Theta]1)] - 2 Cos[2 \[Theta]1] + Cos[2 (r + \[Theta]1)]), -(1/16) Sqrt[5] (2 + 2 Cos[2 r] + Cos[2 (r - \[Theta]1)] - 2 Cos[2 \[Theta]1] + Cos[2 (r + \[Theta]1)]), 1/16 Sqrt[5] (2 + 2 Cos[2 r] + Cos[2 (r - \[Theta]1)] - 2 Cos[2 \[Theta]1] + Cos[2 (r + \[Theta]1)])}}}


biSpinWeylSym[ref_][r_, \[Theta]1_, \[Theta]2_, \[Phi]1_, \[Phi]2_, \[Phi]3_] := N[biSpinWeylSymList[r,\[Theta]1,\[Theta]2,\[Phi]1,\[Phi]2,\[Phi]3][[All,All,ref]]]


randomThetaSu4=RandomVariate[#,{runs,numbvars}]&/@probFromMeasure;


randomPhiSu4=RandomReal[{0,2\[Pi]},{3,runs,numbvars}];


biSpinInitConfig=FromDigits[initspin[[{#,addl[#+1]}]],2]-2&/@bsites;


wignerWeight=Times@@@(wigNorm metric[randomThetaSu4[[1]]]);


allAngles=Transpose[Join[randomThetaSu4,randomPhiSu4],{2,3,1}];


randomSingleSpin=
Transpose[MapThread[Apply,{singleSpinWeylSym/@biSpinInitConfig,allAngles}],{2,4,3,1}];


randomBiSpin=Transpose[MapThread[Apply,{biSpinWeylSym/@biSpinInitConfig,allAngles}],{2,3,4,1}];


initsSingleSpin[rr_] := MapThread[Equal, {Outer[{cS[#1][#2][0],cS[addl[#1+1]][#2][0]} &, bsites, Range[3]], randomSingleSpin[[rr]]}, 3]


initsBiSpin[rr_] := MapThread[Equal, {Outer[cB[#1][#2,#3][0] &, bsites, Range[3],Range[3]], randomBiSpin[[rr]]}, 3]
