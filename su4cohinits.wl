(* ::Package:: *)

measureR[r_]:=6Cos[r]Sin[r]^5


measure\[Theta]1[\[Theta]1_]:=4Cos[\[Theta]1]^3 Sin[\[Theta]1]


measure\[Theta]2[\[Theta]2_]:=Sin[2 \[Theta]2]


measureNorm=1/(2\[Pi]^3);


wigReferenceState[r_,\[Theta]1_,\[Theta]2_]:=measureNorm 1/4 (1+Sqrt[5]+2 Sqrt[5] Cos[2 r])


probFromMeasure=ProbabilityDistribution[#[x],{x,0,\[Pi]/2}]&/@{measureR,measure\[Theta]1,measure\[Theta]2};


singleSpinWeylSym[r_,\[Theta]1_,\[Theta]2_,\[Phi]1_,\[Phi]2_,\[Phi]3_]:={{{Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2])},{Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]+Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]+Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3]),-Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]+Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3]),-Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]+Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3])},{1/2 Sqrt[5] (Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]-Sin[\[Theta]1]^2)),1/2 Sqrt[5] (Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]-Sin[\[Theta]1]^2)),-(1/2) Sqrt[5] (Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]-Sin[\[Theta]1]^2)),-(1/2) Sqrt[5] (Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]-Sin[\[Theta]1]^2))}},{{Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]+Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]+Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]+Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]+Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2])},{Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]),-Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]),Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]),-Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3])},{1/2 Sqrt[5] (Cos[r]^2-Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]+Sin[\[Theta]1]^2)),1/2 Sqrt[5] (-Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]+Sin[\[Theta]1]^2)),1/2 Sqrt[5] (Cos[r]^2-Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]+Sin[\[Theta]1]^2)),1/2 Sqrt[5] (-Cos[r]^2+Sin[r]^2 (Cos[\[Theta]1]^2 Cos[2 \[Theta]2]+Sin[\[Theta]1]^2))}}}


biSpinWeylSym[r_,\[Theta]1_,\[Theta]2_,\[Phi]1_,\[Phi]2_,\[Phi]3_]:={{{1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2-\[Phi]3] Sin[r] Sin[\[Theta]2]),1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2-\[Phi]3] Sin[r] Sin[\[Theta]2]),1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2-\[Phi]3] Sin[r] Sin[\[Theta]2]),1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2-\[Phi]3] Sin[r] Sin[\[Theta]2])},{1/2 Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1]-Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2-\[Phi]3]),1/2 Sqrt[5] Sin[r] (-Cos[r] Sin[\[Theta]1] Sin[\[Phi]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2-\[Phi]3]),1/2 Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1]-Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2-\[Phi]3]),1/2 Sqrt[5] Sin[r] (-Cos[r] Sin[\[Theta]1] Sin[\[Phi]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2-\[Phi]3])},{1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]-Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]+Cos[r] Cos[\[Phi]2] Sin[\[Theta]2]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[\[Theta]2] Cos[\[Phi]1-\[Phi]3] Sin[r] Sin[\[Theta]1]-Cos[r] Cos[\[Phi]2] Sin[\[Theta]2])}},{{1/2 Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2-\[Phi]3]),1/2 Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2-\[Phi]3]),-(1/2) Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2-\[Phi]3]),-(1/2) Sqrt[5] Sin[r] (Cos[r] Sin[\[Theta]1] Sin[\[Phi]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Sin[r] Sin[\[Theta]2] Sin[\[Phi]2-\[Phi]3])},{1/2 Sqrt[5] Sin[r] (-Cos[r] Cos[\[Phi]1] Sin[\[Theta]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2-\[Phi]3] Sin[r] Sin[\[Theta]2]),1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1]-Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2-\[Phi]3] Sin[r] Sin[\[Theta]2]),1/2 Sqrt[5] Sin[r] (Cos[r] Cos[\[Phi]1] Sin[\[Theta]1]-Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2-\[Phi]3] Sin[r] Sin[\[Theta]2]),1/2 Sqrt[5] Sin[r] (-Cos[r] Cos[\[Phi]1] Sin[\[Theta]1]+Cos[\[Theta]1]^2 Cos[\[Theta]2] Cos[\[Phi]2-\[Phi]3] Sin[r] Sin[\[Theta]2])},{1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]-Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]+Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]+Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Sin[\[Theta]2] Sin[\[Phi]2]-Cos[\[Theta]2] Sin[r] Sin[\[Theta]1] Sin[\[Phi]1-\[Phi]3])}},{{1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]-Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]-Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]+Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Cos[r] Cos[\[Theta]2] Cos[\[Phi]3]+Cos[\[Phi]1-\[Phi]2] Sin[r] Sin[\[Theta]1] Sin[\[Theta]2])},{1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]-Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]-Cos[r] Cos[\[Theta]2] Sin[\[Phi]3]),1/2 Sqrt[5] Cos[\[Theta]1] Sin[r] (-Sin[r] Sin[\[Theta]1] Sin[\[Theta]2] Sin[\[Phi]1-\[Phi]2]+Cos[r] Cos[\[Theta]2] Sin[\[Phi]3])},{1/16 Sqrt[5] (2+2 Cos[2 r]+Cos[2 (r-\[Theta]1)]-2 Cos[2 \[Theta]1]+Cos[2 (r+\[Theta]1)]),-(1/16) Sqrt[5] (2+2 Cos[2 r]+Cos[2 (r-\[Theta]1)]-2 Cos[2 \[Theta]1]+Cos[2 (r+\[Theta]1)]),-(1/16) Sqrt[5] (2+2 Cos[2 r]+Cos[2 (r-\[Theta]1)]-2 Cos[2 \[Theta]1]+Cos[2 (r+\[Theta]1)]),1/16 Sqrt[5] (2+2 Cos[2 r]+Cos[2 (r-\[Theta]1)]-2 Cos[2 \[Theta]1]+Cos[2 (r+\[Theta]1)])}}}


randomThetaSu4=RandomVariate[#,{runs,numbvars}]&/@probFromMeasure;


randomPhiSu4=RandomReal[{0,2\[Pi]},{3,runs,numbvars}];


biSpinInitConfig=FromDigits[initspin[[#;;addl[#+1]]],2]-2&/@bsites;


wignerWeight=(2\[Pi])^(3 numbvars) Times@@@(wigReferenceState@@randomThetaSu4);


randomSingleSpin=Transpose[(singleSpinWeylSym@@Join[randomThetaSu4,randomPhiSu4])[[All,All,#1,All,#2]]&@@@({biSpinInitConfig,Range[numbvars]}\[Transpose]),{2,4,3,1}];


randomBiSpin=Transpose[(biSpinWeylSym@@Join[randomThetaSu4,randomPhiSu4])[[All,All,#1,All,#2]]&@@@({biSpinInitConfig,Range[numbvars]}\[Transpose]),{2,3,4,1}];


initsSingleSpin[rr_] := MapThread[Equal, {Outer[{cS[#1][#2][0],cS[addl[#1+1]][#2][0]} &, bsites, Range[3]], randomSingleSpin[[rr]]}, 3]


initsBiSpin[rr_] := MapThread[Equal, {Outer[cB[#1][#2,#3][0] &, bsites, Range[3],Range[3]], randomBiSpin[[rr]]}, 3]
