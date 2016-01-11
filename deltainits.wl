(* ::Package:: *)

initsSingleSpin[rr_] :={Table[cS[si][ss][0]==0,{si,length},{ss,2}],Table[cS[si][3][0]==1.5-initspin[[si]],{si,length}]}


initsBiSpin[rr_] :={Table[cB[si][ss1,ss2][0]==0,{si,bsites},{ss1,2},{ss2,2}],Table[cB[si][3,ss2][0]==0,{si,bsites},{ss2,2}],Table[cB[si][ss1,3][0]==0,{si,bsites},{ss1,2}],Table[cB[si][3,3][0]==(1.5-initspin[[si]])(1.5-initspin[[addl[si+1]]]),{si,bsites}]}
