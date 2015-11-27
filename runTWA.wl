(* ::Package:: *)

(* ::Section:: *)
(*All*)


SetDirectory["/Users/shainen/Dropbox/Research/random clusters/3 site/TWAsu4"];


<<const.wl


<<modfuncs.wl


<<dynfunc.wl


<<XXZhameqns.wl


<<su2cohinits.wl


(* ::Subsection:: *)
(*run TWA*)


(* ::Input:: *)
(*eachTWA2 = Table[solv = NDSolveValue[Flatten[{eqall2, initsSpCoh[rr]}], Flatten[Table[cS[addl[ss]][sp], {ss, length}, {sp, 3, 3}]], {t, 0, tmax}]; {(Through[solv[#]] & /@ times)\[Transpose], Total[(Through[solv[#]] Through[solv[0]] & /@ times)\[Transpose]]/length}, {rr, runs}];*)
(*fullTWA2 = Total[norm^length metricperrun eachTWA2 ]/runs;*)


(*mmu=MaxMemoryUsed[]/10.^6;*)


(*Save["12site.dat",{mmu,(*fullTWA2,sqTWA2,*)fullTWA4,sqTWA4}];*)


Dynamic[rr]
