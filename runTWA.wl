(* ::Package:: *)

(* ::Section:: *)
(*All*)


SetDirectory[NotebookDirectory[]];


<<const.wl


<<modfuncs.wl


<<dynfunc.wl


(*<<XXZhameqns.wl*)


<<heiswrfham.wl


<<su2cohinits.wl


(* ::Subsection:: *)
(*run TWA*)


eachTWA2 = Table[solv = NDSolveValue[Flatten[{eqall2, initsSpCoh[rr]}], Flatten[Table[cS[addl[ss]][sp], {ss, length}, {sp, 3, 3}]], {t, 0, tmax}]; {(Through[solv[#]] & /@ times)\[Transpose], Total[(Through[solv[#]] Through[solv[0]] & /@ times)\[Transpose]]/length}, {rr, runs}];
fullTWA2 = Total[norm^length metricperrun eachTWA2]/runs;


mmu=MaxMemoryUsed[]/10.^6;


Save["12site.dat",{mmu,fullTWA2}];
