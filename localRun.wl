(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


SetDirectory[NotebookDirectory[]]


(*SetDirectory[Directory[]<>"/TWAsu4"];*)


<<const.wl


<<modfuncs.wl


<<dynfunc.wl


<<runfuncs.wl


(*<<XXZhameqns.wl*)


<<heiswrfham.wl


<<gaussianinits.wl


(*<<su2cohinits.wl*)


(*<<su4cohinits.wl*)


(*<<su4cohinitsGaussian.wl*)


(*<<su4cohinitsPolarExp.wl*)


(*<<deltainits.wl*)


(* ::Subsection:: *)
(*run TWA*)


(*eachTWA2 = Table[solv = NDSolveValue[Flatten[{eqall2, initsSpCoh[rr]}], Flatten[Table[cS[addl[ss]][sp], {ss, length}, {sp, 3, 3}]], {t, 0, tmax}]; {(Through[solv[#]] & /@ times)\[Transpose], Total[(Through[solv[#]] Through[solv[0]] & /@ times)\[Transpose]]/length}, {rr, runs}];//AbsoluteTiming
fullTWA2 = Total[norm^length metricperrun eachTWA2]/runs;*)


Dynamic[rr]


start=First@NDSolve`ProcessEquations[Flatten[{eqall4,initsSingleSpin[1],initsBiSpin[1]}],Flatten[{Table[cS[addl[ss]][sp],{ss,length},{sp,3}],Table[cB[addl[ss]][sp1,sp2],{ss,bsites},{sp1,3},{sp2,3}]}],{t,0,tmax}];
fullTWA4=0;
Table[AddTo[fullTWA4,(*wignerWeight[[rr]] *)singleRun[start,Flatten[{initsSingleSpin[rr],initsBiSpin[rr]}]]/runs];,{rr,runs}];//AbsoluteTiming


TWASingle=Partition[fullTWA4[[1;;3length]],3]\[Transpose];
TWABi=Partition[Partition[fullTWA4[[3length+1;;3length+9numbvars]],9]\[Transpose],3];


mmu=MaxMemoryUsed[]/10.^6


(*SetDirectory[ParentDirectory[]];*)


Save["4site.dat",{mmu,TWASingle,TWABi}];
