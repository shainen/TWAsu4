(* ::Package:: *)

fieldsW10={-6.810118375580723`,5.399321183414791`,7.047572832814368`,-4.414079687154078`,-8.41626615919514`,7.9393892160016355`,-5.806999450409989`,-8.53697581757193`,0.6676914555529989`,-2.5431706107186014`,6.844895559224515`,3.891957559769441`};


fieldsW5={-3.4050591877903615`,2.6996605917073957`,3.523786416407184`,-2.207039843577039`,-4.20813307959757`,3.9696946080008177`,-2.9034997252049943`,-4.268487908785965`,0.33384572777649946`,-1.2715853053593007`,3.4224477796122574`,1.9459787798847206`};


randfield2={4.797601952507758`,3.6663364995139176`,2.3403917734404622`,3.63202565523515`,1.210998478118949`,-3.2268904673253154`,0.32605569583666494`,2.0656050398596264`,3.7266406366843867`,4.637831755828962`,0.3933728187737078`,3.1360368038848456`};


hamself[ss_]:=randfields[[addl[ss]]]cS[addl[ss]][fisp][t]


sscoup[ss_]:=j(cS[addl[ss]][1][t]cS[addl[ss+1]][1][t]+cS[addl[ss]][2][t]cS[addl[ss+1]][2][t]+cS[addl[ss]][3][t]cS[addl[ss+1]][3][t])


bcoup[ss_]:=j(cB[addl[ss]][1,1][t]+cB[addl[ss]][2,2][t]+cB[addl[ss]][3,3][t])


(*hamcoupsu2[ss_]:=sscoup[addl[ss]]*)


hamcoupsu2[ss_]:=If[ss==length,0,sscoup[addl[ss]]]


hamcoupsu4[ss_]:=If[MemberQ[bsites,addl[ss]],bcoup[addl[ss]],hamcoupsu2[addl[ss]]]


eqss4=Table[cS[addl[ss]][sp]'[t]==sdot[addl[ss],sp,hamself[addl[ss]]+hamcoupsu4[addl[ss-1]]+hamcoupsu4[addl[ss]]],{ss,length},{sp,3}];


eqsb4=Table[cB[addl[ss]][sp1,sp2]'[t]==bdot[addl[ss],sp1,sp2,hamself[addl[ss]]+hamself[addl[ss+1]]+hamcoupsu4[addl[ss]]+hamcoupsu4[addl[ss-1]]+hamcoupsu4[addl[ss+1]]],{ss,bsites},{sp1,3},{sp2,3}];


eqall4=Flatten[{eqss4,eqsb4}];


eqss2=Table[cS[addl[ss]][sp]'[t]==sdot[addl[ss],sp,hamself[addl[ss]]+hamcoupsu2[addl[ss-1]]+hamcoupsu2[addl[ss]]],{ss,length},{sp,3}];


eqall2=Flatten[{eqss2}];
