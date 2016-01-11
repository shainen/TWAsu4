(* ::Package:: *)

bdot[s_,a_,d_,ham_]:=Total[D[ham,#1](#2)&@@@{{cS[addl[s]][add3[a+1]][t],cB[addl[s]][add3[a+2],add3[d]][t]},{cS[addl[s]][add3[a+2]][t],-cB[addl[s]][add3[a+1],add3[d]][t]},{cS[addl[s+1]][add3[d+1]][t],cB[addl[s]][add3[a],add3[d+2]][t]},{cS[addl[s+1]][add3[d+2]][t],-cB[addl[s]][add3[a],add3[d+1]][t]},{cB[addl[s]][add3[a+1],add3[d]][t],cS[addl[s]][add3[a+2]][t]/4},{cB[addl[s]][add3[a+2],add3[d]][t],-cS[addl[s]][add3[a+1]][t]/4},{cB[addl[s]][add3[a],add3[d+1]][t],cS[addl[s+1]][add3[d+2]][t]/4},{cB[addl[s]][add3[a],add3[d+2]][t],-cS[addl[s+1]][add3[d+1]][t]/4}}]


sdot[s_,a_,ham_]:=Total[D[ham,#1](#2)&@@@{{cS[addl[s]][add3[a+1]][t],cS[addl[s]][add3[a+2]][t]},{cS[addl[s]][add3[a+2]][t],-cS[addl[s]][add3[a+1]][t]}}~Join~Flatten[Table[{{cB[addl[s]][add3[a+1],d][t],cB[addl[s]][add3[a+2],d][t]},{cB[addl[s]][add3[a+2],d][t],-cB[addl[s]][add3[a+1],d][t]}},{d,3}],1]~Join~Flatten[Table[{{cB[addl[s-1]][d,add3[a+1]][t],cB[addl[s-1]][d,add3[a+2]][t]},{cB[addl[s-1]][d,add3[a+2]][t],-cB[addl[s-1]][d,add3[a+1]][t]}},{d,3}],1]]
