ASUAWXT ;DSD/DFM - EXTRACT TRANS - CONVERT TO DDPS FORMAT ;  [ 04/15/98  3:00 PM ]
 ;;3.0;SAMS;**1**;AUG 20, 1993
BEGIN ;EP;FOR RE-EXTRACT//^ASUAWXTW
 D:'$D(U) ^XBKVAR
 I '$D(IO(0)) S IOP=$I D ^%ZIS
 S ASUW("RUN TYPE")=$G(ASUW("RUN TYPE"))
 S:ASUW("RUN TYPE")']"" ASUW("RUN TYPE")=0
 S ASUW("TYPE LAST RUN")=^ASUTLRUN(1,0)
 I $P(ASUW("TYPE LAST RUN"),U,2)=8 G REXT2^ASUAWXTW
 S ASUX("EXTRACT DATE")=DT
OPNHFS ;EP;FOR RE-EXTRACT//^ASUAWXTW
 S ASUW("SAVE MEDIUM")=$P(ASUW("TYPE LAST RUN"),U,9)
 S ASUK("WAREHOUSE")=$G(ASUK("WAREHOUSE"))
 I ASUK("WAREHOUSE")<2 D ^ASUAWBTS
 K ^ASUPDATA
 ;KILL OF UNSUBSCRIPTED GLOBAL - EXTRACT FOR AIB TO DATA CENTER - NEW EACH MONTH
 S ASULNPAD=""
 S (ASUC(0),ASUC("TOT REC COUNT"),ASUC("ACCUMULATE COUNT"),ASUC("TOT PROC"))=0
 F ASUG("FILE NUMBER")=1:1:7 D
 .S ASUC(0)=ASUC(0)+1
 .S ASUG("SAVE NODE")="^ASUTRSV("_ASUG("FILE NUMBER")_",ASUX(""RECORD #""),"
 .S ASUG("TRAN GLOBAL")=U_$P(^ASUXCTRL(ASUG("FILE NUMBER"),0),U,2)
 .S ASUG("TRAN CODE PIECE #")=$P(^ASUXCTRL(ASUG("FILE NUMBER"),0),U,4)
 .S ASUG("AREA CODE PIECE #")=$P(^ASUXCTRL(ASUG("FILE NUMBER"),0),U,7)
 .S ASUG("ZERO NODE")=ASUG("TRAN GLOBAL")_"(0)"
 .S DIE=$E($P(@ASUG("ZERO NODE"),U,2),1,10)
 .S ASUX("FILE NAME")=$P(@ASUG("ZERO NODE"),U)
 .I ASUK("WAREHOUSE")<2 S ASUTRX="W !,""Now Processing "_ASUX("FILE NAME")_" Records"",!" D LOG^ASUAUTIL
 .S ASUX("SORT XREF")=$S($P(ASUW("TYPE LAST RUN"),U,2)=8:"AX",1:"C")
 .I ASUX("SORT XREF")="AX" D
 ..S (ASUX("STATUS"),ASUX("READ STATUS"))=ASUX("EXTRACT DATE")
 ..S ASUX("READ STATUS")=ASUX("READ STATUS")-1
 .E  D
 ..S ASUX("READ STATUS")=$S($P(ASUW("TYPE LAST RUN"),U,2)=9:"X",1:"T")
 ..S ASUX("STATUS")=$S(ASUX("READ STATUS")="X":"Y",1:"U")
 .S ASUX("RECORD #")=""
 .S ASUG("FIND STATUS")=ASUG("TRAN GLOBAL")_"(ASUX(""SORT XREF""),ASUX(""READ STATUS""))"
 .S ASUG("FIND RECORD")=ASUG("TRAN GLOBAL")_"(ASUX(""SORT XREF""),ASUX(""READ STATUS""),ASUX(""RECORD #""))"
 .F  S ASUX("READ STATUS")=$O(@ASUG("FIND STATUS")) Q:ASUX("READ STATUS")'=ASUX("STATUS")  D
 ..F  S ASUX("RECORD #")=$O(@ASUG("FIND RECORD")) Q:ASUX("RECORD #")=""  D
 ...S DA=ASUX("RECORD #"),ASUX("EXTR FLAG")=1
 ...I ASUK("WAREHOUSE")<2 D ^ASUAWXT1
 ...Q:ASUX("SORT XREF")="AX"
 ...S ASUC("TOT PROC")=ASUC("TOT PROC")+1
 ...I ASUX("EXTR FLAG") S DR=".09///"_ASUX("EXTRACT DATE")_";.08///X" D ^DIE
 .S ASUC(ASUG("FILE NUMBER"))=ASUC("TOT REC COUNT")-ASUC("ACCUMULATE COUNT")
 .S $P(^ASUXCTRL(ASUG("FILE NUMBER"),0),U,5)=ASUC(ASUG("FILE NUMBER"))
 .S $P(^ASUXCTRL(ASUG("FILE NUMBER"),0),U,6)=ASUX("EXTRACT DATE")
 .S ASUC("ACCUMULATE COUNT")=ASUC("TOT REC COUNT")
 .I ASUK("WAREHOUSE")<2 S ASUTRX="W !,"""_ASUX("FILE NAME")_" Record Count : "","_$P(^ASUXCTRL(ASUG("FILE NUMBER"),0),U,5) D LOG^ASUAUTIL
 S ASUTRX="W !,*7,""Conversion Completed"",*7" D LOG^ASUAUTIL
 S ASUTRX="W !,""Total records processed: "","_ASUC("TOT PROC") D LOG^ASUAUTIL
 I ASUC("TOT REC COUNT")=0 D
 .S ASUTRX="W !,""There were no current records converted"",*7,!"
 .D LOG^ASUAUTIL
 .I 1
 E  D
 .S ASUTRX="W !,""Total records converted "","_ASUC("TOT REC COUNT")
 .D SETAREA^ASUAUARE
 .S ^ASUPDATA(0)=ASUK("ASUFAC")_U_ASUK("AREA NAME")_U_ASUX("EXTRACT DATE")_U_ASUX("EXTRACT DATE")_U_ASUX("EXTRACT DATE")_U_U_ASUC("TOT REC COUNT")
 .I ASUK("WAREHOUSE") D
 ..I ASUW("RUN TYPE") S $P(^ASUTLRUN(1,0),U,8)=ASUX("EXTRACT DATE") D ^ASUAWXT2
 .E  D
 ..S $P(^ASUTLRUN(1,0),U,8)=ASUX("EXTRACT DATE") D ^ASUAWXT2
 .S AUMED=$S(ASUW("SAVE MEDIUM")]"":ASUW("SAVE MEDIUM"),1:"F") D SAVE
 I $G(ASUK("PRINT QUEUED"))'=1 S DIR(0)="E" D ^DIR
 K ASUX,ASU0,ASU1,ASU2,ASUC,ASUG,ASUT,ASUNPAD,ASULNPAD,ASUFTAPE,AUGL
 K DA,DR,DIE,DTOUT,DUOUT,DIROUT
 K:$G(ASUW("RUN TYPE"))="" ASUV,ASUW
 Q
SV1 ;EP ;
 S AUMED="F",AUUF="/usr/spool/uucppublic"
 S:'$D(ASUK("WAREHOUSE")) ASUK("WAREHOUSE")=1
SAVE ;EP; SAVE GLOBAL
 I ASUK("WAREHOUSE")=2 Q
 S AUGL="ASUPDATA" D ^AUGSAVE K AUGL
 I AUFLG D
 .S ASUTRX="W !!,""Save of ASUPDATA Unsucessful - """ D LOG^ASUAUTIL
 .F ASU("AUFLG")=1:1 Q:'$D(AUFLG(ASU("AUFLG")))  D
 ..S ASUTRX="W """_AUFLG(ASU("AUFLG"))_""",!" D LOG^ASUAUTIL
 K AUFLG
 Q
 S AUGL="ASUTRSV",AUMED="F" D ^AUGSAVE K AUGL
 I AUFLG D
 .S ASUTRX="W !!,""Save of ASUTRSV Unsucessful - """ D LOG^ASUAUTIL
 .F ASU("AUFLG")=1:1 Q:'$D(AUFLG(ASU("AUFLG")))  D
 ..S ASUTRX="W """_AUFLG(ASU("AUFLG"))_""",!" D LOG^ASUAUTIL
 K AUFLG
 Q
