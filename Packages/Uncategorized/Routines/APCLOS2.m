APCLOS2 ; IHS/CMI/LAB - calculate ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
POP ;Population and third party sections of operational sum
PAT S DFN=0 F  S DFN=$O(^AUPNPAT(DFN)) Q:DFN'=+DFN  K DOD,DOB D PROC
 K DFN,APCLV,APCLFYBI,APCLFYEI,APCLGOTA,DOB,DOD,APCLGOT1,APCLHRN,APCLBDAT,APCLEDAT,APCLMCR,APCLACE,APCLGOT,APCLMDFN,APCLVAL,APCLNDFN,APCLR
 Q
PROC ;
 Q:$$DEMO^APCLUTL(DFN,$G(APCLDEMO))
 D LIVREG
 Q:'APCLGOT1
 D NEWREG
 D DEATHS
 D BIRTHS
 D THIRD^APCLOS21
 Q
LIVREG S (APCLGOT1,APCLHRN)=0 F J=0:0 S APCLHRN=$O(^AUPNPAT(DFN,41,APCLHRN)) Q:APCLHRN'=+APCLHRN!(APCLGOT1)  D LR2
 Q
LR2 ;
 Q:'$D(^AUPNPAT(DFN,0))
 Q:'$D(^XTMP("APCLSU",APCLJOB,APCLBTH,$P(^AUPNPAT(DFN,41,APCLHRN,0),U)))
 Q:$P(^DPT(DFN,0),U,19)]""
 Q:$P(^AUPNPAT(DFN,0),U,2)>APCLFYE
 I $D(^DPT(DFN,.35)),$P(^DPT(DFN,.35),U)]"" S DOD=$P(^DPT(DFN,.35),U)
 D LR3P
 S APCLGOT1=1
 I $D(DOD),DOD'>APCLFYE Q
 S ^("LIVREG")=$S($D(^XTMP("APCLOS",APCLJOB,APCLBTH,"LIVREG")):(+^("LIVREG")+1),1:1)
 Q
 ;
 ;
LR3P ;
 Q:$P(^AUPNPAT(DFN,0),U,2)>APCLPYE
 I $D(DOD),DOD'>APCLPYE Q
 S ^("LIVREG")=$S($D(^XTMP("APCLOSP",APCLJOB,APCLBTH,"LIVREG")):(+^("LIVREG")+1),1:1)
 Q
BIRTHS ;
 S DOB=$P(^DPT(DFN,0),U,3)
 Q:DOB=""
 I DOB'<APCLFYB,DOB'>APCLFYE S ^("BIRTHS")=$S($D(^XTMP("APCLOS",APCLJOB,APCLBTH,"BIRTHS")):(+^("BIRTHS")+1),1:1)
 I DOB'<APCLPYB,DOB'>APCLPYE S ^("BIRTHS")=$S($D(^XTMP("APCLOSP",APCLJOB,APCLBTH,"BIRTHS")):(+^("BIRTHS")+1),1:1)
 Q
DEATHS ;
 Q:'$D(DOD)
 I DOD'<APCLFYB,DOD'>APCLFYE S ^("DEATHS")=$S($D(^XTMP("APCLOS",APCLJOB,APCLBTH,"DEATHS")):(+^("DEATHS")+1),1:1)
 I DOD'<APCLPYB,DOD'>APCLPYE S ^("DEATHS")=$S($D(^XTMP("APCLOSP",APCLJOB,APCLBTH,"DEATHS")):(+^("DEATHS")+1),1:1)
 Q
NEWREG ;
 S APCLOS="APCLOS" S APCLBDAT=APCLFYB,APCLEDAT=APCLFYE D NEWREG1
 S APCLOS="APCLOSP" S APCLBDAT=APCLPYB,APCLEDAT=APCLPYE D NEWREG1
 Q
NEWREG1 ;
 I $P(^AUPNPAT(DFN,0),U,2)<APCLBDAT!($P(^AUPNPAT(DFN,0),U,2)>APCLEDAT) Q
 S ^("NEWREG")=$S($D(^XTMP(APCLOS,APCLJOB,APCLBTH,"NEWREG")):(+^("NEWREG")+1),1:1)
 ;
 ;
 Q
