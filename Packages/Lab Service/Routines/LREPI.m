LREPI ;VA/DALOI/SED-EMERGING PATHOGENS SEARCH ;5/1/98
 ;;5.2;LAB SERVICE;**1030**;NOV 01, 1997
 ;;5.2;LAB SERVICE;**132,175,260,281**;Sep 27, 1994
 ; Reference to ^DGPT supported by IA #418
 ; Reference to ^ICD9 supported by IA #10082
 ; Reference to ^ORD(101 supported by IA #872
 ; Reference to PATS^PXRMXX supported by IA #3134
TEST S LRRPS=3000501,LRRPE=3000531,LRRTYPE=1
 S LREPI(2)="",LREPI(17)="",LREPI(18)="",LREPI(19)=""
 ;S D0=0 F  S D0=$O(^LAB(69.5,D0)) Q:+D0'>0  D
 ;.Q:$P(^LAB(69.5,D0,0),U,2)="1"
 ;.Q:$P(^LAB(69.5,D0,0),U,7)=""
 ;.Q:'$D(^ORD(101,$P(^LAB(69.5,D0,0),U,7),0))
 ;.S LREPI(D0)=""
 S LRBEG=9999999-(LRRPE+.9),LREND=9999999-LRRPS+.999999
EN ;
 ;
INIT ;Set up search criteria
 ;Fix start and stop date problem CKA 6/2/2002
 S LRBEG=(9999999-LRRPE)_".0000001",LREND=9999999-LRRPS+.999999
 K ^TMP($J),^TMP("HLS",$J)
 S D0=0 F  S D0=$O(LREPI(D0)) Q:+D0'>0  D
 .S ^TMP($J,$P(^LAB(69.5,D0,0),U,7))=""
 .S:$P(^LAB(69.5,D0,0),U,8)=1 ^TMP($J,"LREPI",D0)=""
 .S LRPROT=$P(^LAB(69.5,D0,0),U,7)
 .Q:LRPROT=""
 .S D1=0 F  S D1=$O(^LAB(69.5,D0,1,D1)) Q:+D1'>0  D
 ..S TST=$P(^LAB(69.5,D0,1,D1,0),U)
 ..Q:'$D(^LAB(60,TST,0))
 ..Q:$P(^LAB(60,TST,0),U,4)=""
 ..I $P(^LAB(60,TST,0),U,4)="CH" D
 ...Q:$P(^LAB(60,TST,0),U,5)=""
 ...S ^TMP($J,"T",TST,D0)=""
 ...S ^TMP($J,"TPROT",TST,LRPROT)=""
 ...S LRIND=$P(^LAB(69.5,D0,1,D1,0),U,2,3)
 ...S ^TMP($J,$P(^LAB(60,TST,0),U,4),TST)=$P(^LAB(60,TST,0),U,5)_U_LRIND
 ..I $P(^LAB(60,TST,0),U,4)="CY" D
 ...S ^TMP($J,"T",TST,D0)=""
 ...S ^TMP($J,$P(^LAB(60,TST,0),U,4),TST)=""
 .S D1=0 F  S D1=$O(^LAB(69.5,D0,2,D1)) Q:+D1'>0  S ^TMP($J,"E",$P(^LAB(69.5,D0,2,D1,0),U),D0)=""
 .S D1=0 F  S D1=$O(^LAB(69.5,D0,9,D1)) Q:+D1'>0  S ^TMP($J,"SNO",$P(^LAB(69.5,D0,9,D1,0),U),D0)=""
 .S D1=0 F  S D1=$O(^LAB(69.5,D0,3,D1)) Q:+D1'>0  S ^TMP($J,"ICD",$P(^LAB(69.5,D0,3,D1,0),U),D0)=""
 K D0,D1,TST,LRIND
 I $D(^TMP($J,"LREPI")) D SEARCH^LREPI4
 I $D(^TMP($J,"ICD")) D PTF^LREPI5
LAB63 ;Search file 63 for lab data
 K LRIND
 S LRDFN=0 F  S LRDFN=$O(^LR(LRDFN)) Q:+LRDFN'>0  D
 .Q:'$D(^LR(LRDFN,0))
 .Q:$P(^LR(LRDFN,0),U,2)'=2
 .S LRPAT=$P(^LR(LRDFN,0),U,3)
 .I $D(^TMP($J,"CH")) D CH
 .I $D(^TMP($J,"CY")) D CYTST^LREPICY
 .I $D(^TMP($J,"E")) D MI
 .;I '$D(^TMP($J,"ICD"))&($D(^TMP($J,"SNO"))) D CY^LREPICY
 .I $D(^TMP($J,"SNO")) D CY^LREPICY
 ;Retrieve patient list from Clinical Reminders
 S LRPROTX=$O(^ORD(101,"B","LREPI",""))
 I LRPROTX]"" S LRSRXX="",LRSRGO=0 F  S LRSRXX=$O(LREPI(LRSRXX)) Q:'LRSRXX  I $G(^LAB(69.5,LRSRXX,0))["HEPATITIS" D  Q
  . D PATS^PXRMXX(LRRPS,LRRPE,"LREPISRCH")
  . S EPISRCH=0 F  S EPISRCH=$O(^TMP("LREPISRCH",$J,EPISRCH)) Q:'EPISRCH  D
  . . S LRENCDT=$P(^TMP("LREPISRCH",$J,EPISRCH),"^") Q:'LRENCDT
  . . Q:$D(^TMP($J,LRPROTX,EPISRCH,LRENCDT))  ;Encounter date already exists, don't update
  . . S ^TMP($J,LRPROTX,EPISRCH,LRENCDT)=$P(^TMP("LREPISRCH",$J,EPISRCH),"^",2)
 I $G(LRREP) D ^LREPI2A
 I '$G(LRREP) D ^LREPI2
EXIT ;EXIT
 S D0=0
 I $G(LRRTYPE)=0 F  S D0=$O(LREPI(D0)) Q:+D0'>0  D
 .S $P(^LAB(69.5,D0,0),U,4)=DT
 K LREPI,DFN,CNT,DA,DIE,DR,DQ,HL,ENTRY,ENDT,ENC,FD,HLECH,HLFS,HLN,HLQ
 K DDER,D0,HLRST,HLSAN,LRBEG,LRCNT,LRCS,LRDATE,LRDFN,LREFG,LRENCDT
 K LREND,LRETND,LRHL7,LRINV,LRINVD,LRITN,LRND,LRNL,LRNLT,LRNTE,LROBR
 K LRPAT,LRPFG,LRPID,LRPROT,LRPV1,LRRPE,LRRPS,LRRTYPE,LRTND,LRTNM,MSG
 K MSGCNT,PTF,RR,SEG,SP,STDT,TST,UN,TSTNM,VAERR,X,XCNP,XMDUZ,XMZ,ZTSK
 K AF,D,DI,LRENT,LRIND,LRPATH,OV,LRENDT,ADMDT,EPISITE,EPISRCH
 K LR31799Z,LRANTI,LRCHK,LRIC,LRIEN,LRIPT,LRMG,LRMGN,LRNX,LRO,LROK
 K LROVR,LRPCNT,LRPTOT,LRSI,LRSITE,LRCYSP,LRDIS,LRDISI,LRIC,LRICD
 K LRICDI,LRIEN,LRIPT,LRMG,LRMGN,LRMOR,LRMORI,LRMSG,PXRMITEM
 K LRSNM,LRSNO,LRSTOP,LRSUB,LRTOP,LRTOPP,LRWKI,LRPRO,LRPROI
 K LRNDC,LRNTE1,LRFIND,LRDRUG,LRCODE,LRDRSEQ,HLHDR,HLMTIEN,HLMTIENS
 K HLNEXT,HLNODE,HLQUIT,HLRESLT,HLRESLTA,LRANS,LRDRSQ1,LRPROTX,LRPTY
 K LRPVVV,LRSRGO,LRSRXX,LRTOLD,LRTRM,LRPREV,LRPRECYC,X1,X2,X3
 K LRANTIND,LRANTINV,LRREP,LRPV1NUM
 Q
ENCT ;SET THE ENCOUNTER FOR PV1
 S LRPROT=$P(^LAB(69.5,LRPATH,0),U,7)
 S LRCHK=0 D ADDCHK^LREPI5 Q:LRCHK
 S LRDATE=9999999-LRINV
 K VAIN,DFN,VAINDT S DFN=LRPAT,VAINDT=LRDATE D INP^VADPT
 S LRENCDT=$S(VAIN(7)'="":$P(VAIN(7),U),1:LRDATE)
 I $P(^LAB(69.5,LRPATH,0),U,8)=1 D CHECK^LREPI4
 S:'$D(^TMP($J,LRPROT,LRPAT,LRENCDT)) ^TMP($J,LRPROT,LRPAT,LRENCDT)=$S(VAIN(7)'="":"I",1:"O")_U_$G(VAIN(10))
 S:$P(^TMP($J,LRPROT,LRPAT,LRENCDT),U)="O" ^(LRENCDT)="O"_U_$S($D(LRPATLOC):LRPATLOC,1:"")
 S:'$D(^TMP($J,LRPROT,LRPAT,LRENCDT,LRPATH,LRINV,ND)) ^TMP($J,LRPROT,LRPAT,LRENCDT,LRPATH,LRINV,ND)=""
 I $G(LRANTIND)="",$G(LRANTINV)="" Q
 S:'$D(^TMP($J,LRPROT,LRPATH,LRENCDT,LRPAT,LRINV,ND,LRANTIND,LRANTINV)) ^TMP($J,LRPROT,LRPAT,LRENCDT,LRPATH,LRINV,ND,LRANTIND,LRANTINV)=""
 Q
CH ;Check the 'CH' node
 S LRINV=LRBEG
 F  S LRINV=$O(^LR(LRDFN,"CH",LRINV)) Q:+LRINV'>0!(LRINV>LREND)  D
 .Q:$P(^LR(LRDFN,"CH",LRINV,0),U,3)=""
 .S LRCNT=1,LRTST=0 F  S LRTST=$O(^TMP($J,"CH",LRTST)) Q:+LRTST'>0  D
 ..S LRND=$P($P(^TMP($J,"CH",LRTST),";",2),U,1) Q:+LRND'>0
 ..S LRPC=$P($P(^TMP($J,"CH",LRTST),";",3),U,1) Q:+LRPC'>0
 ..S LRRES=$P($G(^LR(LRDFN,"CH",LRINV,LRND)),U,LRPC) Q:LRRES=""
 ..S LRPATLOC=$P(^LR(LRDFN,"CH",LRINV,0),U,13)
 ..S ^TMP($J,"TST",LRTST)=+$G(^TMP($J,"TST",LRTST))+1
 ..S ^TMP($J,"TST",LRTST,LRDFN)=""
 ..S LRPATH=0 F  S LRPATH=$O(^TMP($J,"T",LRTST,LRPATH)) Q:+LRPATH'>0  D CHKIND
 K LRTST,LRND,LRPC,LRRES,LRNO
 Q
CHKIND ;Check the results
 I '$D(^LAB(69.5,LRPATH,1,"B",LRTST)) Q
 S LRITST=0,ND="CH",LRNO=0
 F  S LRITST=$O(^LAB(69.5,LRPATH,1,"B",LRTST,LRITST)) Q:+LRITST'>0  D  D:'LRNO ENCT
 .S LRNO=0
 .S LRIND=$P(^LAB(69.5,LRPATH,1,LRITST,0),U,2,3)
 .Q:$P(LRIND,U,1)=""
 .I $P(LRIND,U,1)=1 D  Q
 ..Q:'LRRES#2
 ..S LRSPEC=$P($G(^LR(LRDFN,"CH",LRINV,0)),U,5) Q:LRSPEC=""
 ..Q:'$D(^LAB(60,LRTST,1,LRSPEC,0))
 ..S LRLOW=$P(^LAB(60,LRTST,1,LRSPEC,0),U,2),LRHIG=$P(^(0),U,3)
 ..Q:'LRLOW#2!('LRHIG#2)
 ..I LRRES<LRLOW!(LRRES>LRHIG) Q
 ..S LRNO=1
 .I $P(LRIND,U,2)="" Q
 .S LRRES=$$UP^XLFSTR(LRRES),LRIND=$$UP^XLFSTR(LRIND)
 .I $P(LRIND,U,1)=2,(LRRES[$P(LRIND,U,2)) Q
 .I $P(LRIND,U,1)=3,(LRRES>$P(LRIND,U,2)) Q
 .I $P(LRIND,U,1)=4,(LRRES<$P(LRIND,U,2)) Q
 .I $P(LRIND,U,1)=5,(LRRES=$P(LRIND,U,2)) Q
 .S LRNO=1
 K LRITST,LRLOW,LRHIG,LRSPEC
 Q
MI ;Check the 'MI' node
 S LRINV=LRBEG
 F  S LRINV=$O(^LR(LRDFN,"MI",LRINV)) Q:+LRINV'>0!(LRINV>LREND)  D
 .S LRCNT=1
 .F LRMIND=3,6,9,12,17 S LRETND=0 F  S LRETND=$O(^LR(LRDFN,"MI",LRINV,LRMIND,LRETND)) Q:+LRETND'>0  D
 ..I LRMIND=3,$P($G(^LR(LRDFN,"MI",LRINV,1)),U,2)'="F" Q
 ..I LRMIND'=3,$P($G(^LR(LRDFN,"MI",LRINV,(LRMIND-1))),U,2)'="F" Q
 ..S LRETI=$P($G(^LR(LRDFN,"MI",LRINV,LRMIND,LRETND,0)),U)
 ..Q:+LRETI'>0
 ..Q:'$D(^TMP($J,"E",LRETI))
 ..S ^TMP($J,"EPROT",LRETI)=""
 ..S ^TMP($J,"ETI",LRETI)=+$G(^TMP($J,"ETI",LRETI))+1
 ..S ^TMP($J,"ETI",LRETI,LRDFN)=""
 ..S LRPATH=0 F  S LRPATH=$O(^TMP($J,"E",LRETI,LRPATH)) Q:+LRPATH'>0  D
 ...S ND="MI"
 ...D TOP Q:LRTOP
 ...I LRMIND=3 D ANTI Q
 ...D ENCT
 K LRMIND,LRETI
 Q
TOP ;CHECK TO SEE IF SCREEN ON SITE
 S LRTOP=0
 S LRSITE=$P($G(^LR(LRDFN,"MI",LRINV,0)),U,5) Q:+LRSITE'>0
 I ($O(^LAB(69.5,LRPATH,5,0))="")&($O(^LAB(69.5,LRPATH,6,0))="") Q
 I ($O(^LAB(69.5,LRPATH,5,0))'="")&($O(^LAB(69.5,LRPATH,6,0))'="") Q
 I ($O(^LAB(69.5,LRPATH,5,0))'="")&($D(^LAB(69.5,LRPATH,5,"B",LRSITE))) Q
 I ($O(^LAB(69.5,LRPATH,6,0))'="")&('$D(^LAB(69.5,LRPATH,6,"B",LRSITE))) Q
 S LRTOP=1
 Q
ANTI ;LOOK FOR THE ANTIMICROBIAL SUS FOR ORGANISMS
 I $O(^LAB(69.5,LRPATH,4,0))="" D ENCT Q
 S LRANTI=0 F  S LRANTI=$O(^LAB(69.5,LRPATH,4,LRANTI)) Q:+LRANTI'>0  D
 .S LRANT=$G(^LAB(69.5,LRPATH,4,LRANTI,0),U),LRANTIND=$P(^(0),U,2),LRANTINV=$P(^(0),U,3) Q:+LRANT'>0
 .S LRAND=$P($G(^LAB(62.06,LRANT,0)),U,2) Q:LRAND=""
 .Q:'$D(^LR(LRDFN,"MI",LRINV,LRMIND,LRETND,LRAND))
 .Q:$P(^LR(LRDFN,"MI",LRINV,LRMIND,LRETND,LRAND),U,2)=""
 .Q:$$UP^XLFSTR($E($P($G(^LR(LRDFN,"MI",LRINV,LRMIND,LRETND,LRAND)),U,2),1,1))="S"
 .D ENCT
 .;CHECK MIC VALUES
 .I LRANTIND=""!(LRANTINV="") Q
 .S LRRES=$$UP^XLFSTR($E($P($G(^LR(LRDFN,"MI",LRINV,LRMIND,LRETND,LRAND)),U,2),1,1)),LRANTINV=$$UP^XLFSTR(LRANTINV),LRANTIND=$$UP^XLFSTR(LRANTIND)
 .I LRANTIND=1,(LRRES[LRANTINV) D ENCT Q
 .I LRANTIND=2,(LRRES>LRANTINV) D ENCT Q
 .I LRANTIND=3,(LRRES<LRANTINV) D ENCT Q
 .I LRANTIND=4,(LRRES=LRANTINV) D ENCT Q
 Q
 ;
