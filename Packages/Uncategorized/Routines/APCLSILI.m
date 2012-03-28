APCLSILI ; IHS/CMI/LAB - ILI surveillance export ; 
 ;;3.0;IHS PCC REPORTS;**22,23,24,25,26,27**;FEB 05, 1997
 ;
 ;
START ;
 ;This report will create a comma delimited output file of all visits on from March 21, 2009
 ;through the date run for which the visit meets this criteria
 ;     clinic is in SURVEILLANCE ILI CLINICS taxonomy
 ;     at least 1 POV is in SURVEILLANCE ILI taxonomy
 ;     visit is AMBULATORY, OBSERVATION, DAY SURGERY OR NURSING HOME (outpatient in service category)
 ;after the file is generated it will call sendto with ZISH SEND PARAMETER SURVEILLANCE ILI SEND
 ;
 D EN^XBVK("APCL")  ;kill all apcl variables
 S D=""
 S X=$O(^APCLCNTL("B","ILI STOP DATE",0))
 I X,$P(^APCLCNTL(X,0),U,3) S D=$P(^APCLCNTL(X,0),U,3)   ;if there is a stop date then use it
 I D,DT>D Q
 ;
 D EXIT
 ;
 ;
PROC ;EP - called from xbdbque
 S APCL1ST=""
 I $E(DT,6,7)="01" S APCL1ST=1
 ;I $P($G(^APCLILIC(1,0)),U,2) S APCL1ST=1  ;need to send data first time through after the patch is installed
 ;if this is between the 15th and 27th then check to see if the user pop export has been run
 ;if it has run since the 1st of the month then run it.
 ;get the 1st of this month
 S APCL1OM=$E(DT,1,5)_"01"
 ;get date last one finished.
 S (X,L)="" F  S X=$O(^APCLILIC(1,11,"B",X)) Q:X'=+X  S L=X
 I 'L S APCL1ST=1  ;hasn't run before after patch 27 so run it
 I 'APCL1ST,$E(DT,6,7)>14,$E(DT,6,7)<27,L<APCL1OM S APCL1ST=1  ;if last time run was before the 1st then run it
 K APCLLOCT
 K ^APCLDATA($J)  ;export global
 S APCLCTAX=$O(^ATXAX("B","SURVEILLANCE ILI CLINICS",0))  ;clinic taxonomy
 S APCLDTAX=$O(^ATXAX("B","SURVEILLANCE ILI",0))  ;dx taxonomy
 I 'APCLCTAX D EXIT Q
 I 'APCLDTAX D EXIT Q
 ;
 I $P($G(^APCLILIC(1,0)),U,2) S APCLSD=3090320.9999,$P(^APCLILIC(1,0),U,2)="" I 1
 E  S APCLSD=$$FMADD^XLFDT(DT,-91)_".9999"  ;start with 3/21/09 visits CHANGED TO 90 DAYS IN PATCH 26, AFTER THE FIRST PATCH 26 EXPORT
 S APCLED=$$FMADD^XLFDT(DT,-1)
 S APCLVTOT=0  ;visit counter
 F  S APCLSD=$O(^AUPNVSIT("B",APCLSD)) Q:APCLSD'=+APCLSD!($P(APCLSD,".")>APCLED)  D
 .S APCLV=0 F  S APCLV=$O(^AUPNVSIT("B",APCLSD,APCLV)) Q:APCLV'=+APCLV  D
 ..Q:'$D(^AUPNVSIT(APCLV,0))  ;no zero node
 ..Q:$P(^AUPNVSIT(APCLV,0),U,11)  ;deleted visit
 ..S DFN=$P(^AUPNVSIT(APCLV,0),U,5)
 ..Q:DFN=""
 ..Q:'$D(^DPT(DFN,0))
 ..Q:$P(^DPT(DFN,0),U)["DEMO,PATIENT"
 ..Q:$$DEMO^APCLUTL(DFN,"E")
 ..S G=0,X=0 F  S X=$O(^BGPSITE(X)) Q:X'=+X  I $P($G(^BGPSITE(X,0)),U,12) I $D(^DIBT($P(^BGPSITE(X,0),U,12),1,DFN)) S G=1
 ..Q:G
 ..S APCLKV=0,APCLH1N1=0,(APCLILI,APCLHVAC,APCLIVAC,APCLADVE,APCLSRD,APCLAVM,APCLAV9)=""
 ..S APCLLOC=$P(^AUPNVSIT(APCLV,0),U,6)  Q:APCLLOC=""  ;no location ???
 ..S APCLDATE=$P($P(^AUPNVSIT(APCLV,0),U),".")
 ..S APCLASUF=$P($G(^AUTTLOC(APCLLOC,0)),U,10)
 ..I APCLASUF="" Q  ;no ASUFAC????
 ..;keep visit?
 ..S G=0 D ILIDX I G S APCLKV=1,APCLILI=G
 ..S G=0 D H1N1DX I G S APCLKV=1,APCLH1N1=G
 ..;S APCLHVAC=$$HASVAC(APCLV) I APCLHVAC S APCLKV=1  ;H1N1 vaccine taken out per Amy Groom patch 27
 ..S APCLIVAC=$$HASIVAC(APCLV) I APCLIVAC S APCLKV=1
 ..S APCLADVE=$$HASADVN6^APCLSIL1(APCLV) I APCLADVE S APCLKV=1  ;this was changed in patch 27
 ..S APCLOVAC="" I APCLADVE S APCLOVAC=$$OTHVAC^APCLSIL1(DFN,APCLDATE)
 ..S APCLSRD=$$HASSRD7(APCLV) I APCLSRD S APCLKV=1
 ..S APCLAVM=$$HASAVM(APCLV) I APCLAVM S APCLKV=1
 ..;S APCLAV9=$$HASAV9(APCLV) I APCLAV9 S APCLKV=1
 ..I 'APCLKV Q  ;not a visit to export
 ..D SETREC^APCLSIL2  ;set record
 ;NOW SET TOTAL IN PIECE 13
 S X=0 F  S X=$O(^APCLDATA($J,X)) Q:X'=+X  D
 .I $P(^APCLDATA($J,X),",",8)="" Q  ;not an ILI visit
 .Q:$P(^APCLDATA($J,X),",",15)="H"  ;not ambulatory
 .S L=$P(^APCLDATA($J,X),",",6),D=$P(^APCLDATA($J,X),",",7)
 .S $P(^APCLDATA($J,X),",",13)=$G(APCLLOCT(L,D))
 .Q
 ;NOW SET TOTAL IN PIECE 20
 S X=0 F  S X=$O(^APCLDATA($J,X)) Q:X'=+X  D
 .Q:$P(^APCLDATA($J,X),",",15)'="H"
 .I $P(^APCLDATA($J,X),",",8)="",$P(^APCLDATA($J,X),U,43)=""  ;not an ILI or H1N1 visit
 .S L=$P(^APCLDATA($J,X),",",6),D=$P(^APCLDATA($J,X),",",7)
 .S $P(^APCLDATA($J,X),",",20)=$G(APCLHTOT(L,D))
 .Q
 ;NOW SET TOTAL IN PIECE 42
 S X=0 F  S X=$O(^APCLDATA($J,X)) Q:X'=+X  D
 .Q:$P(^APCLDATA($J,X),",",15)="H"
 .I $P(^APCLDATA($J,X),",",43)="" Q    ;not an H1N1/ili visit
 .S L=$P(^APCLDATA($J,X),",",6),D=$P(^APCLDATA($J,X),",",7)
 .S $P(^APCLDATA($J,X),",",42)=$G(APCLALLT(L,D))
 .Q
 D WRITE
MONUP ;monthly user pop if today is the 1st
 I $G(APCL1ST) D
 .;create entry with start date of DT
 . N APCLFDA,APCLIENS,APCLERR
 . S APCLIENS="+2,"_1_","
 . S APCLFDA(9001003.311,APCLIENS,.01)=DT
 . D UPDATE^DIE("","APCLFDA","APCLIENS","APCLERR(1)")
 . I $D(APCLERR) S APCLER="0~Add Education Topic" Q
 . S APCLEIEN=$G(APCLIENS(2))
 . D MONUP^APCLSIL1  ;S $P(^APCLILC(1,0),U,3)=DT,$P(^APCLILC(1,0),U,2)="" D
 . ;ADD END DATE TO MULTIPLE
 . S $P(^APCLILIC(1,11,APCLEIEN,0),U,2)=DT
 D PURGE
 D EXIT
 Q
HASVAC(V) ;EP - get h1n1 vaccine
 NEW C,X,Y,Z,T,L,M
 S T=$O(^ATXAX("B","SURVEILLANCE H1N1 CVX CODES",0))
 S C=0,X=0 F  S X=$O(^AUPNVIMM("AD",V,X)) Q:X'=+X!(C)  S Y=$P($G(^AUPNVIMM(X,0)),U) D
 .Q:'Y
 .Q:'$D(^AUTTIMM(Y,0))
 .S Z=$P(^AUTTIMM(Y,0),U,3)
 .Q:'$D(^ATXAX(T,21,"B",Z))
 .S C=1_U_Z_U_$$VAL^XBDIQ1(9000010.11,X,.05) I $P(^AUPNVIMM(X,0),U,5) S C=C_U_$$VAL^XBDIQ1(9999999.41,$P(^AUPNVIMM(X,0),U,5),.02)
 .Q
 I C Q C
 S T=$O(^ATXAX("B","SURVEILLANCE CPT H1N1",0))
 S C=0,X=0 F  S X=$O(^AUPNVCPT("AD",V,X)) Q:X'=+X  S Y=$P($G(^AUPNVCPT(X,0)),U) D
 .Q:'Y
 .Q:'$$ICD^ATXCHK(Y,T,1)
 .S C=1_U_$$VAL^XBDIQ1(9000010.18,X,.01)
 Q C
HASH1N1V(P,ED) ;EP
 NEW C,X,Y,Z,T,L,M,D
 S T=$O(^ATXAX("B","SURVEILLANCE H1N1 CVX CODES",0))
 S C=0,X=0 F  S X=$O(^AUPNVIMM("AC",P,X)) Q:X'=+X!(C)  S Y=$P($G(^AUPNVIMM(X,0)),U) D
 .Q:'Y
 .Q:'$D(^AUTTIMM(Y,0))
 .S Z=$P(^AUTTIMM(Y,0),U,3)
 .Q:'$D(^ATXAX(T,21,"B",Z))
 .S D=$$VD^APCLV($P(^AUPNVIMM(X,0),U,3))
 .Q:D>ED
 .S C=1
 .Q
 I C Q C
 S C=$$LASTCPTT^APCLAPIU(P,$$DOB^AUPNPAT(P),ED,"SURVEILLANCE CPT H1N1","D")
 I C Q 1
 Q ""
HASIVAC(V) ;EP - get flu iz
 NEW C,X,Y,Z,T
 S T=$O(^ATXAX("B","BGP FLU IZ CVX CODES",0))
 S C=0,X=0 F  S X=$O(^AUPNVIMM("AD",V,X)) Q:X'=+X  S Y=$P($G(^AUPNVIMM(X,0)),U) D
 .Q:'Y
 .Q:'$D(^AUTTIMM(Y,0))
 .S Z=$P(^AUTTIMM(Y,0),U,3)
 .Q:'$D(^ATXAX(T,21,"B",Z))
 .;get lot and manufacturer added in patch 27
 .S C=1_U_Z_U_$$VAL^XBDIQ1(9000010.11,X,.05) I $P(^AUPNVIMM(X,0),U,5) S C=C_U_$$VAL^XBDIQ1(9999999.41,$P(^AUPNVIMM(X,0),U,5),.02)
 .Q
 I C Q C
 S T=$O(^ATXAX("B","BGP CPT FLU",0))
 S C=0,X=0 F  S X=$O(^AUPNVCPT("AD",V,X)) Q:X'=+X  S Y=$P($G(^AUPNVCPT(X,0)),U) D
 .Q:'Y
 .Q:'$$ICD^ATXCHK(Y,T,1)
 .S C=1_U_$$VAL^XBDIQ1(9000010.18,X,.01)
 I C Q C
 S T=$O(^ATXAX("B","BGP FLU IZ PROCEDURES",0))
 S C=0,X=0 F  S X=$O(^AUPNVPRC("AD",V,X)) Q:X'=+X  S Y=$P($G(^AUPNVPRC(X,0)),U) D
 .Q:'Y
 .Q:'$$ICD^ATXCHK(Y,T,0)
 .S C=1_U_$$VAL^XBDIQ1(9000010.08,X,.01)
 I C Q C
 S T=$O(^ATXAX("B","BGP FLU IZ DXS",0))
 S C=0,X=0 F  S X=$O(^AUPNVPOV("AD",V,X)) Q:X'=+X  S Y=$P($G(^AUPNVPOV(X,0)),U) D
 .Q:'Y
 .Q:'$$ICD^ATXCHK(Y,T,9)
 .S C=1_U_$$VAL^XBDIQ1(9000010.07,X,.01)
 I C Q C
 Q C
ILIDX ;
 Q:"AORSH"'[$P(^AUPNVSIT(APCLV,0),U,7)
 I $P(^AUPNVSIT(APCLV,0),U,7)="H" S APCLHTOT(APCLASUF,$$JDATE(APCLDATE))=$G(APCLHTOT(APCLASUF,$$JDATE(APCLDATE)))+1
 S APCLCLIN=$$CLINIC^APCLV(APCLV,"I")  ;get clinic code
 ;is there a PHN
 S X=0,P=0 F  S X=$O(^AUPNVPRV("AD",APCLV,X)) Q:X'=+X!(P)  D
 .Q:'$D(^AUPNVPRV(X,0))
 .S Y=$P(^AUPNVPRV(X,0),U)
 .S Z=$$VALI^XBDIQ1(200,Y,53.5)
 .Q:'Z
 .I $P($G(^DIC(7,Z,9999999)),U,1)=13 S P=1
 I P G ILIDX1
 I $P(^AUPNVSIT(APCLV,0),U,7)'="H" Q:APCLCLIN=""
 I $P(^AUPNVSIT(APCLV,0),U,7)'="H" Q:'$D(^ATXAX(APCLCTAX,21,"B",APCLCLIN))  ;not in clinic taxonomy
ILIDX1 ;
 I $P(^AUPNVSIT(APCLV,0),U,7)'="H" S APCLLOCT(APCLASUF,$$JDATE(APCLDATE))=$G(APCLLOCT(APCLASUF,$$JDATE(APCLDATE)))+1   ;total number of visits
 S C=0
 K G,Y S G=""
 S X=0 F  S X=$O(^AUPNVPOV("AD",APCLV,X)) Q:X'=+X  S T=$P(^AUPNVPOV(X,0),U) I $$ICD^ATXCHK(T,APCLDTAX,9) S C=C+1,Y(C)=$$VAL^XBDIQ1(9000010.07,X,.01)
 Q:'$D(Y)  ;no diagnosis
 S X=0 F  S X=$O(Y(X)) Q:X'=+X  S G=G_U_Y(X)
 S $P(G,U,1)=1
 Q
HASADV6(APCLV) ;
 NEW X,P,Y,Z,APCLCLIN,T,G,C,D
 I "AORSH"'[$P(^AUPNVSIT(APCLV,0),U,7) Q ""
 S APCLCLIN=$$CLINIC^APCLV(APCLV,"I")  ;get clinic code
 ;is there a PHN
 S X=0,P=0 F  S X=$O(^AUPNVPRV("AD",APCLV,X)) Q:X'=+X!(P)  D
 .Q:'$D(^AUPNVPRV(X,0))
 .S Y=$P(^AUPNVPRV(X,0),U)
 .S Z=$$VALI^XBDIQ1(200,Y,53.5)
 .Q:'Z
 .I $P($G(^DIC(7,Z,9999999)),U,1)=13 S P=1
 I P G HASADV61
 I $P(^AUPNVSIT(APCLV,0),U,7)'="H" I APCLCLIN="" Q ""
 I $P(^AUPNVSIT(APCLV,0),U,7)'="H" I '$D(^ATXAX(APCLCTAX,21,"B",APCLCLIN)) Q ""  ;not in clinic taxonomy
HASADV61 ;
 S G=0,D=""
 S X=0 F  S X=$O(^AUPNVPOV("AD",APCLV,X)) Q:X'=+X!(G)  S T=$P(^AUPNVPOV(X,0),U) I $$ICD^ATXCHK(T,$O(^ATXAX("B","SURVEILLANCE ADV EVENTS DXS",0)),9) S G=1,D=$$VAL^XBDIQ1(9000010.07,X,.01)
 I 'G Q ""  ;no diagnosis
 Q 1_U_D
 ;
HASAV9(APCLV) ;
 NEW X,P,Y,Z,APCLCLIN,T,G,C
 I "AORS"'[$P(^AUPNVSIT(APCLV,0),U,7) Q ""
 S APCLCLIN=$$CLINIC^APCLV(APCLV,"I")  ;get clinic code
 ;is there a PHN
 S X=0,P=0 F  S X=$O(^AUPNVPRV("AD",APCLV,X)) Q:X'=+X!(P)  D
 .Q:'$D(^AUPNVPRV(X,0))
 .S Y=$P(^AUPNVPRV(X,0),U)
 .S Z=$$VALI^XBDIQ1(200,Y,53.5)
 .Q:'Z
 .I $P($G(^DIC(7,Z,9999999)),U,1)=13 S P=1
 I P G HASAV91
 I APCLCLIN="" Q ""
 I '$D(^ATXAX(APCLCTAX,21,"B",APCLCLIN)) Q ""  ;not in clinic taxonomy
HASAV91 ;
 K Y
 S G=""
 I $$HASH1N1V($P(^AUPNVSIT(APCLV,0),U,5),$$FMADD^XLFDT($$VD^APCLV(APCLV),-1)) Q ""  ;patient had H1N1 prior to this
 S X=0,C=0 F  S X=$O(^AUPNVPOV("AD",APCLV,X)) Q:X'=+X  S T=$P(^AUPNVPOV(X,0),U) I $$ICD^ATXCHK(T,$O(^ATXAX("B","SURVEILLANCE ADV EV NO H1N1",0)),9) S C=C+1,Y(C)=$$VAL^XBDIQ1(9000010.07,X,.01)
 I '$D(Y) Q ""  ;no diagnosis
 S X=0,C=0 F  S X=$O(Y(X)) Q:X'=+X!(C>3)  S C=C+1,G=G_U_Y(X)
 S $P(G,U,1)=1
 Q G
H1N1DX ;
 Q:"AORSH"'[$P(^AUPNVSIT(APCLV,0),U,7)  ;just want outpatient with dx
 S APCLCLIN=$$CLINIC^APCLV(APCLV,"I")  ;get clinic code
 ;I $P(^AUPNVSIT(APCLV,0),U,7)'="H" Q:'$D(^ATXAX(APCLCTAX,21,"B",APCLCLIN))  ;not in clinic taxonomy
 I $P(^AUPNVSIT(APCLV,0),U,7)'="H" S APCLALLT(APCLASUF,$$JDATE(APCLDATE))=$G(APCLALLT(APCLASUF,$$JDATE(APCLDATE)))+1   ;total number of visits
 S G=0
 S X=0 F  S X=$O(^AUPNVPOV("AD",APCLV,X)) Q:X'=+X!(G)  S T=$P(^AUPNVPOV(X,0),U) I $$ICD^ATXCHK(T,$O(^ATXAX("B","SURVEILLANCE H1N1 DX",0)),9) S G=1,D=$$VAL^XBDIQ1(9000010.07,X,.01)
 Q:'G  ;no diagnosis
 S G=1_U_D
 Q
HASSRD7(APCLV) ;EP
 NEW X,P,D,Y,Z,APCLCLIN,T,G,C
 I $P(^AUPNVSIT(APCLV,0),U,7)'="H" Q ""  ;just want hOSP
 S C=0
 K G,Y S G=""
 S X=0 F  S X=$O(^AUPNVPOV("AD",APCLV,X)) Q:X'=+X  S T=$P(^AUPNVPOV(X,0),U) I $$ICD^ATXCHK(T,$O(^ATXAX("B","SURVEILLANCE SEV RESP DIS DXS",0)),9) S C=C+1,Y(C)=$$VAL^XBDIQ1(9000010.07,X,.01)
 I '$D(Y) Q ""  ;no diagnosis
 S X=0 F  S X=$O(Y(X)) Q:X'=+X  S G=G_U_Y(X)
 S $P(G,U,1)=1
 Q G
HASAVM(V) ;EP
 NEW C,X,Y,Z,T,L,M,N
 S T=$O(^ATXAX("B","FLU ANTIVIRAL MEDS",0))
 S C="",X=0 F  S X=$O(^AUPNVMED("AD",V,X)) Q:X'=+X!(C)  S Y=$P($G(^AUPNVMED(X,0)),U) D
 .Q:'Y
 .Q:'$D(^PSDRUG(Y,0))
 .S Z=0
 .S N=$P(^PSDRUG(Y,0),U)
 .I $D(^ATXAX(T,21,"B",Y)) S Z=1
 .I N["OSELTAMIVIR" S Z=1
 .I N["ZANAMIVIR" S Z=1
 .I Z=1 S C=1_U_N_U_$P(^AUPNVMED(X,0),U,7)
 .Q
 Q C
WRITE ; use XBGSAVE to save the temp global (APCLDATA) to a delimited
 ; file that is exported to the IE system
 N XBGL,XBQ,XBQTO,XBNAR,XBMED,XBFLT,XBUF,XBFN
 S XBGL="APCLDATA",XBMED="F",XBQ="N",XBFLT=1,XBF=$J,XBE=$J
 S XBNAR="ILI SURVEILLANCE EXPORT"
 S APCLASU=$P($G(^AUTTLOC($P(^AUTTSITE(1,0),U),0)),U,10)  ;asufac for file name
 S XBFN="FLU_"_APCLASU_"_"_$$DATE(DT)_".txt"
 S XBS1="SURVEILLANCE ILI SEND"
 ;
 D ^XBGSAVE
 ;
 I XBFLG'=0 D
 . I XBFLG(1)="" W:'$D(ZTQUEUED) !!,"VISIT ILI file successfully created",!!
 . I XBFLG(1)]"" W:'$D(ZTQUEUED) !!,"VISIT ILI file NOT successfully created",!!
 . W:'$D(ZTQUEUED) !,"File was NOT successfully transferred to IHS/CDC",!,"you will need to manually ftp it.",!
 . W:'$D(ZTQUEUED) !,XBFLG(1),!!
 K ^APCLDATA($J)
 Q
 ;
DATE(D) ;EP
 Q (1700+$E(D,1,3))_$E(D,4,5)_$E(D,6,7)
 ;
JDATE(D) ;EP - get date
 I $G(D)="" Q ""
 NEW A
 S A=$$FMTE^XLFDT(D)
 Q $E(D,6,7)_$$UP^XLFSTR($P(A," ",1))_(1700+$E(D,1,3))
 ;
UID(APCLA) ;Given DFN return unique patient record id.
 I '$G(APCLA) Q ""
 I '$D(^AUPNPAT(APCLA)) Q ""
 ;
 Q $$GET1^DIQ(9999999.06,$P(^AUTTSITE(1,0),U),.32)_$E("0000000000",1,10-$L(APCLA))_APCLA
 ;
EXIT ;clean up and exit
 D EN^XBVK("APCL")
 D ^XBFMK
 Q
 ;
EP ;EP - called from option to create search template using ILI logic
 G ^APCLSIL3
CTR(X,Y) ;EP - Center X in a field Y wide.
 Q $J("",$S($D(Y):Y,1:IOM)-$L(X)\2)_X
 ;----------
EOP ;EP - End of page.
 Q:$E(IOST)'="C"
 Q:$D(ZTQUEUED)!'(IOT["TRM")!$D(IO("S"))
 NEW DIR
 K DIRUT,DFOUT,DLOUT,DTOUT,DUOUT
 S DIR(0)="E" D ^DIR
 Q
 ;----------
USR() ;EP - Return name of current user from ^VA(200.
 Q $S($G(DUZ):$S($D(^VA(200,DUZ,0)):$P(^(0),U),1:"UNKNOWN"),1:"DUZ UNDEFINED OR 0")
 ;----------
LOC() ;EP - Return location name from file 4 based on DUZ(2).
 Q $S($G(DUZ(2)):$S($D(^DIC(4,DUZ(2),0)):$P(^(0),U),1:"UNKNOWN"),1:"DUZ(2) UNDEFINED OR 0")
 ;----------
STOPD ;EP - CALLED FROM OPTION
 W !!,"This option is used to set a stop date for sending ILI surveillance"
 W !,"visit data to IHS/CDC.  Currently the stop date is preset for "
 W !,"September 1, 2009."
 W !!,"When the nightly export runs, it will check this date. If the stop "
 W !,"date is less than the export date, no data will be exported."
 W !!,"For example, if you want to have the export of data stop on "
 W !,"August 1, 2009, then enter that date here.  If you want to continue"
 W !,"the export indefinitely, select a date in the future.",!
 S DA=$O(^APCLCNTL("B","ILI STOP DATE",0))
 I 'DA W !!,"ILI CONTROL FILE ENTRY MISSING.  NOTIFY PROGRAMMER." K DA Q
 S DIE="^APCLCNTL(",DR=".03"
 D ^DIE
 K DIE,DA
 W !!
 D PAUSE^APCLVL01
 Q
PURGE ;
 W:'$D(ZTQUEUED) !!,"Now cleaning up host files older than 7 DAYS"
 K APCLFILE,APCLDIR
 S APCLDIR=$P($G(^AUTTSITE(1,1)),"^",2)
 I APCLDIR="" S APCLDIR=$P($G(^XTV(8989.3,1,"DEV")),"^",1)
 I APCLDIR="" Q
 S APCLASU=$P($G(^AUTTLOC($P(^AUTTSITE(1,0),U),0)),U,10)
 S APCLDT=$$FMADD^XLFDT(DT,-7)
 S APCLDT=$$DATE(APCLDT)
 S APCLFLST=$$LIST^%ZISH(APCLDIR,"FLU_"_APCLASU_"*",.APCLFILE)
 Q:'$O(APCLFILE(""))
 S APCLX=0 F  S APCLX=$O(APCLFILE(APCLX)) Q:APCLX'=+APCLX  D
 .S D=$P($P(APCLFILE(APCLX),"."),"_",3)
 .I D<APCLDT S N=APCLFILE(APCLX) S APCLM=$$DEL^%ZISH(APCLDIR,N)
PURGEPOP ;
 K APCLFILE
 S APCLFLST=$$LIST^%ZISH(APCLDIR,"FLUPOP_"_APCLASU_"*",.APCLFILE)
 Q:'$O(APCLFILE(""))
 S APCLX=0 F  S APCLX=$O(APCLFILE(APCLX)) Q:APCLX'=+APCLX  D
 .S D=$P($P(APCLFILE(APCLX),"."),"_",3)
 .I D<APCLDT S N=APCLFILE(APCLX) S APCLM=$$DEL^%ZISH(APCLDIR,N)
 Q
