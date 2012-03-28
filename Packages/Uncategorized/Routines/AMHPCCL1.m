AMHPCCL1 ; IHS/CMI/LAB - CONTINUATION OF AMHPCCL ;
 ;;4.0;IHS BEHAVIORAL HEALTH;;MAY 14, 2010
 ;
 ;IHS/TUCSON/LAB - fixed passing of pov code patch 2 3/2/98
 ;
 ; for age edits and fixed the passing of multiple POVS to PCC
 ;
KILL ;
 K APCDALVR,APCDPAT,APCDLOC,APCDTYPE,APCDCAT,APCDCLN,APCDTPRO,APCDTPS,APCDTPOV,APCDTNQ,APCDTTOP,APCDTLOU,APCDTPRV,APCDTAT,APCDATMP,APCDAFLG,APCDAUTO,APCDANE,AUPNTALK,AMHPOVP,AMHICDP
 Q
AHPRV(V,P) ;EP is this provider already on the visit?
 NEW Y,Z,G
 S G=0
 S Y=0 F  S Y=$O(^AUPNVPRV("AD",V,Y)) Q:Y'=+Y!(G)  D
 .I $P($G(^AUPNVPRV(Y,0)),U)=P S G=1
 .Q
 Q G
VFILES ;EP Create v file entries
 S Y=$P(AMHR0,U,8) I Y D ^AUPNPAT
PROV ;
 S AMHX=0 F  S AMHX=$O(^AMHRPROV("AD",AMHR,AMHX)) Q:AMHX'=+AMHX  D
 .D KILL
 .Q:$$AHPRV(AMHVSIT,$P(^AMHRPROV(AMHX,0),U))
 .S APCDALVR("APCDVSIT")=AMHVSIT
 .S APCDALVR("APCDATMP")="[APCDALVR 9000010.06 (ADD)]"
 .I $P(^DD(9000010.06,.01,0),U,2)[6 S P=$P(^AMHRPROV(AMHX,0),U),A=$P(^DIC(3,P,0),U,16) D  K A,P Q:X=""
 ..S X=""
 ..I A="" D E9 Q
 ..I $P(^VA(200,P,0),U)'=$P(^DIC(16,A,0),U) D E9 Q
 ..S X=A
 .I $P(^DD(9000010.06,.01,0),U,2)[200 S X=$P(^AMHRPROV(AMHX,0),U)
 .I X]"" S APCDALVR("APCDTPRO")="`"_X
 .S APCDALVR("APCDPAT")=$P(AMHR0,U,8)
 .S APCDALVR("APCDTPS")=$P(^AMHRPROV(AMHX,0),U,4)
 .D ^APCDALVR
 .I $D(APCDALVR("APCDAFLG")) S AMHBN="VFILE",AMHVFILE="V PROVIDER" D @("E"_APCDALVR("APCDAFLG"))
 .Q
POV ;create V POVS
 S (AMHX,AMHGOT)=0 F  S AMHX=$O(^AMHRPRO("AD",AMHR,AMHX)) Q:AMHX'=+AMHX   D
 .I AMHGOT,AMHLPCCT=2 Q
 .D KILL
 .S AMHPOVP=$P(^AMHRPRO(AMHX,0),U)
 .Q:$P(^AMHPROB(AMHPOVP,0),U,5)=""
 .S APCDALVR("APCDVSIT")=AMHVSIT
 .S APCDALVR("APCDATMP")="[APCDALVR 9000010.07 (ADD)]"
 .S APCDALVR("APCDPAT")=$P(AMHR0,U,8)
 .S APCDALVR("APCDOVRR")=""
 .I $$PPINT^AMHUTIL(AMHR)]"",$D(^AMHSITE(DUZ(2),11,"B",$$PPINT^AMHUTIL(AMHR))) D PROVEXCP Q:'$D(APCDALVR("APCDTPOV"))  I 1
 .E  D @AMHLPCCT ;-- get pov and narrative based on pcc type of link
 .;S AMHICDP=$O(^ICD9("AB",APCDALVR("APCDTPOV"),0)) ;CSV
 .S AMHICDP=+$$CODEN^ICDCODE(APCDALVR("APCDTPOV"),80) ;CSV
 .I $D(^APCDINPT(9,11,"AC",APCDALVR("APCDTPOV"))) D
 ..;calculate days old and put in aupndays
 ..I AUPNDOB]"" S X2=AUPNDOB,X1=$P($P(AMHR0,U),".") D ^%DTC S AUPNDAYS=X
 ..Q:'$D(^ICD9(AMHICDP,9999999))
 ..I AUPNDAYS<$P(^ICD9(AMHICDP,9999999),U) S APCDALVR("APCDTACC")="`"_DUZ Q
 ..I $P(^ICD9(AMHICDP,9999999),U,2)<AUPNDAYS S APCDALVR("APCDTACC")="`"_DUZ
 .;IHS/TUCSON/LAB - added 2 lines below for patch 2 3/2/98
 .;S %=$O(^ICD9("AB",APCDALVR("APCDTPOV"),0)) Q:'%
 .S %=+$$CODEN^ICDCODE(APCDALVR("APCDTPOV"),80) Q:'%  Q:%=-1  ;CSV
 .S APCDALVR("APCDTPOV")="`"_%
 .D ^APCDALVR
 .I $D(APCDALVR("APCDAFLG")) S AMHBN="VFILE",AMHVFILE="V POV" D @("E"_APCDALVR("APCDAFLG")) Q
 .S AMHGOT=AMHGOT+1
 .Q
MEDPROB ;
 S AMHX=0 F  S AMHX=$O(^AMHRTMDP("AD",AMHR,AMHX)) Q:AMHX'=+AMHX  D
 .D KILL
 .S APCDALVR("APCDVSIT")=AMHVSIT
 .S APCDALVR("APCDATMP")="[APCDALVR 9000010.07 (ADD)]"
 .S APCDALVR("APCDTPOV")=".9999"
 .S APCDALVR("APCDPAT")=$P(AMHR0,U,8)
 .S APCDALVR("APCDTNQ")="`"_$P(^AMHRTMDP(AMHX,0),U)
 .S APCDALVR("APCDOVRR")=""
 .D ^APCDALVR
 .I $D(APCDALVR("APCDAFLG")) S AMHBN="VFILE",AMHVFILE="V POV" D @("E"_APCDALVR("APCDAFLG"))
 .Q
AT ;create v activity time record
 D KILL
 S APCDALVR("APCDVSIT")=AMHVSIT
 S APCDALVR("APCDATMP")="[APCDALVR 9000010.19 (ADD)]"
 S APCDALVR("APCDTACT")=$P(AMHR0,U,12)
 S APCDALVR("APCDPAT")=$P(AMHR0,U,8)
 S APCDALVR("APCDTTSG")=$P($G(^AMHREC(AMHR,11)),U,4)
 D ^APCDALVR
 I $D(APCDALVR("APCDAFLG")) S AMHBN="VFILE",AMHVFILE="V ACTIVITY TIME" D @("E"_APCDALVR("APCDAFLG"))
CPT ;pass v cpt's
 S AMHX=0 F  S AMHX=$O(^AMHRPROC("AD",AMHR,AMHX)) Q:AMHX'=+AMHX  D
 .D KILL
 .S APCDALVR("APCDVSIT")=AMHVSIT
 .S APCDALVR("APCDATMP")="[APCDALVR 9000010.18 (ADD)]"
 .S APCDALVR("APCDTCPT")="`"_$P(^AMHRPROC(AMHX,0),U)
 .S APCDALVR("APCDPAT")=$P(AMHR0,U,8)
 .D ^APCDALVR
 .I $D(APCDALVR("APCDAFLG")) S AMHBN="VFILE",AMHVFILE="V CPT" D @("E"_APCDALVR("APCDAFLG"))
 .Q
EDUC ;education provided
 S AMHX=0 F  S AMHX=$O(^AMHREDU("AD",AMHR,AMHX)) Q:AMHX'=+AMHX  D
 .D KILL
 .S APCDALVR("APCDVSIT")=AMHVSIT
 .S APCDALVR("APCDATMP")="[APCDALVR 9000010.16 (ADD)]"
 .S APCDALVR("APCDTTOP")="`"_$P(^AMHREDU(AMHX,0),U)
 .S APCDALVR("APCDPAT")=$P(AMHR0,U,8)
 .S X=$P(^AMHREDU(AMHX,0),U,4) I X D   I X S APCDALVR("APCDTPRO")="`"_X,APCDALVR("APCDTPRV")="`"_X
 ..I $P(^DD(9000010.16,.05,0),U,2)[6 S X=$G(^DIC(16,X,"A3"))
 ..Q
 .S APCDALVR("APCDTIG")=$P(^AMHREDU(AMHX,0),U,5)
 .S APCDALVR("APCDTMIN")=$P(^AMHREDU(AMHX,0),U,6)
 .S APCDALVR("APCDTCPT")=$P(^AMHREDU(AMHX,0),U,7) I APCDALVR("APCDTCPT")]"" S APCDALVR("APCDTCPT")="`"_APCDALVR("APCDTCPT")
 .S APCDALVR("APCDTLOU")=$P(^AMHREDU(AMHX,0),U,8)
 .S APCDALVR("APCDTOBJ")=$P(^AMHREDU(AMHX,0),U,9) S X=+$P(^DD(9000010.16,.14,0),">",2) S APCDALVR("APCDTOBJ")=$E(APCDALVR("APCDTOBJ"),1,X)
 .S APCDALVR("APCDTBC")=$P(^AMHREDU(AMHX,0),U,11)
 .S APCDALVR("APCDTCOM")=$P($G(^AMHREDU(AMHX,11)),U)
 .S APCDALVR("APCDTRTL")=$P($G(^AMHREDU(AMHX,11)),U,2) I APCDALVR("APCDTRTL")]"" S APCDALVR("APCDTRTL")="`"_APCDALVR("APCDTRTL")
 .D ^APCDALVR
 .I $D(APCDALVR("APCDAFLG")) S AMHBN="VFILE",AMHVFILE="V PATIENT ED" D @("E"_APCDALVR("APCDAFLG")) Q
 .;for now only till apcd patch goes out
 .I $P($G(^AMHREDU(AMHX,11)),U,2)]"" S DIE="^AUPNVPED(",DA=APCDALVR("APCDADFN"),DR="1102////"_$P(^AMHREDU(AMHX,11),U,2) D ^DIE K DA,DR,DIE
 .Q
 ;
VHF ;PASS HEALTH FACTORS
 S AMHX=0 F  S AMHX=$O(^AMHRHF("AD",AMHR,AMHX)) Q:AMHX'=+AMHX  D
 .D KILL
 .S APCDALVR("APCDVSIT")=AMHVSIT
 .S APCDALVR("APCDATMP")="[APCDALVR 9000010.23 (ADD)]"
 .S APCDALVR("APCDTHF")="`"_$P(^AMHRHF(AMHX,0),U)
 .S APCDALVR("APCDPAT")=$P(AMHR0,U,8)
 .S X=$P(^AMHRHF(AMHX,0),U,5) I X D   I X S APCDALVR("APCDTPRO")="`"_X
 ..I $P(^DD(9000010.23,.05,0),U,2)[6 S X=$G(^DIC(16,X,"A3"))
 ..Q
 .S APCDALVR("APCDTLS")=$P(^AMHRHF(AMHX,0),U,4)
 .S APCDALVR("APCDTQTY")=$P(^AMHRHF(AMHX,0),U,6)
 .S APCDALVR("APCDTCOM")=$P($G(^AMHRHF(AMHX,811)),U)
 .D ^APCDALVR
 .I $D(APCDALVR("APCDAFLG")) S AMHBN="VFILE",AMHVFILE="V HEALTH FACTORS" D @("E"_APCDALVR("APCDAFLG")) Q
 .Q
 ;
VMR ;PASS MEASUREMENTS
 S AMHX=0 F  S AMHX=$O(^AMHRMSR("AD",AMHR,AMHX)) Q:AMHX'=+AMHX  D
 .D KILL
 .S APCDALVR("APCDVSIT")=AMHVSIT
 .S APCDALVR("APCDATMP")="[APCDALVR 9000010.01 (ADD)]"
 .S APCDALVR("APCDTTYP")="`"_$P(^AMHRMSR(AMHX,0),U)
 .S APCDALVR("APCDPAT")=$P(AMHR0,U,8)
 .S APCDALVR("APCDTVAL")=$P(^AMHRMSR(AMHX,0),U,4)
 .D ^APCDALVR
 .I $D(APCDALVR("APCDAFLG")) S AMHBN="VFILE",AMHVFILE="V MEASUREMENT" D @("E"_APCDALVR("APCDAFLG")) Q
 .Q
 ;
EXAM ;
 I $P($G(^AMHREC(AMHR,14)),U,1)]"" D
 .Q:$O(^AUTTEXAM("C",34,0))=""
 .S AMHRES=$$VAL^XBDIQ1(9002011,AMHR,1401) I AMHRES["NEG" S AMHRES="N"
 .S AMHP=$P(^AMHREC(AMHR,14),U,2),AMHCOM=$P($G(^AMHREC(AMHR,15)),U) ;,AMHCOM=$TR(AMHCOM,";",":")
 .I AMHRES["REFUSED"!(AMHRES["UNABLE") S AMHEXCDE=34 D REF K AMHEXCDE Q
 .D KILL S APCDALVR("APCDVSIT")=AMHVSIT
 .S APCDALVR("APCDATMP")="[APCDALVR 9000010.13 (ADD)]"
 .S APCDALVR("APCDTEX")=34
 .S APCDALVR("APCDPAT")=$P(AMHR0,U,8)
 .S APCDALVR("APCDTRES")=AMHRES
 .S APCDALVR("APCDTCOM")=AMHCOM
 .S APCDALVR("APCDTEPR")="" I AMHP S APCDALVR("APCDTEPR")="`"_AMHP
 .D ^APCDALVR
 .I $D(APCDALVR("APCDAFLG")) S AMHBN="VFILE",AMHVFILE="V EXAM" D @("E"_APCDALVR("APCDAFLG"))
 .;this line is temporary until d/e patch 8 is released, then remove it!!!
 .;I '$D(APCDALVR("APCDAFLG")) D
 .;.D ^XBFMK
 .;.S DA=APCDALVR("APCDADFN"),DIE="^AUPNVXAM(",DR="81101///"_AMHCOM D ^DIE,^XBFMK
 .;.Q
 .Q
 I $P($G(^AMHREC(AMHR,14)),U,3)]"" D
 .Q:$O(^AUTTEXAM("C",35,0))=""
 .S AMHRES=$$VAL^XBDIQ1(9002011,AMHR,1403) I AMHRES["NEG" S AMHRES="N"
 .S AMHP=$P(^AMHREC(AMHR,14),U,4),AMHCOM=$P($G(^AMHREC(AMHR,16)),U) ;,AMHCOM=$TR(AMHCOM,";",":")
 .I AMHRES["REFUSED"!(AMHRES["UNABLE") S AMHEXCDE=35 D REF K AMHEXCDE Q
 .D KILL S APCDALVR("APCDVSIT")=AMHVSIT
 .S APCDALVR("APCDATMP")="[APCDALVR 9000010.13 (ADD)]"
 .S APCDALVR("APCDTEX")=35
 .S APCDALVR("APCDPAT")=$P(AMHR0,U,8)
 .S APCDALVR("APCDTRES")=AMHRES
 .S APCDALVR("APCDTCOM")=AMHCOM
 .S APCDALVR("APCDTEPR")="" I AMHP S APCDALVR("APCDTEPR")="`"_AMHP
 .D ^APCDALVR
 .Q
 I $P($G(^AMHREC(AMHR,14)),U,5)]"" D
 .Q:$O(^AUTTEXAM("C",36,0))=""
 .S AMHRES=$$VAL^XBDIQ1(9002011,AMHR,1405) I AMHRES["NEG" S AMHRES="N"
 .S AMHP=$P(^AMHREC(AMHR,14),U,6),AMHCOM=$P($G(^AMHREC(AMHR,17)),U) ;,AMHCOM=$TR(AMHCOM,";",":")
 .I AMHRES["REFUSED"!(AMHRES["UNABLE") S AMHEXCDE=36 D REF K AMHEXCDE Q
 .D KILL S APCDALVR("APCDVSIT")=AMHVSIT
 .S APCDALVR("APCDATMP")="[APCDALVR 9000010.13 (ADD)]"
 .S APCDALVR("APCDTEX")=36
 .S APCDALVR("APCDPAT")=$P(AMHR0,U,8)
 .S APCDALVR("APCDTRES")=AMHRES
 .S APCDALVR("APCDTCOM")=AMHCOM
 .S APCDALVR("APCDTEPR")="" I AMHP S APCDALVR("APCDTEPR")="`"_AMHP
 .D ^APCDALVR
 .Q
 Q
REF ;enter refusal into PCC
 S X=$O(^AUTTREFT("B","EXAM",0))
 Q:X=""
 S AMHTIDI=$O(^AUTTEXAM("C",AMHEXCDE,0)) Q:AMHTIDI=""  S AMHTID=$P(^AUTTEXAM(AMHTIDI,0),U)
 K DIC,DLAYGO,DIADD
 S DIC(0)="L",DIC="^AUPNPREF(",DIC("DR")=".02////"_$P(AMHR0,U,8)_";.03////"_$P($P(AMHR0,U),".")_";.04////"_AMHTID_";.05////9999999.15;.06////"_AMHTIDI_";.07////"_$S($E(AMHRES)["UNABLE":"U",AMHRES["REFUSED":"R",1:"")_";1101///"_AMHCOM
 K DD,D0 D FILE^DICN
 I Y=-1 W !!,"Creating refusal entry failed...." H 2 D ^XBFMK K DIADD,DLAYGO Q
 D ^XBFMK
 K DIADD,DLAYGO
 Q
PROVEXCP ;provider exception to the rule
 S Y=$O(^AMHSITE(DUZ(2),11,"B",$$PPINT^AMHUTIL(AMHR),""))
 I Y=""!(Y="???") S AMHVFILE="V POV",AMHBN="VFILE" D E8 Q
 S X=$P(^AMHSITE(DUZ(2),11,Y,0),U,2)
 I X="" S AMHVFILE="V POV",AMHBN="VFILE" D E8 Q
 I X=1 Q  ;NO PCC LINK AT ALL
 D @X
 Q
2 ;-- pass standard narrative and code
 ;S APCDALVR("APCDTPOV")=$S($P(^AMHSITE(DUZ(2),0),U,13)]"":$P(^ICD9($P(^AMHSITE(DUZ(2),0),U,13),0),U),1:"V65.40") ;replace with standard code
 S APCDALVR("APCDTPOV")=$S($P(^AMHSITE(DUZ(2),0),U,13)]"":$P($$ICDDX^ICDCODE($P(^AMHSITE(DUZ(2),0),U,13)),U,2),1:"V65.40") ;replace with standard code
 I AMHPTYPE="M" S APCDALVR("APCDTNQ")=$S($$MHNARR^AMHLEIN(DUZ(2))]"":$$MHNARR^AMHLEIN(DUZ(2)),1:"MENTAL HEALTH") Q  ;replace
 I AMHPTYPE="S" S APCDALVR("APCDTNQ")=$S($$SSNARR^AMHLEIN(DUZ(2))]"":$$SSNARR^AMHLEIN(DUZ(2)),1:"SOCIAL SERVICE VISIT") Q  ;replace
 I AMHPTYPE="C" S APCDALVR("APCDTNQ")=$S($$CDNARR^AMHLEIN(DUZ(2))]"":$$CDNARR^AMHLEIN(DUZ(2)),1:"CHEMICAL DEPENDENCY VISIT") Q  ;replace
 I AMHPTYPE="O" S APCDALVR("APCDTNQ")=$S($$OTNARR^AMHLEIN(DUZ(2))]"":$$OTNARR^AMHLEIN(DUZ(2)),1:"BEHAVIOR HEALTH VISIT") Q  ;replace
 S APCDALVR("APCDTNQ")="BEHAVIORAL HEALTH VISIT"
 Q
 ;
3 ;crosswalk (mask some stuff, no other)
 S APCDALVR("APCDTPOV")=$P(^AMHPROB(AMHPOVP,0),U,5)
 I '$P(^AMHPROB(AMHPOVP,0),U,8) S APCDALVR("APCDTNQ")="`"_$P(^AMHRPRO(AMHX,0),U,4)
 K AMHERR I $P(^AMHPROB(AMHPOVP,0),U,8) D  Q:$D(AMHERR)
 .;S X=$P(^AMHPROB(AMHPOVP,0),U,5),Y=$O(^ICD9("AB",X,""))  ;CSV
 .S X=$P(^AMHPROB(AMHPOVP,0),U,5),Y=+$$CODEN^ICDCODE(X,80) ;CSV
 .I Y=""!(Y=-1) S AMHBN="VFILE",AMHVFILE="V POV" D E2 S AMHERR=1 Q
 .S APCDALVR("APCDTNQ")=$E($P($$ICDDX^ICDCODE(Y,$P($P(^AMHREC(AMHR,0),U),".")),U,4),1,79)
 .I $P(^AMHPROB(AMHPOVP,0),U,9) S APCDALVR("APCDTNQ")=$E(APCDALVR("APCDTNQ"),1,42)_" SEE "_$E($$PPNAME^AMHUTIL(AMHR),1,18)_" FOR DETAILS"
 .I $P(^AMHPROB(AMHPOVP,0),U,11) S APCDALVR("APCDTNQ")="DIAGNOSTIC IMPRESSION:  "_$E(APCDALVR("APCDTNQ"),1,55)
 .I $P(^AMHPROB(AMHPOVP,0),U,12) S APCDALVR("APCDTNQ")=$E($P(^AMHPROB(AMHPOVP,0),U,2),1,35)_" - "_$E($P(^AUTNPOV($P(^AMHRPRO(AMHX,0),U,4),0),U),1,40)
 Q
 ;
4 ;
 S APCDALVR("APCDTPOV")=$P(^AMHPROB(AMHPOVP,0),U,5)
 S APCDALVR("APCDTNQ")="`"_$P(^AMHRPRO(AMHX,0),U,4)
 Q
5 ;
 S APCDALVR("APCDTPOV")=$P(^AMHPROB(AMHPOVP,0),U,5)
 ;S APCDALVR("APCDTNQ")=$S($P(^AMHSITE(DUZ(2),0),U,14)]"":$P(^AMHSITE(DUZ(2),0),U,14),1:"MENTAL HEALTH/SOCIAL WORK/SERVICES") ;replace
 I AMHPTYPE="M" S APCDALVR("APCDTNQ")=$S($$MHNARR^AMHLEIN(DUZ(2))]"":$$MHNARR^AMHLEIN(DUZ(2)),1:"MENTAL HEALTH") Q  ;replace
 I AMHPTYPE="S" S APCDALVR("APCDTNQ")=$S($$SSNARR^AMHLEIN(DUZ(2))]"":$$SSNARR^AMHLEIN(DUZ(2)),1:"SOCIAL SERVICE VISIT") Q  ;replace
 I AMHPTYPE="C" S APCDALVR("APCDTNQ")=$S($$CDNARR^AMHLEIN(DUZ(2))]"":$$CDNARR^AMHLEIN(DUZ(2)),1:"CHEMICAL DEPENDENCY VISIT") Q  ;replace
 I AMHPTYPE="O" S APCDALVR("APCDTNQ")=$S($$OTNARR^AMHLEIN(DUZ(2))]"":$$OTNARR^AMHLEIN(DUZ(2)),1:"BEHAVIOR HEALTH VISIT") Q  ;replace
 S APCDALVR("APCDTNQ")="BEHAVIORAL HEALTH VISIT"
 Q
V2 S AMHERROR="inability to create visit" G LBULL
V3 S AMHERROR="invalid visit parameters (date, location etc.)" G LBULL
 ;
E1 S AMHERROR="incorrect template specification" G LBULL
E2 S AMHERROR="invalid values being passed to "_AMHVFILE G LBULL
E8 S AMHERROR="Provider specific link not complete in site file "_$$PPNAME^AMHUTIL(AMHR) G LBULL
E9 S AMHERROR="Could not resolve file 200-file 6 pointer for V PROVIDER.  "_$P(^VA(200,X,0),U) G LBULL
 ;
LBULL ;
 K XMB
 S XMB(1)=AMHR,XMB(2)=$P(^DPT($P(AMHR0,U,8),0),U)_" (DFN "_$P(AMHR0,U,8)_")",Y=$P(AMHR0,U) D DD^%DT S XMB(3)=Y,XMB(4)=AMHERROR,XMB(5)=$G(AMHVFILE),XMB="AMH PCC LINK FAIL "_AMHBN,AMHDUZ=DUZ,DUZ=.5
 D ^XMB S DUZ=AMHDUZ K XMB,AMHERROR,AMHBN,AMHVFILE
 Q
