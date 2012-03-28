APCHS61 ; IHS/CMI/LAB - PART 6 OF APCHS -- SUMMARY PRODUCTION COMPONENTS 18 Jun 2008 10:07 AM ; 
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;
 ;
FMH ;EP -  ******* FAMILY HISTORY * 9000014 *******
 ; <SETUP>
 I '$D(^AUPNFH("AC",APCHSPAT)),'$D(^AUPNFHR("AA",APCHSPAT)) Q  ;no family history to display
 X APCHSCKP Q:$D(APCHSQIT)  X:'APCHSNPG APCHSBRK
 ; <DISPLAY>
 K APCHTFH
 S APCHSDFN=0 F  S APCHSDFN=$O(^AUPNFH("AC",APCHSPAT,APCHSDFN)) Q:'APCHSDFN  D
 .Q:'$D(^AUPNFH(APCHSDFN,0))  ;bad xref
 .S R=$P(^AUPNFH(APCHSDFN,0),U,9)
 .I R="" S R="Z",S=$$VAL^XBDIQ1(9000014,APCHSDFN,.07),Z=S_" ",O=8 D  G FMH1
 ..I S="" S S="UNKNOWN",Z="UNKNOWN "
 .S S=$$VAL^XBDIQ1(9000014.1,R,.01),Z=S_" "_$P(^AUPNFHR(R,0),U,3)
 .S O=$P(^AUPNFHR(R,0),U) I O S O=$P($G(^AUTTRLSH(O,21)),U,3)
 .I 'O S O=8
FMH1 .S APCHTFH(O,S,Z,R,(9999999-$$LDM(APCHSDFN)),APCHSDFN)=""
 ;get relations with no conditions
 S X=0 F  S X=$O(^AUPNFHR("AA",APCHSPAT,X)) Q:X'=+X  S Y=0 F  S Y=$O(^AUPNFHR("AA",APCHSPAT,X,Y)) Q:Y'=+Y  D
 .I '$D(^AUPNFH("AE",Y)) D
 ..S R=Y
 ..S S=$$VAL^XBDIQ1(9000014.1,R,.01),Z=S_" "_$P(^AUPNFHR(R,0),U,3)
 ..S O=$P(^AUPNFHR(R,0),U) I O S O=$P($G(^AUTTRLSH(O,21)),U,3)
 ..I 'O S O=8
 ..S APCHTFH(O,S,Z,R,(9999999-$P(^AUPNFHR(R,0),U,9)),0)=""
 W "Date Last Mod",?14,"Relation/Status/Diagnosis"
 S APCHO=0 F  S APCHO=$O(APCHTFH(APCHO)) Q:APCHO'=+APCHO  D FMH2
FMHX K APCHSDFN,APCHSN,APCHSICD,APCHSDAT,APCHSNRQ,APCHSICL,APCHSDFN,APCHTFH,APCHS,APCHZ,APCHR,APCHD
 Q
LDM(I) ;get last date modified of Family History or relation
 I $G(I)="" Q ""
 I '$D(^AUPNFH(I,0)) Q ""
 NEW J,D,E
 S D=""
 S J=$P(^AUPNFH(I,0),U,9) I J S D=$P($G(^AUPNFHR(J,0)),U,9) I D="" S D=$P($G(^AUPNFHR(J,0)),U,9)
 S E=$P(^AUPNFH(I,0),U,12) I E>D S D=E
 S E=$P(^AUPNFH(I,0),U,3) I E>D S D=E
 Q D
FMH2 ;
 S APCHS="",APCHC=0 F  S APCHS=$O(APCHTFH(APCHO,APCHS)) Q:APCHS=""!($D(APCHSQIT))  D
 .S APCHZ="" F  S APCHZ=$O(APCHTFH(APCHO,APCHS,APCHZ)) Q:APCHZ=""!($D(APCHSQIT))  D
 ..S APCHR="" F  S APCHR=$O(APCHTFH(APCHO,APCHS,APCHZ,APCHR)) Q:APCHR=""!($D(APCHSQIT))  D
 ...S APCHTD=$O(APCHTFH(APCHO,APCHS,APCHZ,APCHR,0)),APCHTD=(9999999-APCHTD) S Y=APCHTD X APCHSCVD S APCHTDAT=Y S:APCHTDAT="/" APCHTDAT=""
 ...S APCHD="",APCHC=0 F  S APCHD=$O(APCHTFH(APCHO,APCHS,APCHZ,APCHR,APCHD)) Q:APCHD=""!($D(APCHSQIT))  D
 ....S APCHSDFN="" F  S APCHSDFN=$O(APCHTFH(APCHO,APCHS,APCHZ,APCHR,APCHD,APCHSDFN)) Q:APCHSDFN=""!($D(APCHSQIT))  D FHDSP
 ;S APCHSDFN="" F APCHSQ=0:0 S APCHSDFN=$O(^AUPNFH("AC",APCHSPAT,APCHSDFN)) Q:APCHSDFN=""  D FHDSP
 ; <CLEANUP>
 Q
FHDSP S APCHC=APCHC+1
 I APCHC=1 W !,APCHTDAT,?14,APCHZ,"  Status: "
 S APCHSTAT=""
 I 'APCHR,APCHSDFN D
 .S APCHSTAT=$S($P(^AUPNFH(APCHSDFN,0),U,6)]"":$$VAL^XBDIQ1(9000014,APCHSDFN,.06),1:"None")
 I APCHR S APCHSTAT=$S($P($G(^AUPNFHR(APCHR,0)),U,4)]"":$$VAL^XBDIQ1(9000014.1,APCHR,.04),1:"None")
 I APCHC=1 W APCHSTAT,!
 I APCHR,$P(^AUPNFHR(APCHR,0),U,5)]""!($P(^AUPNFHR(APCHR,0),U,6)]"") D
 .I APCHC=1 W ?14,"Age at Death: ",$$VAL^XBDIQ1(9000014.1,APCHR,.05),"  Cause of Death: ",$S($P(^AUPNFHR(APCHR,0),U,6)]"":$P(^AUPNFHR(APCHR,0),U,6),1:"Data Not Available"),!
 I APCHR,$P(^AUPNFHR(APCHR,0),U,7)]""!($P(^AUPNFHR(APCHR,0),U,8)]"") D
 .I APCHC=1 W ?14,"Multiple Birth: "_$$VAL^XBDIQ1(9000014.1,APCHR,.07)_$S($P(^AUPNFHR(APCHR,0),U,7)="Y":"  Multiple Birth Type: "_$$VAL^XBDIQ1(9000014.1,APCHR,.08),1:""),! ;_"  Date Updated: "_$$VAL^XBDIQ1(9000014.1,R,.09)
 Q:'APCHSDFN
 S APCHSN=^AUPNFH(APCHSDFN,0)
 S APCHSICD=$P(APCHSN,U,1) D GETICDDX^APCHSUTL
 ;S Y=$P(APCHSN,U,3) X APCHSCVD S APCHSDAT=Y
 S APCHSNRQ=$P(APCHSN,U,4)
 D GETNARR^APCHSUTL
 X APCHSCKP Q:$D(APCHSQIT)  ;  W !,APCHSDAT
 S (X,R,S,N,A,P)=""
 ;S R=$$VAL^XBDIQ1(9000014,APCHSDFN,.07)
 S APCHSNRQ=APCHSNRQ_" ("_$$VAL^XBDIQ1(9000014,APCHSDFN,.01)_")"
 S A="" I APCHSDFN S A=$$VAL^XBDIQ1(9000014,APCHSDFN,.11) I A="" S A=$$VAL^XBDIQ1(9000014,APCHSDFN,.05)
 ;S S=$$VAL^XBDIQ1(9000014,APCHSDFN,.06)
 ;S P=$$VAL^XBDIQ1(9000014,APCHSDFN,.08)
 ;S X=R
 ;I X]"" S X=X_"; "
 S X=APCHSNRQ
 S X=X_$S(A]"":"; Age at Onset: "_A,1:"; Age at Onset: None")
 ;S X=X_$S(S]"":"; Status: "_S,1:"; Status: None")
 ;S X=X_$S(P]"":"; Documented By: "_P,1:"")
 S APCHSICL=14,APCHSNRQ=X
 D PRTICD^APCHSUTL
 Q
 ;
PWH ;EP - called from component Patient wellness Handout
 ; <SETUP>
 X APCHSCKP Q:$D(APCHSQIT)  X:'APCHSNPG APCHSBRK
 ; <DISPLAY>
 K APCHTFH
 S APCHSIVD="" F  S APCHSIVD=$O(^APCHPWHL("AA",APCHSPAT,APCHSIVD)) Q:APCHSIVD=""!(APCHSIVD>APCHSDLM)  D
 .S APCHIEN=0 F  S APCHIEN=$O(^APCHPWHL("AA",APCHSPAT,APCHSIVD,APCHIEN)) Q:APCHIEN'=+APCHIEN  D
 ..S APCHSN=$G(^APCHPWHL(APCHIEN,0))
 ..I APCHSN="" Q
 ..S N=$$VAL^XBDIQ1(9001027,APCHIEN,.02)
 ..S $P(APCHTFH(N),U)=$P($G(APCHTFH(N)),U)+1
 ..S P=$P(APCHTFH(N),U)+1
 ..S $P(APCHTFH(N),U,P)=$$DATE^APCHSMU($P(^APCHPWHL(APCHIEN,0),U,4))
 ;now display
 I '$D(APCHTFH) W "No Patient Wellness Handouts given to this patient.",! Q
 W ?2,"PATIENT WELLNESS HANDOUT TYPE",?34,"# given",?42,"Dates Last 4 Given to Patient",!
 W $$REPEAT^XLFSTR("-",79),!
 S APCHSN="" F  S APCHSN=$O(APCHTFH(APCHSN)) Q:APCHSN=""!($D(APCHSQIT))  D
 .W ?2,APCHSN,?34,$P(APCHTFH(APCHSN),U) W ?42,$P(APCHTFH(APCHSN),U,2)," ",$P(APCHTFH(APCHSN),U,3)," ",$P(APCHTFH(APCHSN),U,4)," ",$P(APCHTFH(APCHSN),U,5),!
 .Q
 K APCHTFH,APCHSN
 Q
