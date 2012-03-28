FHWORI ; HISC/NCA - Initial Master File Population ;10/1/96  13:12
 ;;5.0;Dietetics;**12**;Oct 11, 1995
DO ; Code Diet Order
 S PKG=$O(^DIC(9.4,"C","FH",0)) Q:'PKG
 S VER=$P($G(^DIC(9.4,PKG,22,0)),"^",3) Q:'VER
 S VER=+$G(^DIC(9.4,PKG,22,VER,0))
 K ^TMP($J,"FHUPD"),MSG
 S FILE="111",FILNM=$P($G(^FH(111,0)),"^",1),EVENT="REP",ACT="MAD"
 F IEN=0:0 S IEN=$O(^FH(111,IEN)) Q:IEN<1  S X=$G(^(IEN,0)) S:$G(^("I"))="Y" ^TMP($J,"FHUPD",IEN)="" D CODE,D1
 K ACT,EVENT,FHK,FILE,FILNM,IEN,IEN1,NAM,N1,N2,PREC,SYN,SYN1,X
TF ; Code Tubefeeding
 K ^TMP($J,"FHUPT")
 S FILE="118.2",FILNM=$P($G(^FH(118.2,0)),"^",1),EVENT="REP",ACT="MAD" D CODE
 F IEN=0:0 S IEN=$O(^FH(118.2,IEN)) Q:IEN<1  S X=$G(^(IEN,0)) S:$G(^("I"))="Y" ^TMP($J,"FHUPT",IEN)="" D CODE,T1
 K ACT,EVENT,FILE,FILNM,IEN,IEN1,NAM,N1,N2,PKG,SYN,VER,X
INACTD ; Code Update Inactive Diet Orders
 G:'$D(^TMP($J,"FHUPD")) INACTT
 S FILE="111",FILNM=$P($G(^FH(111,0)),"^",1),EVENT="UPD",ACT="MDC" D CODE
 F IEN=0:0 S IEN=$O(^TMP($J,"FHUPD",IEN)) Q:IEN<1  S X=$G(^FH(111,IEN,0)) D CODE,D1
 K ^TMP($J,"FHUPD"),ACT,EVENT,FILE,FILNM,IEN,NAM,N1,N2,PREC,SYN,X
INACTT ; Code Update Inactive Tubefeedings
 Q:'$D(^TMP($J,"FHUPT"))
 S FILE="118.2",FILNM=$P($G(^FH(118.2,0)),"^",1),EVENT="UPD",ACT="MDC" D CODE
 F IEN=0:0 S IEN=$O(^TMP($J,"FHUPT",IEN)) Q:IEN<1  S X=$G(^FH(118.2,IEN,0)) D CODE,T1
 K ^TMP($J,"FHUPT"),ACT,EVENT,FILE,FILNM,IEN,IEN1,NAM,N1,N2,SYN,X
 Q
CODE ; Code MSH for Orderable Items
 S N1=1
 S MSG(N1)="MSH|^~\&|DIETETICS|"_^DD("SITE",1)_"|||||MFN"
 ; code MFI
 S N1=N1+1
 S MSG(N1)="MFI|"_FILE_"^"_FILNM_"^99DD||"_EVENT_"|||NE"
 Q
D1 ; Code MFE, ZFH, and ZSY for Diet Orders
 S NAM=$P(X,"^",1) Q:NAM=""  S PREC=$P(X,"^",4) Q:'PREC
 S SYN=$P(X,"^",2),N1=N1+1
 S MSG(N1)="MFE|"_ACT_"|||^^^"_IEN_"^"_NAM_"^99FHD"
 S N1=N1+1,MSG(N1)="ZFH|D|"_PREC_"||"_$P(X,"^",3)
 S FHK=0 F IEN1=0:0 S IEN1=$O(^FH(111,IEN,"AN",IEN1)) Q:IEN1<1  S SYN1=$G(^(IEN1,0)) D
 .S FHK=IEN1,SYN1=$P(SYN1,"^",1) Q:SYN1=""  S N1=N1+1
 .S MSG(N1)="ZSY|"_IEN1_"|"_SYN1 Q
 I SYN'="" S N1=N1+1,MSG(N1)="ZSY|"_(FHK+1)_"|"_SYN
 D SEND
 Q
T1 ; Code MFE, ZFH, and ZSY for Tubefeedings
 S NAM=$P(X,"^",1) Q:NAM=""  S N1=N1+1
 S MSG(N1)="MFE|"_ACT_"|||^^^"_IEN_"^"_NAM_"^99FHT"
 S N1=N1+1,MSG(N1)="ZFH|T"
 F IEN1=0:0 S IEN1=$O(^FH(118.2,IEN,1,IEN1)) Q:IEN1<1  S SYN=$G(^(IEN1,0)) D
 .S SYN=$P(SYN,"^",1) Q:SYN=""  S N1=N1+1
 .S MSG(N1)="ZSY|"_IEN1_"|"_SYN Q
 D SEND
 Q
SEND ; Send Message to OE/RR
 D MSG^XQOR("FH ORDERABLE ITEM UPDATE",.MSG)
 K MSG Q
