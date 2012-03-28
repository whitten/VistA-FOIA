APCHS2G ; IHS/CMI/LAB - PART 2B OF APCHS -- SUMMARY PRODUCTION COMPONENTS ;
 ;;2.0;IHS PCC SUITE;;MAY 14, 2009
 ;
OUTPT ; ********** OUTPATIENT ENCOUNTERS * 9000010/9000010.07 **********
 ; <SETUP>
 Q:'$D(^AUPNVSIT("AA",APCHSPAT))
 X APCHSCKP Q:$D(APCHSQIT)  X:'APCHSNPG APCHSBRK
 ; <DISPLAY>
 S APCHSPVD=0
 S APCHSPFN=""
 F APCHSIVD=0:0 S APCHSIVD=$O(^AUPNVSIT("AA",APCHSPAT,APCHSIVD)) Q:APCHSIVD=""!(APCHSIVD>APCHSDLM)  D  Q:APCHSNDM=0!($D(APCHSQIT))
 . D ONEDATE
 . Q:$D(APCHSQIT)
 . S:(APCHSDAT'=APCHSPVD)&APCHSDTU APCHSNDM=APCHSNDM-APCHSDTU,APCHSPVD=APCHSDAT
 . Q
 ;
OUTPTX ; <CLEANUP>
 K APCHSIVD,APCHSDTU,APCHSDAT,APCHSVDF,APCHSFAC,APCHSPFN,APCHSSCL,APCHSMTX,APCHSMOD,APCHSPVD,APCHSOVT,APCHSNDT,APCHSCLI,APCHSPDN,APCHSICD,APCHSICL,APCHSNRQ,APCHSPHN
 K APCHSNFL,APCHSNSH,APCHSCCL,APCHSNAB,APCHSVSC,APCHSITE,APCHSQIT,APCHSDCL,Y,APCHCSVD
 Q
 ;
ONEDATE ;
 S APCHSCCL=""
 S (Y,APCHCSVD)=-APCHSIVD\1+9999999 X APCHSCVD S APCHSDAT=Y
 S APCHSDTU=0,APCHSNDT=(APCHSDAT'=APCHSPVD)
 S APCHSVDF="" F APCHSQ=0:0 S APCHSVDF=$O(^AUPNVSIT("AA",APCHSPAT,APCHSIVD,APCHSVDF)) Q:APCHSVDF=""  D  Q:$D(APCHSQIT)
 . S APCHSSCL=""
 . S APCHSN=^AUPNVSIT(APCHSVDF,0)
 . Q:'$P(APCHSN,U,9)
 . Q:$P(APCHSN,U,11)
 . Q:'$$PHN(APCHSVDF)  ;do not use is phn is not a provider on this visit
 . D GETCLN,GETSITEV^APCHSUTL,DSPVIS
 . Q:$D(APCHSQIT)
 . Q
 Q
 ;
GETCLN ;
 ;APCHSDCL=set to 34 if Display Clinic is Yes, 23 if No
 I $D(^APCHSCTL(APCHSTYP,2)),$P(^(2),U,3)="Y"
 E  S APCHSCLI=" ",APCHSDCL=23 Q
 S APCHSDCL=34
 S APCHSCLI=$P(APCHSN,U,8) I APCHSCLI="" S APCHSCCL="<none>" Q
 S APCHSCLI=$P(APCHSN,U,8) Q:APCHSCLI=""
 Q:'$D(^DIC(40.7,APCHSCLI))
 I $D(^DIC(40.7,APCHSCLI,9999999)),$P(^(9999999),U,1)]"" S APCHSCLI=$P(^DIC(40.7,APCHSCLI,9999999),U,1),APCHSCCL=APCHSCLI Q
 S APCHSCLI=$E($P(^DIC(40.7,APCHSCLI,0),U,1),1,10)
 S APCHSCCL=APCHSCLI
 Q
PHN(V) ;if one provider is phn quit on 1 otherwise quit on ""
 I 'V Q ""
 I '$D(^AUPNVSIT(V)) Q ""
 I '$D(^AUPNVPRV("AD",V)) Q ""
 I $$PRIMPROV^APCLV(V,"D")=13!($$PRIMPROV^APCLV(V,"D")=32) Q 1
 Q ""
 ;if include secondary remove lines above about primary
 NEW %,%1,Y,P S Y=0,%1="" F  S Y=$O(^AUPNVPRV("AD",V,Y)) Q:Y'=+Y  S P=$P(^AUPNVPRV(Y,0),U) D
 .I $P(^DD(9000010.06,.01,0),U,2)[200,'$D(^VA(200,P)) Q
 .I $P(^DD(9000010.06,.01,0),U,2)[6,'$D(^DIC(6,P)) Q
 .S %=$$VALI^XBDIQ1($S($P(^DD(9000010.06,.01,0),U,2)[200:200,1:6),P,$S($P(^DD(9000010.06,.01,0),U,2)[200:53.5,1:2)) I % S %=$P($G(^DIC(7,%,9999999)),U)
 .I %=13!(%=32) S %1=1
 Q %1
DSPVIS ;
 S APCHSDTU=1
 I $O(^AUPNVPOV("AD",APCHSVDF,""))="" D NOPOV Q
 S APCHSPDN="" F APCHSQ=0:0 S APCHSPDN=$O(^AUPNVPOV("AD",APCHSVDF,APCHSPDN)) Q:'APCHSPDN  S APCHSN=^AUPNVPOV(APCHSPDN,0) D HASPOV
 Q
 ;
NOPOV ;
 S (APCHSICD,APCHSNRQ)="<purpose of visit not yet entered>",APCHSMOD=""
 G COMMON
 ;
HASPOV ;
 S APCHSICD=$P(APCHSN,U,1) D GETICDDX^APCHSUTL
 S APCHSNRQ=$P(APCHSN,U,4) D GETNARR^APCHSUTL I $P(APCHSN,U,5)]"" S APCHSNRQ=APCHSNRQ_"  (Stage: "_$P(APCHSN,U,5)_")" ;IHS/CMI/LAB
 S APCHSMOD=$P(APCHSN,U,6)
COMMON ;
 X APCHSCKP Q:$D(APCHSQIT)  S:APCHSNPG APCHSNDT=1
 I APCHSNDT W APCHSDAT S (APCHSPFN,APCHSSCL)="",APCHSNDT=0
 I APCHSNSH=APCHSPFN S APCHSFAC=""
 E  S (APCHSFAC,APCHSPFN)=APCHSNSH,APCHSSCL=""
 I APCHSCCL=APCHSSCL S APCHSCLI=""
 E  S (APCHSCLI,APCHSSCL)=APCHSCCL
 I APCHSICD["<purpose of visit not"&(APCHSSCL="<none>") S APCHSCLI=""
 I APCHSMOD]"" S APCHSMTX=$P(^DD(9000010.07,.06,0),U,3),APCHSMTX=$P($P(APCHSMTX,APCHSMOD_":",2),";",1),APCHSMTX=$P(APCHSMTX,",",1),APCHSICD=APCHSMTX_" "_APCHSICD
 S:$D(^AUPNVCHS("AD",APCHSVDF)) APCHSNTE="*** CHS ***"
 ;S APCHSICL=$S(APCHSCLI'=" ":34,1:23)
 W ?10,APCHSFAC W:APCHSCLI]"" ?23,APCHSCLI
 S APCHSICL=$S(APCHSDCL=34:34,1:24)
 S:0 APCHSICD=APCHSVSC_":"_APCHSICD D PRTICD^APCHSUTL
 ;display phn fields
 Q:'$D(^AUPNVPHN("AD",APCHSVDF))
 S APCHSPHN=$O(^AUPNVPHN("AD",APCHSVDF,0))
 Q:'APCHSPHN
 I $P(^AUPNVPHN(APCHSPHN,0),U,5)]"" X APCHSCKP Q:$D(APCHSQIT)  S:APCHSNPG APCHSNDT=1 W ?APCHSICL,"Intervention: ",$$VAL^XBDIQ1(9000010.32,APCHSPHN,.05)
 I $P(^AUPNVPHN(APCHSPHN,0),U,6)]"" X APCHSCKP Q:$D(APCHSQIT)  S:APCHSNPG APCHSNDT=1 W !?APCHSICL,"Complexity: ",$$VAL^XBDIQ1(9000010.32,APCHSPHN,.06)
 I $P(^AUPNVPHN(APCHSPHN,0),U,5)]""!($P(^AUPNVPHN(APCHSPHN,0),U,6)]"") W !
 I $D(^AUPNVPHN(APCHSPHN,21)) S APCHSNRQ="Psycho/Soc/Env: "_^AUPNVPHN(APCHSPHN,21),APCHSTXT="",APCHSICL=$S(APCHSDCL=34:34,1:24)+1 D PRTTXT^APCHSUTL
 I $D(^AUPNVPHN(APCHSPHN,22)) S APCHSNRQ="NSG Dx: "_^AUPNVPHN(APCHSPHN,22),APCHSTXT="",APCHSICL=$S(APCHSDCL=34:34,1:24)+1 D PRTTXT^APCHSUTL
 I $D(^AUPNVPHN(APCHSPHN,23)) S APCHSNRQ="Short Term Goals: "_^AUPNVPHN(APCHSPHN,23),APCHSTXT="",APCHSICL=$S(APCHSDCL=34:34,1:24)+1 D PRTTXT^APCHSUTL
 I $D(^AUPNVPHN(APCHSPHN,24)) S APCHSNRQ="Long Term Goals: "_^AUPNVPHN(APCHSPHN,24),APCHSTXT="",APCHSICL=$S(APCHSDCL=34:34,1:24)+1 D PRTTXT^APCHSUTL
 Q
 ;
