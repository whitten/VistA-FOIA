HLZFUNC ;IHS/OHPRD/DWG - IHS HL7 FUNCTIONS;   [ 04/02/2003   8:37 AM ]
 ;;1.6;HEALTH LEVEL SEVEN;**1004**;APR 1, 2003
 ;;1.6;DHCP HEALTH LEVEL SEVEN;;OCT 13, 1995
 ;
 ;
SITE() ;
 N X
 S X=DUZ(2)_"^"_$P(^DIC(4,DUZ(2),0),U)_"^"_$P(^AUTTLOC(DUZ(2),0),U,10)
 Q X
 ;
