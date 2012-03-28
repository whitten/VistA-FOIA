APCDUTL ; IHS/CMI/LAB - DATA ENTRY UTILITIES ; 
 ;;2.0;IHS PCC SUITE;**2**;MAY 14, 2009
 ;
NR ;EP - called from APCD NRS (ADD) input template
 NEW A,B,C,D
 D EN^DDIOL("Nutritional Risk Screening Factors",,"!!")
 D EN^DDIOL("","","!")
 I $P(^AUPNVNTS(DA,0),U,4) D EN^DDIOL("- Age 70+",,"!?5")
 I $P(^AUPNVNTS(DA,0),U,5) D EN^DDIOL("- Nutrition Support",,"!?5")
 I $P(^AUPNVNTS(DA,0),U,6) D EN^DDIOL("- High Risk Weight Issue",,"!?5")
 I $P(^AUPNVNTS(DA,0),U,7) D EN^DDIOL("- High Risk Diagnosis",,"!?5")
 I $P(^AUPNVNTS(DA,0),U,8) D EN^DDIOL("- Poor Appetite",,"!?5")
 I $P(^AUPNVNTS(DA,0),U,9) D EN^DDIOL("- Difficulty Chewing",,"!?5")
 I $P(^AUPNVNTS(DA,0),U,10) D EN^DDIOL("- Food Allergies/Intolerances",,"!?5")
 I $P(^AUPNVNTS(DA,0),U,11) D EN^DDIOL("- Recent Vomiting or Diarrhea",,"!?5")
 I $P(^AUPNVNTS(DA,0),U,12) S B=$P(^AUPNVNTS(DA,0),U,13)
 I $P(^AUPNVNTS(DA,0),U,12) D EN^DDIOL("- Other:  "_B,,"!?5")
 D EN^DDIOL("","","!!")
 Q
