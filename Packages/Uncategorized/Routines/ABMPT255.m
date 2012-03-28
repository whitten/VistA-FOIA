ABMPT255 ; IHS/ASDST/LSL - 3P BILLING 2.5 Patch 5 POST INIT ;  
 ;;2.6;IHS 3P BILLING SYSTEM;;NOV 12, 2009
 ;
 ; 03/09/04 V2.5 P5 - Post init routine created
 ;
 Q
EN ; EP
 D ^ABMDPOST                 ; Just in case patch 2 not installed
 Q:$G(^ABMDERR(192,0))'=""   ;post-init has already been installed-don't do again
 D ERRCD192                    ; Create 3P Error Code 192
 D ERRCD193                    ; Create 3P Error Code 193
 D CODES                        ;Update codes to be one character
 D QUES               ;make sure right questions are on 837 formats
 Q
 ;
ERRCD192 ;
 ; Create 3P Error Code 192 - Imprecise Injury Date
 ; The code error for all 3 837 modes of export, else warning
 S DIC="^ABMDERR("
 S DIC(0)="EMQZXL"
 S DINUM=192
 S DLAYGO=9002274
 S X="IMPRECISE INJURY DATE"
 K DD,DO
 D FILE^DICN
 I +Y<1 Q
 ;
 S DIE=DIC
 S DA=+Y
 S DR=".02///If page 3 question ""Accident Related"" is yes, an exact date (mm/dd/yyyy) of injury is required"
 D ^DIE
 ;
 S IEN=DA
 D ERRSITE(IEN)
 F X=21,22,23 D EXPMODE(IEN,X)
 Q
ERRCD193 ;
 ; Create 3P Error Code 193 - Referring Provider Missing Person Class
 S DIC="^ABMDERR("
 S DIC(0)="EMQZXL"
 S DINUM=193
 S DLAYGO=9002274
 S X="PIN/PERSON CLASS MISSING FROM REFERRING PROVIDER"
 K DD,DO
 D FILE^DICN
 I +Y<1 Q
 ;
 S DIE=DIC
 S DA=+Y
 S DR=".02///Edit Referring Physician on page 3 and add Referring Physician PIN and/or Referring Physician Person Class"
 D ^DIE
 ;
 S IEN=DA
 D ERRSITE(IEN)
 F X=21,22,23 D EXPMODE(IEN,X)
 Q
 ;
ERRSITE(IEN) ;
 S ABMSITE=1
 F  S ABMSITE=$O(^ABMDPARM(ABMSITE)) Q:+ABMSITE=0  D
 . K DA,DIE,DIC,DR,Y,DINUM
 . S DLAYGO=9002274
 . S DA(1)=IEN
 . S DIC="^ABMDERR("_DA(1)_",31,"
 . S DINUM=ABMSITE,X=ABMSITE
 . S DIC(0)="EQXMLZ"
 . S DIC("P")=$P(^DD(9002274.04,31,0),U,2)
 . S DIC("DR")=".03///E"
 . K DD,DO
 . D FILE^DICN
 Q
 ;
EXPMODE(IEN,X) ;
 Q:'+IEN
 K DA,DIE,DIC,DR,Y,DINUM
 S DLAYGO=9002274
 S DA(1)=IEN
 S DIC="^ABMDERR("_DA(1)_",21,"
 S DINUM=X
 S DIC(0)="EQXMLZ"
 S DIC("P")=$P(^DD(9002274.04,21,0),U,2)
 K DD,DO
 D FILE^DICN
 Q
CODES ; change admission type and admission source codes to 1-digit codes
 S DIE="^ABMDCODE("
 F ABMT="A","T" D
 .S ABMCD=""
 .F  S ABMCD=$O(^ABMDCODE("AC",ABMT,ABMCD)) Q:ABMCD=""  D
 ..Q:$L(ABMCD)=1
 ..S ABMIEN=""
 ..F  S ABMIEN=$O(^ABMDCODE("AC",ABMT,ABMCD,ABMIEN)) Q:ABMIEN=""  D
 ...S DA=ABMIEN
 ...S DR=".01///"_+ABMCD
 ...D ^DIE
 K ABMT,ABMIEN
 ;
 ; add new codes with category Type of Service
 F ABMI=1:1 S ABMLN=$P($T(CODETOS+ABMI),";;",2) Q:ABMLN="END"  D
 .S ABMCD=$P(ABMLN,"^")
 .S ABMDESC=$P(ABMLN,"^",2)
 .S DIC="^ABMDCODE("
 .S DIC(0)="ALM"
 .S DIC("DR")=".01////"_ABMCD_";.02////K;.03////"_ABMDESC
 .K DD,DO
 .D FILE^DICN
 Q
QUES ;
 S DIE="^ABMDEXP("
 F DA=21,22,23 D
 .I DA=21 S DR=".08////1,2,3,4,5,8,14,19,21,22,23,24,28"
 .I DA=22 S DR=".08////1,2,3,4B,5,6,7,9,10,11,12B,14,15,19,20,25,26,28,29,30"
 .I DA=23 S DR=".08////1,2,3,4B,5,14,16,17,18,19,28"
 .D ^DIE
 Q
CODETOS ;
 ;;0^WHOLE BLOOD
 ;;1^MEDICAL CARE
 ;;2^SURGERY
 ;;3^CONSULATION
 ;;4^DIAGNOSTIC RADIOLOGY
 ;;5^DIAGNOSTIC LABORATORY
 ;;6^THERAPEUTIC RADIOLOGY
 ;;7^ANESTHESIA
 ;;8^ASSISTANT AT SURGERY
 ;;9^OTHER MEDICAL ITEMS OR SERVICES
 ;;A^USED DME
 ;;B^HIGH RISK SCREENING MAMMOGRAPHY
 ;;C^LOW RISK SCREENING MAMMOGRAPHY
 ;;D^AMBULANCE
 ;;E^ENTERAL/PARENTERAL NUTRIENTS/SUPPLIES
 ;;F^AMBULATORY SURGICAL CENTER
 ;;G^IMMUNOSUPPRESSIVE DRUGS
 ;;H^HOSPICE
 ;;J^DIABETIC SHOES
 ;;K^HEARING ITEMS AND SERVICES
 ;;L^ESRD SUPPLIES
 ;;M^MONTHLY CAPITATION PAYMENT FOR DIALYSIS
 ;;N^KIDNEY DONOR
 ;;P^LUMP SUM PURCHASE OF DME, PROSTHETICS, ORTHOTICS
 ;;Q^VISION ITEMS OR SERVICES
 ;;R^RENTAL OF DME
 ;;S^SURGICAL DRESSINGS OR OTHER MEDICAL SUPPLIES
 ;;T^OUTPATIENT MENTAL HEALTH TREATMENT LIMITATION
 ;;U^OCCUPATIONAL THERAPY
 ;;V^PNEUMOCOCCAL/FLU VACCINE
 ;;W^PHYSICAL THERAPY
 ;;END
