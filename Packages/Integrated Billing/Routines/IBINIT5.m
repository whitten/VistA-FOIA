IBINIT5	; ; 21-MAR-1994
	;;Version 2.0 ; INTEGRATED BILLING ;; 21-MAR-94
	K ^UTILITY("DIF",$J) S DIFRDIFI=1 F I=1:1:112 S ^UTILITY("DIF",$J,DIFRDIFI)=$T(IXF+I),DIFRDIFI=DIFRDIFI+1
	Q
IXF	;;INTEGRATED BILLING^IB
	;;36I;INSURANCE COMPANY;^DIC(36,;0;y;y;;n;;;n
	;;
	;;350;INTEGRATED BILLING ACTION;^IB(;0;y;y;;n;;;n;;
	;;
	;;350.1;IB ACTION TYPE;^IBE(350.1,;0;y;y;;n;;;y;m;y
	;;
	;;350.2I;IB ACTION CHARGE;^IBE(350.2,;0;y;y;;n;;;y;m;y
	;;
	;;350.21;IB ACTION STATUS;^IBE(350.21,;0;y;y;;n;;;n;;
	;;
	;;350.3I;IB CHARGE REMOVE REASONS;^IBE(350.3,;0;y;y;;n;;;y;m;y
	;;
	;;350.4ID;BILLABLE AMBULATORY SURGICAL CODE;^IBE(350.4,;0;y;y;;n;;;n;;
	;;
	;;350.41PI;UPDATE BILLABLE AMBULATORY SURGICAL CODE;^IBE(350.41,;0;y;y;;n;;;n;;
	;;
	;;350.5ID;BASC LOCALITY MODIFIER;^IBE(350.5,;0;y;y;;n;;;n;;
	;;
	;;350.6I;IB ARCHIVE/PURGE LOG;^IBE(350.6,;0;y;y;;n;;;n;;
	;;
	;;350.7IO;AMBULATORY CHECK-OFF SHEET;^IBE(350.7,;0;y;y;;n;;;n;;
	;;
	;;350.71OI;AMBULATORY SURG. CHECK-OFF SHEET PRINT FIELDS;^IBE(350.71,;0;y;y;;n;;;n;;
	;;
	;;350.8I;IB ERROR;^IBE(350.8,;0;y;y;;n;;;y;o;n
	;;
	;;350.9I;IB SITE PARAMETERS;^IBE(350.9,;0;y;y;;n;;;n;;
	;;
	;;351I;CATEGORY C BILLING CLOCK;^IBE(351,;0;y;y;;n;;;n;;
	;;
	;;351.1P;IB CONTINUOUS PATIENT;^IBE(351.1,;0;y;y;;n;;;n;;
	;;
	;;351.2PI;SPECIAL INPATIENT BILLING CASES;^IBE(351.2,;0;y;y;;n;;;n;;
	;;
	;;352.1I;BILLABLE APPOINTMENT TYPE;^IBE(352.1,;0;y;y;;n;;;y;m;y
	;;
	;;352.2P;NON-BILLABLE DISPOSITIONS;^IBE(352.2,;0;y;y;;n;;;n;;
	;;
	;;352.3P;NON-BILLABLE CLINIC STOP CODES;^IBE(352.3,;0;y;y;;n;;;n;;
	;;
	;;352.4P;NON-BILLABLE CLINICS;^IBE(352.4,;0;y;y;;n;;;n;;
	;;
	;;353;BILL FORM TYPE;^IBE(353,;0;y;y;;n;;;y;m;y
	;;
	;;353.1I;PLACE OF SERVICE;^IBE(353.1,;0;y;y;;n;;;y;o;y
	;;
	;;353.2I;TYPE OF SERVICE;^IBE(353.2,;0;y;y;;n;;;y;o;y
	;;
	;;354PI;BILLING PATIENT;^IBA(354,;0;y;y;;n;;;n;;
	;;
	;;354.1ID;BILLING EXEMPTIONS;^IBA(354.1,;0;y;y;;n;;;n;;
	;;
	;;354.2I;EXEMPTION REASON;^IBE(354.2,;0;y;y;;n;;;y;m;y
	;;
	;;354.3DI;BILLING THRESHOLDS;^IBE(354.3,;0;y;y;;n;;;y;m;y
	;;
	;;354.4P;BILLING ALERTS;^IBA(354.4,;0;y;y;;n;;;n;;
	;;
	;;354.5;BILLING ALERT DEFINITION;^IBE(354.5,;0;y;y;;n;;;y;m;y
	;;
	;;354.6;IB FORM LETTER;^IBE(354.6,;0;y;y;;n;;;y;m;y
	;;
	;;355.1I;TYPE OF PLAN;^IBE(355.1,;0;y;y;;n;;;y;o;y
	;;
	;;355.2;TYPE OF INSURANCE COVERAGE;^IBE(355.2,;0;y;y;;n;;;y;o;y
	;;
	;;355.3IP;GROUP INSURANCE PLAN;^IBA(355.3,;0;y;y;;n;;;n
	;;
	;;355.4ID;ANNUAL BENEFITS;^IBA(355.4,;0;y;y;;n;;;n
	;;
	;;355.5P;INSURANCE CLAIMS YEAR TO DATE;^IBA(355.5,;0;y;y;;n;;;n
	;;
	;;355.6;INSURANCE RIDERS;^IBE(355.6,;0;y;y;;n;;;y;m;y
	;;
	;;355.7PI;PERSONAL POLICY;^IBA(355.7,;0;y;y;;n;;;n;;
	;;
	;;356I;CLAIMS TRACKING;^IBT(356,;0;y;y;;n;;;n;
	;;
	;;356.1ID;HOSPITAL REVIEW;^IBT(356.1,;0;y;y;;n;;;n
	;;
	;;356.11I;CLAIMS TRACKING REVIEW TYPE;^IBE(356.11,;0;y;y;;n;;;y;m;y
	;;
	;;356.2ID;INSURANCE REVIEW;^IBT(356.2,;0;y;y;;n;;;n
	;;
	;;356.21;CLAIMS TRACKING DENIAL REASONS;^IBE(356.21,;0;y;y;;n;;;y;m;y
	;;
	;;356.3I;CLAIMS TRACKING SI/IS CATEGORIES;^IBE(356.3,;0;y;y;;n;;;y;m;y
	;;
	;;356.399PI;CLAIMS TRACKING/BILL;^IBT(356.399,;0;y;y;;n;;;n
	;;
	;;356.4I;CLAIMS TRACKING NON-ACUTE CLASSIFICATIONS;^IBE(356.4,;0;y;y;;n;;;y;m;y
	;;
	;;356.5P;CLAIMS TRACKING ALOS;^IBE(356.5,;0;y;y;;n;;;y;m;y
	;;
	;;356.6;CLAIMS TRACKING TYPE;^IBE(356.6,;0;y;y;;n;;;y;m;y
	;;
	;;356.7;CLAIMS TRACKING ACTION;^IBE(356.7,;0;y;y;;n;;;y;m;y
	;;
	;;356.8;CLAIMS TRACKING NON-BILLABLE REASONS;^IBE(356.8,;0;y;y;;n;;;y;m;y
	;;
	;;356.9IP;INPATIENT DIAGNOSIS;^IBT(356.9,;0;y;y;;n;;;n
	;;
	;;356.91PI;INPATIENT PROCEDURE;^IBT(356.91,;0;y;y;;n;;;n
	;;
	;;356.93PI;INPATIENT INTERIM DRG;^IBT(356.93,;0;y;y;;n;;;n;;
	;;
	;;356.94ID;INPATIENT PROVIDERS;^IBT(356.94,;0;y;y;;n;;;n
	;;
	;;357I;ENCOUNTER FORM;^IBE(357,;0;y;y;;n;;;n;;
	;;
	;;357.1I;ENCOUNTER FORM BLOCK;^IBE(357.1,;0;y;y;;n;;;n;;
	;;
