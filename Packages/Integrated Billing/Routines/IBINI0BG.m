IBINI0BG	; ; 21-MAR-1994
	;;Version 2.0 ; INTEGRATED BILLING ;; 21-MAR-94
	Q:'DIFQR(399.2)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,399.2,504,0)
	;;=504^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,505,0)
	;;=505^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,506,0)
	;;=506^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,507,0)
	;;=507^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,508,0)
	;;=508^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,509,0)
	;;=509^OUTPATIENT/OTHER^^OTHER OUTPATIENT SERVICES
	;;^UTILITY(U,$J,399.2,510,0)
	;;=510^CLINIC^1^GENERAL CLASSIFICATION
	;;^UTILITY(U,$J,399.2,511,0)
	;;=511^CHRONIC PAIN CL^^CHRONIC PAIN CENTER
	;;^UTILITY(U,$J,399.2,512,0)
	;;=512^DENTAL CLINIC^1^DENTAL CLINIC
	;;^UTILITY(U,$J,399.2,513,0)
	;;=513^PSYCH CLINIC^^PSYCHIATRIC CLINIC
	;;^UTILITY(U,$J,399.2,514,0)
	;;=514^OB-GYN CLINIC^^OB-GYN CLINIC
	;;^UTILITY(U,$J,399.2,515,0)
	;;=515^PEDS CLINIC^^PEDIATRIC CLINIC
	;;^UTILITY(U,$J,399.2,516,0)
	;;=516^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,517,0)
	;;=517^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,518,0)
	;;=518^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,519,0)
	;;=519^OTHER CLINIC^^OTHER CLINIC
	;;^UTILITY(U,$J,399.2,520,0)
	;;=520^FREESTAND CLINIC^^GENERAL CLASSIFICATION
	;;^UTILITY(U,$J,399.2,521,0)
	;;=521^RURAL/CLINIC^^RURAL HEALTH-CLINIC
	;;^UTILITY(U,$J,399.2,522,0)
	;;=522^RURAL/HOME^^RURAL HEALTH-HOME
	;;^UTILITY(U,$J,399.2,523,0)
	;;=523^FAMILY PRACTICE^^FAMILY PRACTICE
	;;^UTILITY(U,$J,399.2,524,0)
	;;=524^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,525,0)
	;;=525^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,526,0)
	;;=526^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,527,0)
	;;=527^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,528,0)
	;;=528^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,529,0)
	;;=529^OTHER FR/STD CLINIC^^OTHER FREESTANDING CLINIC
	;;^UTILITY(U,$J,399.2,530,0)
	;;=530^OSTEOPATH SVS^^GENERAL CLASSIFICATION
	;;^UTILITY(U,$J,399.2,531,0)
	;;=531^OSTEOPATH RX^^OSTEOPATHIC THERAPY
	;;^UTILITY(U,$J,399.2,532,0)
	;;=532^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,533,0)
	;;=533^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,534,0)
	;;=534^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,535,0)
	;;=535^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,536,0)
	;;=536^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,537,0)
	;;=537^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,538,0)
	;;=538^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,539,0)
	;;=539^OTHER OSTEOPATH^^OTHER OSTEOPATHIC SERVICES
	;;^UTILITY(U,$J,399.2,540,0)
	;;=540^AMBULANCE^^GENERAL CLASSIFICATION
	;;^UTILITY(U,$J,399.2,541,0)
	;;=541^AMBUL/SUPPLY^^SUPPLIES
	;;^UTILITY(U,$J,399.2,542,0)
	;;=542^AMBUL/MED TRANS^^MEDICAL TRANSPORT
	;;^UTILITY(U,$J,399.2,543,0)
	;;=543^AMBUL/HEARTMOBL^^HEART MOBILE
	;;^UTILITY(U,$J,399.2,544,0)
	;;=544^AMBUL/OXY^^OXYGEN
	;;^UTILITY(U,$J,399.2,545,0)
	;;=545^AIR AMBULANCE^^AIR AMBULANCE
	;;^UTILITY(U,$J,399.2,546,0)
	;;=546^AMBUL/NEONAT^^NEONATAL AMBULANCE SERVICES
	;;^UTILITY(U,$J,399.2,547,0)
	;;=547^AMBUL/PHARMACY^^PHARMACY
	;;^UTILITY(U,$J,399.2,548,0)
	;;=548^AMBUL/TELEPHONIC EKG^^TELEPHONE TRANSMISSION EKG
	;;^UTILITY(U,$J,399.2,549,0)
	;;=549^OTHER AMBULANCE^^OTHER AMBULANCE
	;;^UTILITY(U,$J,399.2,550,0)
	;;=550^SKILLED NURSING^^GENERAL CLASSIFICATION
	;;^UTILITY(U,$J,399.2,551,0)
	;;=551^SKILLED NURS/VISIT^^VISIT CHARGE
	;;^UTILITY(U,$J,399.2,552,0)
	;;=552^SKILLED NURS/HOUR^^HOURLY CHARGE
	;;^UTILITY(U,$J,399.2,553,0)
	;;=553^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,554,0)
	;;=554^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,555,0)
	;;=555^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,556,0)
	;;=556^*RESERVED^^*RESERVED
	;;^UTILITY(U,$J,399.2,557,0)
	;;=557^*RESERVED^^*RESERVED
