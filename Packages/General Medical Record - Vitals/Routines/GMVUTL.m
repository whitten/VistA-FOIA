GMVUTL ;HOIFO/RM,MD,FT-CALLABLE ENTRY POINTS FOR PROGRAMMER UTILITIES ;12/27/01  10:46
 ;;5.0;GEN. MED. REC. - VITALS;;Oct 31, 2002
 ;
 ; This routine uses the following IAs:
 ; <None>
 ;
EN1 ; CALL TO CONVERT TEMPERATURE (F) IN VARIABLE X TO TEMPERATURE (C)
 ; IN VARIABLE Y
 S Y=$J(X-32*5/9,0,1)
 Q
EN2 ; CALL TO CONVERT AN INCHES MEASUREMENT IN X TO A CENTIMETER 
 ; MEASUREMENT IN Y
 S Y=$J(2.54*X,0,2)
 Q
EN3 ; CALL TO CONVERT A WEIGHT (LBS) IN VARIABLE X TO A WEIGHT (KG)
 ; IN VARIABLE Y
 S Y=$J(X/2.2,0,2)
 Q
