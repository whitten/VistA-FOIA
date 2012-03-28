BQIPLDS2 ;VNGT/HS/ALA-Panel Def Description Utility Cont. ; 04 Feb 2011  1:58 PM
 ;;2.1;ICARE MANAGEMENT SYSTEM;;Feb 07, 2011
 ;
 ;
ALGY ;EP - Allergy
 ; Only reformat description with designated operand
 I $G(FPARMS("ALLOP"))="" Q
 ; If only a single Allergy was identified operand is meaningless
 I '$D(FMPARMS("ALLERGY")) Q
 S FPARMS("ALLOP")=$S(FPARMS("ALLOP")="&":", AND ",1:", OR ")
 N DX,APM
 S (DX,APM)="",FPARMS("ALLERGY")=""
 F  S DX=$O(FMPARMS("ALLERGY",DX)) Q:DX=""  D
 . I $D(AFMPARMS("ALLERGY",DX)) D
 .. S APM=$$ADDAP^BQIPLDS1("ALLERGY",DX)
 . I $O(FMPARMS("ALLERGY",DX))="" S FPARMS("ALLERGY")=FPARMS("ALLERGY")_DX_APM Q
 . S FPARMS("ALLERGY")=FPARMS("ALLERGY")_DX_APM_FPARMS("ALLOP")
 K FMPARMS("ALLERGY"),AFMPARMS("ALLERGY")
 Q
