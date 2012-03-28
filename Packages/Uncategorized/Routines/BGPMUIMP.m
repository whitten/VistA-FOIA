BGPMUIMP ;IHS/MSC/MGH - Import taxonomy;20 Dec 2010 10:37;DU
 ;;11.0;IHS CLINICAL REPORTING;**4**;JAN 06, 2011;Build 84
 ; Pharmacy List Update Functions
PHLFIL(DIR,FIL,TAX) ; EP - Import updates from a file
 N ERR,POP,CNT,ATXFLG,TAXIEN
 D OPEN^%ZISH(,DIR,FIL,"R")
 I POP W "File not found",! Q
 S ATXFLG=1
 S TAXIEN="" S TAXIEN=$O(^ATXAX("B",TAX,TAXIEN))
 Q:'TAXIEN ""
 F CNT=1:1 D  Q:POP
 .N REC,LP
 .U IO
 .D READNXT^%ZISH(.REC)
 .I '$L($G(REC)) S POP=1 Q
 .S LP=0
 .F  S LP=$O(REC(LP)) Q:'LP  S REC=REC_REC(LP)
 .U IO(0)
 .S ERR=$$PHLREC(REC)
 .W:$L(ERR) CNT,": ",ERR,!
 D CLOSE^%ZISH()
 Q
PHLREC(REC,DEBUG) ; EP - Import updates from a single record
 N CODE,AIEN
 S NAME=$G(REC)
 S AIEN="+1,"_TAXIEN_","
 S FDA(9002226.02101,AIEN,.01)=NAME
 S FDA(9002226.02101,AIEN,.02)=NAME
 D UPDATE^DIE(,"FDA","IEN","ERR")
 K FDA,IEN,ERR
 Q ""
LLISTFIL(DIR,FIL,TAX) ; EP - Import updates from a file
 N ERR,POP,CNT,ATXFLG,TAXIEN
 D OPEN^%ZISH(,DIR,FIL,"R")
 I POP W "File not found",! Q
 S ATXFLG=1
 S TAXIEN="" S TAXIEN=$O(^ATXAX("B",TAX,TAXIEN))
 Q:'TAXIEN ""
 F CNT=1:1 D  Q:POP
 .N LST,LP
 .U IO
 .D READNXT^%ZISH(.LST)
 .I '$L($G(LST)) S POP=1 Q
 .S LP=0
 .F  S LP=$O(LST(LP)) Q:'LP  S LST=LST_LST(LP)
 .U IO(0)
 .S ERR=$$PHLLST(LST)
 .W:$L(ERR) CNT,": ",ERR,!
 D CLOSE^%ZISH()
 Q
PHLLST(LST,DEBUG) ; EP - Import updates from a single record
 N REC
 F LP=1:1 S REC=$P(LST,", ",LP) Q:REC=""  D
 .N CODE,AIEN
 .S NAME=$G(REC)
 .S AIEN="+1,"_TAXIEN_","
 .S FDA(9002226.02101,AIEN,.01)=NAME
 .S FDA(9002226.02101,AIEN,.02)=NAME
 .D UPDATE^DIE(,"FDA","IEN","ERR")
 .K FDA,IEN,ERR
 Q ""
