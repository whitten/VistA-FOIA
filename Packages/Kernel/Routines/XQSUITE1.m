XQSUITE1 ;Subroutines for XQSUITE: SuiteMan ;11/29/94  11:47 [ 04/02/2003   8:29 AM ]
 ;;8.0;KERNEL;**1002,1003,1004,1005,1007**;APR 1, 2003
 ;;8.0;KERNEL;;Jul 10, 1995
 ;;KV8
 ;
SPLIT ;Takes X, Returns Y1 and Y2
 N XQIN,XQMID,XQDONE,XQSP
 S XQIN=X,XQMID=($L(XQIN)\2)+1,XQDONE=0,XQSP=" "
 I $E(XQIN,XQMID)=XQSP S Y1=$E(XQIN,1,XQMID-1),Y2=$E(XQIN,XQMID+1,$L(XQIN))
 F XQI=1:1:XQMID-1 Q:XQDONE  D
 .I $E(XQIN,XQMID-XQI)=XQSP S Y1=$E(XQIN,1,(XQMID-XQI)-1),Y2=$E(XQIN,$L(Y1)+2,$L(XQIN)),XQDONE=1
 .Q:XQDONE 
 .I $E(XQIN,XQMID+XQI)=XQSP S Y1=$E(XQIN,1,(XQMID+XQI)-1),Y2=$E(XQIN,$L(Y1)+2,$L(XQIN)),XQDONE=1
 .Q
 ;W !,XQIN,!,Y1,!,Y2
 Q
