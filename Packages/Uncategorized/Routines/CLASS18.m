CLASS18 ;$O DPT B X-REF [ 03/10/99  2:48 PM ]
 ;This routine will $o thru B X-ref of name
 ;set counter = for each  print each name - then start again
 ;
START ;
 S TOTALCT=0
 S GRANDTOT=0
 D HEADING
 S D="" F  S D=$O(^CLASSDMO("B",D)) I D'="" S TOTALCT=TOTALCT+1 Q:D=""!(D="JACKSON,MICHAEL")  D PRINT
 ;WRITE LAST NAME ACCESSED & GRAND TOTAL COUNT
 W !!,"LAST PATIENT DFN ACCESSED = ",LASTNAME
 W !,"GRAND TOTAL COUNT-ALL RECORDS: ",GRANDTOT
 K D,X,TOTALCT,LASTNAME,HOB1,HOBDFN,HOBBY,TONISEX,GRANTOT
 Q
PRINT ;GET DFN FOR NAME TO PRINT
 S X="" F  S X=$O(^CLASSDMO("B",D,X)) Q:X'=+X  S GRANDTOT=GRANDTOT+1 D PRINT1
 Q
PRINT1 ;PRINT ACTUAL NAME
 W !?5,TOTALCT,?15,$P(^CLASSDMO(X,0),U)
 W ?40,X,?45,$P(^CLASSDMO(X,0),U,10)
 S LASTNAME=X
 S TONISEX=$P(^CLASSDMO(X,0),U,8)
 W ?60,TONISEX
 I $D(^CLASSDMO(X,1,0)) D HOBBIES
 I $D(HOBBPRNT) W ?65,HOBBPRNT
 I TOTALCT>5 D HEADING S TOTALCT=0
 Q
 ;
HEADING ;Print Heading
 W !!,"EMPLOYEE: ",?15,"NAME",?39,"DFN",?45,"SSN",?60,"SEX",?65,"1 HOBBY",!
 Q
 ;
HOBBIES ;If Hobbies Exist - get one hobby dfn only
 S HOB1=""
 F  S HOB1=$O(^CLASSDMO(X,1,"B",HOB1)) Q:HOB1=""  D
 . S HOBDFN="" F  S HOBDFN=$O(^CLASSDMO(X,1,"B",HOB1,0)) Q:HOBDFN'=+HOBDFN  S HOBBY=$P(^CLASSDMO(X,1,HOBDFN,0),U),HOBBPRNT=$P(^CLASSHOB(HOBBY,0),U) Q
 Q
