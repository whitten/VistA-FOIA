BONDEMO ;$ORDER DPT B X-REF [ 09/15/99  11:10 AM ]
 ;This routine will $o thru B X-ref of name
 ;set counter = for each  print each name - then start again
 ;STUDENT REPLACE THIS LINE
START ;
 ;STUDENT INSERTED LINE OF CODE
 ;STUDENT INSERTED THIS SECOND LINE
 S TOTALCT=0
 S GRANDTOT=0
 D HEADING
 S D="" F  S D=$O(^TONIDEMO("B",D)) I D'="" S TOTALCT=TOTALCT+1 Q:D=""!(D="JACKSON,MICHAEL")  D PRINT
 ;WRITE LAST NAME ACCESSED & GRAND TOTAL COUNT
 W !!,"LAST PATIENT DFN ACCESSED = ",LASTNAME
 W !,"GRAND TOTAL COUNT-ALL RECORDS: ",GRANDTOT
 K D,X,TOTALCT,LASTNAME,HOB1,HOBDFN,HOBBY,TONISEX,GRANTOT
 Q
PRINT ;GET DFN FOR NAME TO PRINT
 S X="" F  S X=$O(^TONIDEMO("B",D,X)) Q:X'=+X  S GRANDTOT=GRANDTOT+1 D PRINT1
 Q
PRINT1 ;PRINT ACTUAL NAME
 W !?5,TOTALCT,?15,$P(^TONIDEMO(X,0),U)
 W ?40,X,?45,$P(^TONIDEMO(X,0),U,10)
 S LASTNAME=X
 S TONISEX=$P(^TONIDEMO(X,0),U,8)
 W ?60,TONISEX
 I $D(^TONIDEMO(X,1,0)) D HOBBIES
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
 F  S HOB1=$O(^TONIDEMO(X,1,"B",HOB1)) Q:HOB1=""  D
 . S HOBDFN="" F  S HOBDFN=$O(^TONIDEMO(X,1,"B",HOB1,0)) Q:HOBDFN'=+HOBDFN  S HOBBY=$P(^TONIDEMO(X,1,HOBDFN,0),U),HOBBPRNT=$P(^TONIHOBB(HOBBY,0),U) Q
 Q
