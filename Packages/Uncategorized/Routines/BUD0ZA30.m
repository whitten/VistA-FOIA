BUD0ZA30 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON DEC 13, 2010;
 ;;5.0;IHS/RPMS UNIFORM DATA SYSTEM;;JAN 18, 2011;Build 12
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"1062,54868-3319-03 ",.01)
 ;;54868-3319-03
 ;;9002226.02101,"1062,54868-3319-03 ",.02)
 ;;54868-3319-03
 ;;9002226.02101,"1062,54868-3319-04 ",.01)
 ;;54868-3319-04
 ;;9002226.02101,"1062,54868-3319-04 ",.02)
 ;;54868-3319-04
 ;;9002226.02101,"1062,54868-3319-05 ",.01)
 ;;54868-3319-05
 ;;9002226.02101,"1062,54868-3319-05 ",.02)
 ;;54868-3319-05
 ;;9002226.02101,"1062,54868-3319-06 ",.01)
 ;;54868-3319-06
 ;;9002226.02101,"1062,54868-3319-06 ",.02)
 ;;54868-3319-06
 ;;9002226.02101,"1062,54868-3327-00 ",.01)
 ;;54868-3327-00
 ;;9002226.02101,"1062,54868-3327-00 ",.02)
 ;;54868-3327-00
 ;;9002226.02101,"1062,54868-3334-00 ",.01)
 ;;54868-3334-00
 ;;9002226.02101,"1062,54868-3334-00 ",.02)
 ;;54868-3334-00
 ;;9002226.02101,"1062,54868-3334-01 ",.01)
 ;;54868-3334-01
 ;;9002226.02101,"1062,54868-3334-01 ",.02)
 ;;54868-3334-01
 ;;9002226.02101,"1062,54868-3334-02 ",.01)
 ;;54868-3334-02
 ;;9002226.02101,"1062,54868-3334-02 ",.02)
 ;;54868-3334-02
 ;;9002226.02101,"1062,54868-3334-03 ",.01)
 ;;54868-3334-03
 ;;9002226.02101,"1062,54868-3334-03 ",.02)
 ;;54868-3334-03
 ;;9002226.02101,"1062,54868-3334-04 ",.01)
 ;;54868-3334-04
 ;;9002226.02101,"1062,54868-3334-04 ",.02)
 ;;54868-3334-04
 ;;9002226.02101,"1062,54868-3335-00 ",.01)
 ;;54868-3335-00
 ;;9002226.02101,"1062,54868-3335-00 ",.02)
 ;;54868-3335-00
 ;;9002226.02101,"1062,54868-3335-01 ",.01)
 ;;54868-3335-01
 ;;9002226.02101,"1062,54868-3335-01 ",.02)
 ;;54868-3335-01
 ;;9002226.02101,"1062,54868-3335-02 ",.01)
 ;;54868-3335-02
 ;;9002226.02101,"1062,54868-3335-02 ",.02)
 ;;54868-3335-02
 ;;9002226.02101,"1062,54868-3335-03 ",.01)
 ;;54868-3335-03
 ;;9002226.02101,"1062,54868-3335-03 ",.02)
 ;;54868-3335-03
 ;;9002226.02101,"1062,54868-3377-00 ",.01)
 ;;54868-3377-00
 ;;9002226.02101,"1062,54868-3377-00 ",.02)
 ;;54868-3377-00
 ;;9002226.02101,"1062,54868-3377-01 ",.01)
 ;;54868-3377-01
 ;;9002226.02101,"1062,54868-3377-01 ",.02)
 ;;54868-3377-01
 ;;9002226.02101,"1062,54868-3377-02 ",.01)
 ;;54868-3377-02
 ;;9002226.02101,"1062,54868-3377-02 ",.02)
 ;;54868-3377-02
 ;;9002226.02101,"1062,54868-3426-00 ",.01)
 ;;54868-3426-00
 ;;9002226.02101,"1062,54868-3426-00 ",.02)
 ;;54868-3426-00
 ;;9002226.02101,"1062,54868-3426-01 ",.01)
 ;;54868-3426-01
 ;;9002226.02101,"1062,54868-3426-01 ",.02)
 ;;54868-3426-01
 ;;9002226.02101,"1062,54868-3474-00 ",.01)
 ;;54868-3474-00
 ;;9002226.02101,"1062,54868-3474-00 ",.02)
 ;;54868-3474-00
 ;;9002226.02101,"1062,54868-3598-00 ",.01)
 ;;54868-3598-00
 ;;9002226.02101,"1062,54868-3598-00 ",.02)
 ;;54868-3598-00
 ;;9002226.02101,"1062,54868-3619-00 ",.01)
 ;;54868-3619-00
 ;;9002226.02101,"1062,54868-3619-00 ",.02)
 ;;54868-3619-00
 ;;9002226.02101,"1062,54868-3711-00 ",.01)
 ;;54868-3711-00
 ;;9002226.02101,"1062,54868-3711-00 ",.02)
 ;;54868-3711-00
 ;;9002226.02101,"1062,54868-3711-01 ",.01)
 ;;54868-3711-01
 ;;9002226.02101,"1062,54868-3711-01 ",.02)
 ;;54868-3711-01
 ;;9002226.02101,"1062,54868-3823-01 ",.01)
 ;;54868-3823-01
 ;;9002226.02101,"1062,54868-3823-01 ",.02)
 ;;54868-3823-01
 ;;9002226.02101,"1062,54868-4091-00 ",.01)
 ;;54868-4091-00
 ;;9002226.02101,"1062,54868-4091-00 ",.02)
 ;;54868-4091-00
 ;;9002226.02101,"1062,54868-4091-01 ",.01)
 ;;54868-4091-01
 ;;9002226.02101,"1062,54868-4091-01 ",.02)
 ;;54868-4091-01
 ;;9002226.02101,"1062,54868-4091-02 ",.01)
 ;;54868-4091-02
 ;;9002226.02101,"1062,54868-4091-02 ",.02)
 ;;54868-4091-02
 ;;9002226.02101,"1062,54868-4198-00 ",.01)
 ;;54868-4198-00
 ;;9002226.02101,"1062,54868-4198-00 ",.02)
 ;;54868-4198-00
 ;;9002226.02101,"1062,54868-4198-01 ",.01)
 ;;54868-4198-01
 ;;9002226.02101,"1062,54868-4198-01 ",.02)
 ;;54868-4198-01
 ;;9002226.02101,"1062,54868-4203-00 ",.01)
 ;;54868-4203-00
 ;;9002226.02101,"1062,54868-4203-00 ",.02)
 ;;54868-4203-00
 ;;9002226.02101,"1062,54868-4205-00 ",.01)
 ;;54868-4205-00
 ;;9002226.02101,"1062,54868-4205-00 ",.02)
 ;;54868-4205-00
 ;;9002226.02101,"1062,54868-4205-01 ",.01)
 ;;54868-4205-01
 ;;9002226.02101,"1062,54868-4205-01 ",.02)
 ;;54868-4205-01
 ;;9002226.02101,"1062,54868-4205-02 ",.01)
 ;;54868-4205-02
 ;;9002226.02101,"1062,54868-4205-02 ",.02)
 ;;54868-4205-02
 ;;9002226.02101,"1062,54868-4206-00 ",.01)
 ;;54868-4206-00
 ;;9002226.02101,"1062,54868-4206-00 ",.02)
 ;;54868-4206-00
 ;;9002226.02101,"1062,54868-4206-03 ",.01)
 ;;54868-4206-03
 ;;9002226.02101,"1062,54868-4206-03 ",.02)
 ;;54868-4206-03
 ;;9002226.02101,"1062,54868-4206-04 ",.01)
 ;;54868-4206-04
 ;;9002226.02101,"1062,54868-4206-04 ",.02)
 ;;54868-4206-04
 ;;9002226.02101,"1062,54868-4221-00 ",.01)
 ;;54868-4221-00
 ;;9002226.02101,"1062,54868-4221-00 ",.02)
 ;;54868-4221-00
 ;;9002226.02101,"1062,54868-4343-00 ",.01)
 ;;54868-4343-00
 ;;9002226.02101,"1062,54868-4343-00 ",.02)
 ;;54868-4343-00
 ;;9002226.02101,"1062,54868-4354-00 ",.01)
 ;;54868-4354-00
 ;;9002226.02101,"1062,54868-4354-00 ",.02)
 ;;54868-4354-00
 ;;9002226.02101,"1062,54868-4354-01 ",.01)
 ;;54868-4354-01
 ;;9002226.02101,"1062,54868-4354-01 ",.02)
 ;;54868-4354-01
 ;;9002226.02101,"1062,54868-4381-00 ",.01)
 ;;54868-4381-00
 ;;9002226.02101,"1062,54868-4381-00 ",.02)
 ;;54868-4381-00
 ;;9002226.02101,"1062,54868-4391-00 ",.01)
 ;;54868-4391-00
