BGP06M24 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"977,63874-0332-10 ",.02)
 ;;63874-0332-10
 ;;9002226.02101,"977,63874-0332-14 ",.01)
 ;;63874-0332-14
 ;;9002226.02101,"977,63874-0332-14 ",.02)
 ;;63874-0332-14
 ;;9002226.02101,"977,63874-0332-15 ",.01)
 ;;63874-0332-15
 ;;9002226.02101,"977,63874-0332-15 ",.02)
 ;;63874-0332-15
 ;;9002226.02101,"977,63874-0332-20 ",.01)
 ;;63874-0332-20
 ;;9002226.02101,"977,63874-0332-20 ",.02)
 ;;63874-0332-20
 ;;9002226.02101,"977,63874-0332-30 ",.01)
 ;;63874-0332-30
 ;;9002226.02101,"977,63874-0332-30 ",.02)
 ;;63874-0332-30
 ;;9002226.02101,"977,63874-0332-60 ",.01)
 ;;63874-0332-60
 ;;9002226.02101,"977,63874-0332-60 ",.02)
 ;;63874-0332-60
 ;;9002226.02101,"977,63874-0332-90 ",.01)
 ;;63874-0332-90
 ;;9002226.02101,"977,63874-0332-90 ",.02)
 ;;63874-0332-90
 ;;9002226.02101,"977,63874-0368-01 ",.01)
 ;;63874-0368-01
 ;;9002226.02101,"977,63874-0368-01 ",.02)
 ;;63874-0368-01
 ;;9002226.02101,"977,63874-0368-02 ",.01)
 ;;63874-0368-02
 ;;9002226.02101,"977,63874-0368-02 ",.02)
 ;;63874-0368-02
 ;;9002226.02101,"977,63874-0368-15 ",.01)
 ;;63874-0368-15
 ;;9002226.02101,"977,63874-0368-15 ",.02)
 ;;63874-0368-15
 ;;9002226.02101,"977,63874-0368-20 ",.01)
 ;;63874-0368-20
 ;;9002226.02101,"977,63874-0368-20 ",.02)
 ;;63874-0368-20
 ;;9002226.02101,"977,63874-0368-28 ",.01)
 ;;63874-0368-28
 ;;9002226.02101,"977,63874-0368-28 ",.02)
 ;;63874-0368-28
 ;;9002226.02101,"977,63874-0368-30 ",.01)
 ;;63874-0368-30
 ;;9002226.02101,"977,63874-0368-30 ",.02)
 ;;63874-0368-30
 ;;9002226.02101,"977,63874-0368-60 ",.01)
 ;;63874-0368-60
 ;;9002226.02101,"977,63874-0368-60 ",.02)
 ;;63874-0368-60
 ;;9002226.02101,"977,63874-0388-01 ",.01)
 ;;63874-0388-01
 ;;9002226.02101,"977,63874-0388-01 ",.02)
 ;;63874-0388-01
 ;;9002226.02101,"977,63874-0388-07 ",.01)
 ;;63874-0388-07
 ;;9002226.02101,"977,63874-0388-07 ",.02)
 ;;63874-0388-07
 ;;9002226.02101,"977,63874-0388-10 ",.01)
 ;;63874-0388-10
 ;;9002226.02101,"977,63874-0388-10 ",.02)
 ;;63874-0388-10
 ;;9002226.02101,"977,63874-0388-12 ",.01)
 ;;63874-0388-12
 ;;9002226.02101,"977,63874-0388-12 ",.02)
 ;;63874-0388-12
 ;;9002226.02101,"977,63874-0388-15 ",.01)
 ;;63874-0388-15
 ;;9002226.02101,"977,63874-0388-15 ",.02)
 ;;63874-0388-15
 ;;9002226.02101,"977,63874-0388-20 ",.01)
 ;;63874-0388-20
 ;;9002226.02101,"977,63874-0388-20 ",.02)
 ;;63874-0388-20
 ;;9002226.02101,"977,63874-0388-30 ",.01)
 ;;63874-0388-30
 ;;9002226.02101,"977,63874-0388-30 ",.02)
 ;;63874-0388-30
 ;;9002226.02101,"977,63874-0406-01 ",.01)
 ;;63874-0406-01
 ;;9002226.02101,"977,63874-0406-01 ",.02)
 ;;63874-0406-01
 ;;9002226.02101,"977,63874-0406-10 ",.01)
 ;;63874-0406-10
 ;;9002226.02101,"977,63874-0406-10 ",.02)
 ;;63874-0406-10
 ;;9002226.02101,"977,63874-0406-14 ",.01)
 ;;63874-0406-14
 ;;9002226.02101,"977,63874-0406-14 ",.02)
 ;;63874-0406-14
 ;;9002226.02101,"977,63874-0406-15 ",.01)
 ;;63874-0406-15
 ;;9002226.02101,"977,63874-0406-15 ",.02)
 ;;63874-0406-15
 ;;9002226.02101,"977,63874-0406-20 ",.01)
 ;;63874-0406-20
 ;;9002226.02101,"977,63874-0406-20 ",.02)
 ;;63874-0406-20
 ;;9002226.02101,"977,63874-0406-28 ",.01)
 ;;63874-0406-28
 ;;9002226.02101,"977,63874-0406-28 ",.02)
 ;;63874-0406-28
 ;;9002226.02101,"977,63874-0406-30 ",.01)
 ;;63874-0406-30
 ;;9002226.02101,"977,63874-0406-30 ",.02)
 ;;63874-0406-30
 ;;9002226.02101,"977,63874-0406-60 ",.01)
 ;;63874-0406-60
 ;;9002226.02101,"977,63874-0406-60 ",.02)
 ;;63874-0406-60
 ;;9002226.02101,"977,63874-0407-01 ",.01)
 ;;63874-0407-01
 ;;9002226.02101,"977,63874-0407-01 ",.02)
 ;;63874-0407-01
 ;;9002226.02101,"977,63874-0407-10 ",.01)
 ;;63874-0407-10
 ;;9002226.02101,"977,63874-0407-10 ",.02)
 ;;63874-0407-10
 ;;9002226.02101,"977,63874-0407-15 ",.01)
 ;;63874-0407-15
 ;;9002226.02101,"977,63874-0407-15 ",.02)
 ;;63874-0407-15
 ;;9002226.02101,"977,63874-0407-20 ",.01)
 ;;63874-0407-20
 ;;9002226.02101,"977,63874-0407-20 ",.02)
 ;;63874-0407-20
 ;;9002226.02101,"977,63874-0407-30 ",.01)
 ;;63874-0407-30
 ;;9002226.02101,"977,63874-0407-30 ",.02)
 ;;63874-0407-30
 ;;9002226.02101,"977,63874-0407-60 ",.01)
 ;;63874-0407-60
 ;;9002226.02101,"977,63874-0407-60 ",.02)
 ;;63874-0407-60
 ;;9002226.02101,"977,63874-0407-90 ",.01)
 ;;63874-0407-90
 ;;9002226.02101,"977,63874-0407-90 ",.02)
 ;;63874-0407-90
 ;;9002226.02101,"977,63874-0454-01 ",.01)
 ;;63874-0454-01
 ;;9002226.02101,"977,63874-0454-01 ",.02)
 ;;63874-0454-01
 ;;9002226.02101,"977,63874-0454-02 ",.01)
 ;;63874-0454-02
 ;;9002226.02101,"977,63874-0454-02 ",.02)
 ;;63874-0454-02
 ;;9002226.02101,"977,63874-0454-04 ",.01)
 ;;63874-0454-04
 ;;9002226.02101,"977,63874-0454-04 ",.02)
 ;;63874-0454-04
 ;;9002226.02101,"977,63874-0454-15 ",.01)
 ;;63874-0454-15
 ;;9002226.02101,"977,63874-0454-15 ",.02)
 ;;63874-0454-15
 ;;9002226.02101,"977,63874-0454-20 ",.01)
 ;;63874-0454-20
 ;;9002226.02101,"977,63874-0454-20 ",.02)
 ;;63874-0454-20
 ;;9002226.02101,"977,63874-0454-30 ",.01)
 ;;63874-0454-30
 ;;9002226.02101,"977,63874-0454-30 ",.02)
 ;;63874-0454-30
 ;;9002226.02101,"977,63874-0454-60 ",.01)
 ;;63874-0454-60
 ;;9002226.02101,"977,63874-0454-60 ",.02)
 ;;63874-0454-60
 ;;9002226.02101,"977,63874-0468-01 ",.01)
 ;;63874-0468-01
 ;;9002226.02101,"977,63874-0468-01 ",.02)
 ;;63874-0468-01
 ;;9002226.02101,"977,63874-0468-10 ",.01)
 ;;63874-0468-10
 ;;9002226.02101,"977,63874-0468-10 ",.02)
 ;;63874-0468-10
 ;;9002226.02101,"977,63874-0468-14 ",.01)
 ;;63874-0468-14
 ;;9002226.02101,"977,63874-0468-14 ",.02)
 ;;63874-0468-14
 ;;9002226.02101,"977,63874-0468-15 ",.01)
 ;;63874-0468-15
 ;;9002226.02101,"977,63874-0468-15 ",.02)
 ;;63874-0468-15
 ;;9002226.02101,"977,63874-0468-20 ",.01)
 ;;63874-0468-20
 ;;9002226.02101,"977,63874-0468-20 ",.02)
 ;;63874-0468-20
 ;;9002226.02101,"977,63874-0468-30 ",.01)
 ;;63874-0468-30
 ;;9002226.02101,"977,63874-0468-30 ",.02)
 ;;63874-0468-30
 ;;9002226.02101,"977,63874-0468-60 ",.01)
 ;;63874-0468-60
 ;;9002226.02101,"977,63874-0468-60 ",.02)
 ;;63874-0468-60
 ;;9002226.02101,"977,63874-0468-90 ",.01)
 ;;63874-0468-90
 ;;9002226.02101,"977,63874-0468-90 ",.02)
 ;;63874-0468-90
 ;;9002226.02101,"977,63874-0486-01 ",.01)
 ;;63874-0486-01
 ;;9002226.02101,"977,63874-0486-01 ",.02)
 ;;63874-0486-01
 ;;9002226.02101,"977,63874-0486-02 ",.01)
 ;;63874-0486-02
 ;;9002226.02101,"977,63874-0486-02 ",.02)
 ;;63874-0486-02
 ;;9002226.02101,"977,63874-0486-15 ",.01)
 ;;63874-0486-15
 ;;9002226.02101,"977,63874-0486-15 ",.02)
 ;;63874-0486-15
 ;;9002226.02101,"977,63874-0486-30 ",.01)
 ;;63874-0486-30
 ;;9002226.02101,"977,63874-0486-30 ",.02)
 ;;63874-0486-30
 ;;9002226.02101,"977,63874-0486-40 ",.01)
 ;;63874-0486-40
 ;;9002226.02101,"977,63874-0486-40 ",.02)
 ;;63874-0486-40
 ;;9002226.02101,"977,63874-0486-60 ",.01)
 ;;63874-0486-60
 ;;9002226.02101,"977,63874-0486-60 ",.02)
 ;;63874-0486-60
 ;;9002226.02101,"977,63874-0676-01 ",.01)
 ;;63874-0676-01
 ;;9002226.02101,"977,63874-0676-01 ",.02)
 ;;63874-0676-01
 ;;9002226.02101,"977,63874-0676-12 ",.01)
 ;;63874-0676-12
 ;;9002226.02101,"977,63874-0676-12 ",.02)
 ;;63874-0676-12
 ;;9002226.02101,"977,63874-0676-15 ",.01)
 ;;63874-0676-15
 ;;9002226.02101,"977,63874-0676-15 ",.02)
 ;;63874-0676-15
 ;;9002226.02101,"977,63874-0676-20 ",.01)
 ;;63874-0676-20
 ;;9002226.02101,"977,63874-0676-20 ",.02)
 ;;63874-0676-20
 ;;9002226.02101,"977,64376-0503-01 ",.01)
 ;;64376-0503-01
 ;;9002226.02101,"977,64376-0503-01 ",.02)
 ;;64376-0503-01
 ;;9002226.02101,"977,64376-0503-10 ",.01)
 ;;64376-0503-10
 ;;9002226.02101,"977,64376-0503-10 ",.02)
 ;;64376-0503-10
 ;;9002226.02101,"977,65162-0669-10 ",.01)
 ;;65162-0669-10
 ;;9002226.02101,"977,65162-0669-10 ",.02)
 ;;65162-0669-10
 ;;9002226.02101,"977,65162-0670-10 ",.01)
 ;;65162-0670-10
 ;;9002226.02101,"977,65162-0670-10 ",.02)
 ;;65162-0670-10
 ;;9002226.02101,"977,65162-0725-10 ",.01)
 ;;65162-0725-10
 ;;9002226.02101,"977,65162-0725-10 ",.02)
 ;;65162-0725-10
 ;;9002226.02101,"977,65162-0727-10 ",.01)
 ;;65162-0727-10
 ;;9002226.02101,"977,65162-0727-10 ",.02)
 ;;65162-0727-10
 ;;9002226.02101,"977,65162-0731-10 ",.01)
 ;;65162-0731-10
 ;;9002226.02101,"977,65162-0731-10 ",.02)
 ;;65162-0731-10
 ;;9002226.02101,"977,65483-0391-10 ",.01)
 ;;65483-0391-10
 ;;9002226.02101,"977,65483-0391-10 ",.02)
 ;;65483-0391-10
 ;;9002226.02101,"977,65483-0391-11 ",.01)
 ;;65483-0391-11
 ;;9002226.02101,"977,65483-0391-11 ",.02)
 ;;65483-0391-11
 ;;9002226.02101,"977,65483-0391-50 ",.01)
 ;;65483-0391-50
 ;;9002226.02101,"977,65483-0391-50 ",.02)
 ;;65483-0391-50
 ;;9002226.02101,"977,65483-0392-10 ",.01)
 ;;65483-0392-10
 ;;9002226.02101,"977,65483-0392-10 ",.02)
 ;;65483-0392-10
 ;;9002226.02101,"977,65483-0392-22 ",.01)
 ;;65483-0392-22
 ;;9002226.02101,"977,65483-0392-22 ",.02)
 ;;65483-0392-22
 ;;9002226.02101,"977,65483-0392-50 ",.01)
 ;;65483-0392-50
 ;;9002226.02101,"977,65483-0392-50 ",.02)
 ;;65483-0392-50
 ;;9002226.02101,"977,65483-0393-10 ",.01)
 ;;65483-0393-10
 ;;9002226.02101,"977,65483-0393-10 ",.02)
 ;;65483-0393-10
 ;;9002226.02101,"977,65483-0393-33 ",.01)
 ;;65483-0393-33
 ;;9002226.02101,"977,65483-0393-33 ",.02)
 ;;65483-0393-33
 ;;9002226.02101,"977,65483-0393-50 ",.01)
 ;;65483-0393-50
 ;;9002226.02101,"977,65483-0393-50 ",.02)
 ;;65483-0393-50
 ;;9002226.02101,"977,65726-0250-10 ",.01)
 ;;65726-0250-10
 ;;9002226.02101,"977,65726-0250-10 ",.02)
 ;;65726-0250-10
 ;;9002226.02101,"977,65726-0250-25 ",.01)
 ;;65726-0250-25
 ;;9002226.02101,"977,65726-0250-25 ",.02)
 ;;65726-0250-25
 ;;9002226.02101,"977,65726-0251-10 ",.01)
 ;;65726-0251-10
 ;;9002226.02101,"977,65726-0251-10 ",.02)
 ;;65726-0251-10
 ;;9002226.02101,"977,65726-0251-25 ",.01)
 ;;65726-0251-25
 ;;9002226.02101,"977,65726-0251-25 ",.02)
 ;;65726-0251-25
 ;;9002226.02101,"977,65862-0062-01 ",.01)
 ;;65862-0062-01
 ;;9002226.02101,"977,65862-0062-01 ",.02)
 ;;65862-0062-01
 ;;9002226.02101,"977,65862-0063-01 ",.01)
 ;;65862-0063-01
 ;;9002226.02101,"977,65862-0063-01 ",.02)
 ;;65862-0063-01
 ;;9002226.02101,"977,65862-0063-99 ",.01)
 ;;65862-0063-99
 ;;9002226.02101,"977,65862-0063-99 ",.02)
 ;;65862-0063-99
 ;;9002226.02101,"977,65862-0064-01 ",.01)
 ;;65862-0064-01
 ;;9002226.02101,"977,65862-0064-01 ",.02)
 ;;65862-0064-01
 ;;9002226.02101,"977,65862-0064-99 ",.01)
 ;;65862-0064-99
 ;;9002226.02101,"977,65862-0064-99 ",.02)
 ;;65862-0064-99
 ;;9002226.02101,"977,65862-0086-01 ",.01)
 ;;65862-0086-01
 ;;9002226.02101,"977,65862-0086-01 ",.02)
 ;;65862-0086-01
 ;;9002226.02101,"977,65862-0086-30 ",.01)
 ;;65862-0086-30
 ;;9002226.02101,"977,65862-0086-30 ",.02)
 ;;65862-0086-30
 ;;9002226.02101,"977,65862-0087-01 ",.01)
 ;;65862-0087-01
 ;;9002226.02101,"977,65862-0087-01 ",.02)
 ;;65862-0087-01
 ;;9002226.02101,"977,65862-0087-30 ",.01)
 ;;65862-0087-30
 ;;9002226.02101,"977,65862-0087-30 ",.02)
 ;;65862-0087-30
 ;;9002226.02101,"977,65862-0142-01 ",.01)
 ;;65862-0142-01
 ;;9002226.02101,"977,65862-0142-01 ",.02)
 ;;65862-0142-01
 ;;9002226.02101,"977,65862-0143-01 ",.01)
 ;;65862-0143-01
 ;;9002226.02101,"977,65862-0143-01 ",.02)
 ;;65862-0143-01
 ;;9002226.02101,"977,65862-0144-01 ",.01)
 ;;65862-0144-01
 ;;9002226.02101,"977,65862-0144-01 ",.02)
 ;;65862-0144-01
 ;;9002226.02101,"977,65862-0145-01 ",.01)
 ;;65862-0145-01
 ;;9002226.02101,"977,65862-0145-01 ",.02)
 ;;65862-0145-01
 ;;9002226.02101,"977,65862-0168-01 ",.01)
 ;;65862-0168-01
 ;;9002226.02101,"977,65862-0168-01 ",.02)
 ;;65862-0168-01
 ;;9002226.02101,"977,65862-0168-99 ",.01)
 ;;65862-0168-99
 ;;9002226.02101,"977,65862-0168-99 ",.02)
 ;;65862-0168-99
 ;;9002226.02101,"977,65862-0169-01 ",.01)
 ;;65862-0169-01
 ;;9002226.02101,"977,65862-0169-01 ",.02)
 ;;65862-0169-01
 ;;9002226.02101,"977,65862-0169-99 ",.01)
 ;;65862-0169-99
 ;;9002226.02101,"977,65862-0169-99 ",.02)
 ;;65862-0169-99
 ;;9002226.02101,"977,65862-0170-01 ",.01)
 ;;65862-0170-01
 ;;9002226.02101,"977,65862-0170-01 ",.02)
 ;;65862-0170-01
 ;;9002226.02101,"977,65862-0170-99 ",.01)
 ;;65862-0170-99
 ;;9002226.02101,"977,65862-0170-99 ",.02)
 ;;65862-0170-99
 ;;9002226.02101,"977,66105-0994-03 ",.01)
 ;;66105-0994-03
 ;;9002226.02101,"977,66105-0994-03 ",.02)
 ;;66105-0994-03
 ;;9002226.02101,"977,66105-0994-06 ",.01)
 ;;66105-0994-06
 ;;9002226.02101,"977,66105-0994-06 ",.02)
 ;;66105-0994-06
 ;;9002226.02101,"977,66105-0994-10 ",.01)
 ;;66105-0994-10
 ;;9002226.02101,"977,66105-0994-10 ",.02)
 ;;66105-0994-10
 ;;9002226.02101,"977,66105-0994-11 ",.01)
 ;;66105-0994-11
 ;;9002226.02101,"977,66105-0994-11 ",.02)
 ;;66105-0994-11
 ;;9002226.02101,"977,66105-0994-15 ",.01)
 ;;66105-0994-15
 ;;9002226.02101,"977,66105-0994-15 ",.02)
 ;;66105-0994-15
 ;;9002226.02101,"977,66105-0996-03 ",.01)
 ;;66105-0996-03
 ;;9002226.02101,"977,66105-0996-03 ",.02)
 ;;66105-0996-03
 ;;9002226.02101,"977,66105-0996-06 ",.01)
 ;;66105-0996-06
 ;;9002226.02101,"977,66105-0996-06 ",.02)
 ;;66105-0996-06
 ;;9002226.02101,"977,66105-0996-10 ",.01)
 ;;66105-0996-10
 ;;9002226.02101,"977,66105-0996-10 ",.02)
 ;;66105-0996-10
 ;;9002226.02101,"977,66105-0996-11 ",.01)
 ;;66105-0996-11
 ;;9002226.02101,"977,66105-0996-11 ",.02)
 ;;66105-0996-11
 ;;9002226.02101,"977,66105-0996-15 ",.01)
 ;;66105-0996-15
 ;;9002226.02101,"977,66105-0996-15 ",.02)
 ;;66105-0996-15
 ;;9002226.02101,"977,66116-0239-30 ",.01)
 ;;66116-0239-30
 ;;9002226.02101,"977,66116-0239-30 ",.02)
 ;;66116-0239-30
 ;;9002226.02101,"977,66116-0455-30 ",.01)
 ;;66116-0455-30
