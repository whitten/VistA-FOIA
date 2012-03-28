BGP0ZD13 ;IHS/CMI/LAB-CREATED BY ^ATXSTX ON MAY 23, 2010;
 ;;10.0;IHS CLINICAL REPORTING;;JUN 18, 2010
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"976,10019-0030-03 ",.02)
 ;;10019-0030-03
 ;;9002226.02101,"976,10019-0030-04 ",.01)
 ;;10019-0030-04
 ;;9002226.02101,"976,10019-0030-04 ",.02)
 ;;10019-0030-04
 ;;9002226.02101,"976,10768-7019-01 ",.01)
 ;;10768-7019-01
 ;;9002226.02101,"976,10768-7019-01 ",.02)
 ;;10768-7019-01
 ;;9002226.02101,"976,10768-7019-02 ",.01)
 ;;10768-7019-02
 ;;9002226.02101,"976,10768-7019-02 ",.02)
 ;;10768-7019-02
 ;;9002226.02101,"976,11584-0465-01 ",.01)
 ;;11584-0465-01
 ;;9002226.02101,"976,11584-0465-01 ",.02)
 ;;11584-0465-01
 ;;9002226.02101,"976,38779-0052-04 ",.01)
 ;;38779-0052-04
 ;;9002226.02101,"976,38779-0052-04 ",.02)
 ;;38779-0052-04
 ;;9002226.02101,"976,38779-0052-05 ",.01)
 ;;38779-0052-05
 ;;9002226.02101,"976,38779-0052-05 ",.02)
 ;;38779-0052-05
 ;;9002226.02101,"976,38779-0076-03 ",.01)
 ;;38779-0076-03
 ;;9002226.02101,"976,38779-0076-03 ",.02)
 ;;38779-0076-03
 ;;9002226.02101,"976,38779-0076-04 ",.01)
 ;;38779-0076-04
 ;;9002226.02101,"976,38779-0076-04 ",.02)
 ;;38779-0076-04
 ;;9002226.02101,"976,38779-0076-05 ",.01)
 ;;38779-0076-05
 ;;9002226.02101,"976,38779-0076-05 ",.02)
 ;;38779-0076-05
 ;;9002226.02101,"976,38779-0076-08 ",.01)
 ;;38779-0076-08
 ;;9002226.02101,"976,38779-0076-08 ",.02)
 ;;38779-0076-08
 ;;9002226.02101,"976,38779-0078-04 ",.01)
 ;;38779-0078-04
 ;;9002226.02101,"976,38779-0078-04 ",.02)
 ;;38779-0078-04
 ;;9002226.02101,"976,38779-0078-05 ",.01)
 ;;38779-0078-05
 ;;9002226.02101,"976,38779-0078-05 ",.02)
 ;;38779-0078-05
 ;;9002226.02101,"976,38779-0078-08 ",.01)
 ;;38779-0078-08
 ;;9002226.02101,"976,38779-0078-08 ",.02)
 ;;38779-0078-08
 ;;9002226.02101,"976,38779-0078-09 ",.01)
 ;;38779-0078-09
 ;;9002226.02101,"976,38779-0078-09 ",.02)
 ;;38779-0078-09
 ;;9002226.02101,"976,38779-0079-09 ",.01)
 ;;38779-0079-09
 ;;9002226.02101,"976,38779-0079-09 ",.02)
 ;;38779-0079-09
 ;;9002226.02101,"976,38779-0237-03 ",.01)
 ;;38779-0237-03
 ;;9002226.02101,"976,38779-0237-03 ",.02)
 ;;38779-0237-03
 ;;9002226.02101,"976,38779-0237-04 ",.01)
 ;;38779-0237-04
 ;;9002226.02101,"976,38779-0237-04 ",.02)
 ;;38779-0237-04
 ;;9002226.02101,"976,38779-0237-05 ",.01)
 ;;38779-0237-05
 ;;9002226.02101,"976,38779-0237-05 ",.02)
 ;;38779-0237-05
 ;;9002226.02101,"976,38779-0299-04 ",.01)
 ;;38779-0299-04
 ;;9002226.02101,"976,38779-0299-04 ",.02)
 ;;38779-0299-04
 ;;9002226.02101,"976,38779-0299-05 ",.01)
 ;;38779-0299-05
 ;;9002226.02101,"976,38779-0299-05 ",.02)
 ;;38779-0299-05
 ;;9002226.02101,"976,38779-0299-08 ",.01)
 ;;38779-0299-08
 ;;9002226.02101,"976,38779-0299-08 ",.02)
 ;;38779-0299-08
 ;;9002226.02101,"976,38779-0299-09 ",.01)
 ;;38779-0299-09
 ;;9002226.02101,"976,38779-0299-09 ",.02)
 ;;38779-0299-09
 ;;9002226.02101,"976,38779-0302-04 ",.01)
 ;;38779-0302-04
 ;;9002226.02101,"976,38779-0302-04 ",.02)
 ;;38779-0302-04
 ;;9002226.02101,"976,38779-0302-05 ",.01)
 ;;38779-0302-05
 ;;9002226.02101,"976,38779-0302-05 ",.02)
 ;;38779-0302-05
 ;;9002226.02101,"976,38779-0302-08 ",.01)
 ;;38779-0302-08
 ;;9002226.02101,"976,38779-0302-08 ",.02)
 ;;38779-0302-08
 ;;9002226.02101,"976,38779-0362-04 ",.01)
 ;;38779-0362-04
 ;;9002226.02101,"976,38779-0362-04 ",.02)
 ;;38779-0362-04
 ;;9002226.02101,"976,38779-0362-05 ",.01)
 ;;38779-0362-05
 ;;9002226.02101,"976,38779-0362-05 ",.02)
 ;;38779-0362-05
 ;;9002226.02101,"976,38779-0362-08 ",.01)
 ;;38779-0362-08
 ;;9002226.02101,"976,38779-0362-08 ",.02)
 ;;38779-0362-08
 ;;9002226.02101,"976,38779-0484-04 ",.01)
 ;;38779-0484-04
 ;;9002226.02101,"976,38779-0484-04 ",.02)
 ;;38779-0484-04
 ;;9002226.02101,"976,38779-0484-05 ",.01)
 ;;38779-0484-05
 ;;9002226.02101,"976,38779-0484-05 ",.02)
 ;;38779-0484-05
 ;;9002226.02101,"976,38779-0484-08 ",.01)
 ;;38779-0484-08
 ;;9002226.02101,"976,38779-0484-08 ",.02)
 ;;38779-0484-08
 ;;9002226.02101,"976,38779-0548-04 ",.01)
 ;;38779-0548-04
 ;;9002226.02101,"976,38779-0548-04 ",.02)
 ;;38779-0548-04
 ;;9002226.02101,"976,38779-0548-05 ",.01)
 ;;38779-0548-05
 ;;9002226.02101,"976,38779-0548-05 ",.02)
 ;;38779-0548-05
 ;;9002226.02101,"976,38779-0548-08 ",.01)
 ;;38779-0548-08
 ;;9002226.02101,"976,38779-0548-08 ",.02)
 ;;38779-0548-08
 ;;9002226.02101,"976,38779-0548-09 ",.01)
 ;;38779-0548-09
 ;;9002226.02101,"976,38779-0548-09 ",.02)
 ;;38779-0548-09
 ;;9002226.02101,"976,38779-1698-04 ",.01)
 ;;38779-1698-04
 ;;9002226.02101,"976,38779-1698-04 ",.02)
 ;;38779-1698-04
 ;;9002226.02101,"976,38779-1698-05 ",.01)
 ;;38779-1698-05
 ;;9002226.02101,"976,38779-1698-05 ",.02)
 ;;38779-1698-05
 ;;9002226.02101,"976,38779-1698-08 ",.01)
 ;;38779-1698-08
 ;;9002226.02101,"976,38779-1698-08 ",.02)
 ;;38779-1698-08
 ;;9002226.02101,"976,49452-2583-01 ",.01)
 ;;49452-2583-01
 ;;9002226.02101,"976,49452-2583-01 ",.02)
 ;;49452-2583-01
 ;;9002226.02101,"976,49452-2583-02 ",.01)
 ;;49452-2583-02
 ;;9002226.02101,"976,49452-2583-02 ",.02)
 ;;49452-2583-02
 ;;9002226.02101,"976,49452-2597-01 ",.01)
 ;;49452-2597-01
 ;;9002226.02101,"976,49452-2597-01 ",.02)
 ;;49452-2597-01
 ;;9002226.02101,"976,49452-2597-02 ",.01)
 ;;49452-2597-02
 ;;9002226.02101,"976,49452-2597-02 ",.02)
 ;;49452-2597-02
 ;;9002226.02101,"976,49452-2597-03 ",.01)
 ;;49452-2597-03
 ;;9002226.02101,"976,49452-2597-03 ",.02)
 ;;49452-2597-03
 ;;9002226.02101,"976,49452-3178-01 ",.01)
 ;;49452-3178-01
 ;;9002226.02101,"976,49452-3178-01 ",.02)
 ;;49452-3178-01
 ;;9002226.02101,"976,49452-3178-02 ",.01)
 ;;49452-3178-02
 ;;9002226.02101,"976,49452-3178-02 ",.02)
 ;;49452-3178-02
 ;;9002226.02101,"976,49452-3178-03 ",.01)
 ;;49452-3178-03
 ;;9002226.02101,"976,49452-3178-03 ",.02)
 ;;49452-3178-03
 ;;9002226.02101,"976,49452-3657-01 ",.01)
 ;;49452-3657-01
 ;;9002226.02101,"976,49452-3657-01 ",.02)
 ;;49452-3657-01
 ;;9002226.02101,"976,49452-3657-02 ",.01)
 ;;49452-3657-02
 ;;9002226.02101,"976,49452-3657-02 ",.02)
 ;;49452-3657-02
 ;;9002226.02101,"976,49452-3657-03 ",.01)
 ;;49452-3657-03
 ;;9002226.02101,"976,49452-3657-03 ",.02)
 ;;49452-3657-03
 ;;9002226.02101,"976,49452-3657-04 ",.01)
 ;;49452-3657-04
 ;;9002226.02101,"976,49452-3657-04 ",.02)
 ;;49452-3657-04
 ;;9002226.02101,"976,49452-3657-05 ",.01)
 ;;49452-3657-05
 ;;9002226.02101,"976,49452-3657-05 ",.02)
 ;;49452-3657-05
 ;;9002226.02101,"976,49452-3670-01 ",.01)
 ;;49452-3670-01
 ;;9002226.02101,"976,49452-3670-01 ",.02)
 ;;49452-3670-01
 ;;9002226.02101,"976,49452-3670-02 ",.01)
 ;;49452-3670-02
 ;;9002226.02101,"976,49452-3670-02 ",.02)
 ;;49452-3670-02
 ;;9002226.02101,"976,49452-3670-03 ",.01)
 ;;49452-3670-03
 ;;9002226.02101,"976,49452-3670-03 ",.02)
 ;;49452-3670-03
 ;;9002226.02101,"976,49452-3916-02 ",.01)
 ;;49452-3916-02
 ;;9002226.02101,"976,49452-3916-02 ",.02)
 ;;49452-3916-02
 ;;9002226.02101,"976,49452-3916-03 ",.01)
 ;;49452-3916-03
 ;;9002226.02101,"976,49452-3916-03 ",.02)
 ;;49452-3916-03
 ;;9002226.02101,"976,49452-3916-04 ",.01)
 ;;49452-3916-04
 ;;9002226.02101,"976,49452-3916-04 ",.02)
 ;;49452-3916-04
 ;;9002226.02101,"976,49452-3916-06 ",.01)
 ;;49452-3916-06
 ;;9002226.02101,"976,49452-3916-06 ",.02)
 ;;49452-3916-06
 ;;9002226.02101,"976,49452-3917-02 ",.01)
 ;;49452-3917-02
 ;;9002226.02101,"976,49452-3917-02 ",.02)
 ;;49452-3917-02
 ;;9002226.02101,"976,49452-3917-03 ",.01)
 ;;49452-3917-03
 ;;9002226.02101,"976,49452-3917-03 ",.02)
 ;;49452-3917-03
 ;;9002226.02101,"976,49452-3917-04 ",.01)
 ;;49452-3917-04
 ;;9002226.02101,"976,49452-3917-04 ",.02)
 ;;49452-3917-04
 ;;9002226.02101,"976,49452-3917-05 ",.01)
 ;;49452-3917-05
 ;;9002226.02101,"976,49452-3917-05 ",.02)
 ;;49452-3917-05
 ;;9002226.02101,"976,49452-3919-01 ",.01)
 ;;49452-3919-01
 ;;9002226.02101,"976,49452-3919-01 ",.02)
 ;;49452-3919-01
 ;;9002226.02101,"976,49452-3919-02 ",.01)
 ;;49452-3919-02
 ;;9002226.02101,"976,49452-3919-02 ",.02)
 ;;49452-3919-02
 ;;9002226.02101,"976,49452-3919-03 ",.01)
 ;;49452-3919-03
 ;;9002226.02101,"976,49452-3919-03 ",.02)
 ;;49452-3919-03
 ;;9002226.02101,"976,49452-3919-05 ",.01)
 ;;49452-3919-05
 ;;9002226.02101,"976,49452-3919-05 ",.02)
 ;;49452-3919-05
 ;;9002226.02101,"976,49452-4815-01 ",.01)
 ;;49452-4815-01
 ;;9002226.02101,"976,49452-4815-01 ",.02)
 ;;49452-4815-01
 ;;9002226.02101,"976,49452-4815-02 ",.01)
 ;;49452-4815-02
 ;;9002226.02101,"976,49452-4815-02 ",.02)
 ;;49452-4815-02
 ;;9002226.02101,"976,49452-4815-03 ",.01)
 ;;49452-4815-03
 ;;9002226.02101,"976,49452-4815-03 ",.02)
 ;;49452-4815-03
 ;;9002226.02101,"976,49452-4815-04 ",.01)
 ;;49452-4815-04
 ;;9002226.02101,"976,49452-4815-04 ",.02)
 ;;49452-4815-04
 ;;9002226.02101,"976,49452-4815-05 ",.01)
 ;;49452-4815-05
 ;;9002226.02101,"976,49452-4815-05 ",.02)
 ;;49452-4815-05
 ;;9002226.02101,"976,49452-4817-01 ",.01)
 ;;49452-4817-01
 ;;9002226.02101,"976,49452-4817-01 ",.02)
 ;;49452-4817-01
 ;;9002226.02101,"976,49452-4817-02 ",.01)
 ;;49452-4817-02
 ;;9002226.02101,"976,49452-4817-02 ",.02)
 ;;49452-4817-02
 ;;9002226.02101,"976,49452-4817-03 ",.01)
 ;;49452-4817-03
 ;;9002226.02101,"976,49452-4817-03 ",.02)
 ;;49452-4817-03
 ;;9002226.02101,"976,49452-4817-04 ",.01)
 ;;49452-4817-04
 ;;9002226.02101,"976,49452-4817-04 ",.02)
 ;;49452-4817-04
 ;;9002226.02101,"976,49452-4817-06 ",.01)
 ;;49452-4817-06
 ;;9002226.02101,"976,49452-4817-06 ",.02)
 ;;49452-4817-06
 ;;9002226.02101,"976,49452-5476-01 ",.01)
 ;;49452-5476-01
 ;;9002226.02101,"976,49452-5476-01 ",.02)
 ;;49452-5476-01
 ;;9002226.02101,"976,49452-5476-02 ",.01)
 ;;49452-5476-02
 ;;9002226.02101,"976,49452-5476-02 ",.02)
 ;;49452-5476-02
 ;;9002226.02101,"976,49452-5476-03 ",.01)
 ;;49452-5476-03
 ;;9002226.02101,"976,49452-5476-03 ",.02)
 ;;49452-5476-03
 ;;9002226.02101,"976,49452-7583-01 ",.01)
 ;;49452-7583-01
 ;;9002226.02101,"976,49452-7583-01 ",.02)
 ;;49452-7583-01
 ;;9002226.02101,"976,49452-7583-02 ",.01)
 ;;49452-7583-02
 ;;9002226.02101,"976,49452-7583-02 ",.02)
 ;;49452-7583-02
 ;;9002226.02101,"976,49452-7583-03 ",.01)
 ;;49452-7583-03
 ;;9002226.02101,"976,49452-7583-03 ",.02)
 ;;49452-7583-03
 ;;9002226.02101,"976,49452-7832-05 ",.01)
 ;;49452-7832-05
 ;;9002226.02101,"976,49452-7832-05 ",.02)
 ;;49452-7832-05
 ;;9002226.02101,"976,49452-7834-01 ",.01)
 ;;49452-7834-01
 ;;9002226.02101,"976,49452-7834-01 ",.02)
 ;;49452-7834-01
 ;;9002226.02101,"976,49452-7834-04 ",.01)
 ;;49452-7834-04
 ;;9002226.02101,"976,49452-7834-04 ",.02)
 ;;49452-7834-04
 ;;9002226.02101,"976,49884-0596-01 ",.01)
 ;;49884-0596-01
 ;;9002226.02101,"976,49884-0596-01 ",.02)
 ;;49884-0596-01
 ;;9002226.02101,"976,49884-0723-01 ",.01)
 ;;49884-0723-01
 ;;9002226.02101,"976,49884-0723-01 ",.02)
 ;;49884-0723-01
 ;;9002226.02101,"976,49884-0723-05 ",.01)
 ;;49884-0723-05
 ;;9002226.02101,"976,49884-0723-05 ",.02)
 ;;49884-0723-05
 ;;9002226.02101,"976,49884-0777-01 ",.01)
 ;;49884-0777-01
 ;;9002226.02101,"976,49884-0777-01 ",.02)
 ;;49884-0777-01
 ;;9002226.02101,"976,49884-0777-05 ",.01)
 ;;49884-0777-05
 ;;9002226.02101,"976,49884-0777-05 ",.02)
 ;;49884-0777-05
 ;;9002226.02101,"976,49884-0778-01 ",.01)
 ;;49884-0778-01
 ;;9002226.02101,"976,49884-0778-01 ",.02)
 ;;49884-0778-01
 ;;9002226.02101,"976,49884-0778-05 ",.01)
 ;;49884-0778-05
 ;;9002226.02101,"976,49884-0778-05 ",.02)
 ;;49884-0778-05
 ;;9002226.02101,"976,49884-0779-01 ",.01)
 ;;49884-0779-01
 ;;9002226.02101,"976,49884-0779-01 ",.02)
 ;;49884-0779-01
 ;;9002226.02101,"976,49884-0779-05 ",.01)
 ;;49884-0779-05
 ;;9002226.02101,"976,49884-0779-05 ",.02)
 ;;49884-0779-05
 ;;9002226.02101,"976,49999-0004-10 ",.01)
 ;;49999-0004-10
 ;;9002226.02101,"976,49999-0004-10 ",.02)
 ;;49999-0004-10
 ;;9002226.02101,"976,49999-0004-20 ",.01)
 ;;49999-0004-20
 ;;9002226.02101,"976,49999-0004-20 ",.02)
 ;;49999-0004-20
 ;;9002226.02101,"976,49999-0004-30 ",.01)
 ;;49999-0004-30
 ;;9002226.02101,"976,49999-0004-30 ",.02)
 ;;49999-0004-30
 ;;9002226.02101,"976,49999-0006-06 ",.01)
 ;;49999-0006-06
 ;;9002226.02101,"976,49999-0006-06 ",.02)
 ;;49999-0006-06
 ;;9002226.02101,"976,49999-0006-15 ",.01)
 ;;49999-0006-15
 ;;9002226.02101,"976,49999-0006-15 ",.02)
 ;;49999-0006-15
 ;;9002226.02101,"976,49999-0006-20 ",.01)
 ;;49999-0006-20
 ;;9002226.02101,"976,49999-0006-20 ",.02)
 ;;49999-0006-20
 ;;9002226.02101,"976,49999-0006-21 ",.01)
 ;;49999-0006-21
 ;;9002226.02101,"976,49999-0006-21 ",.02)
 ;;49999-0006-21
 ;;9002226.02101,"976,49999-0006-28 ",.01)
 ;;49999-0006-28
 ;;9002226.02101,"976,49999-0006-28 ",.02)
 ;;49999-0006-28
 ;;9002226.02101,"976,49999-0006-30 ",.01)
 ;;49999-0006-30
 ;;9002226.02101,"976,49999-0006-30 ",.02)
 ;;49999-0006-30
 ;;9002226.02101,"976,49999-0006-40 ",.01)
 ;;49999-0006-40
 ;;9002226.02101,"976,49999-0006-40 ",.02)
 ;;49999-0006-40
 ;;9002226.02101,"976,49999-0006-60 ",.01)
 ;;49999-0006-60
 ;;9002226.02101,"976,49999-0006-60 ",.02)
 ;;49999-0006-60
 ;;9002226.02101,"976,49999-0009-10 ",.01)
 ;;49999-0009-10
 ;;9002226.02101,"976,49999-0009-10 ",.02)
 ;;49999-0009-10
 ;;9002226.02101,"976,49999-0009-14 ",.01)
 ;;49999-0009-14
 ;;9002226.02101,"976,49999-0009-14 ",.02)
 ;;49999-0009-14
 ;;9002226.02101,"976,49999-0009-15 ",.01)
 ;;49999-0009-15
 ;;9002226.02101,"976,49999-0009-15 ",.02)
 ;;49999-0009-15
 ;;9002226.02101,"976,49999-0009-20 ",.01)
 ;;49999-0009-20
 ;;9002226.02101,"976,49999-0009-20 ",.02)
 ;;49999-0009-20
 ;;9002226.02101,"976,49999-0009-30 ",.01)
 ;;49999-0009-30
 ;;9002226.02101,"976,49999-0009-30 ",.02)
 ;;49999-0009-30
 ;;9002226.02101,"976,49999-0009-42 ",.01)
 ;;49999-0009-42
