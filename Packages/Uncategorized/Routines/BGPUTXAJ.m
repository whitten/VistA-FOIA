BGPUTXAJ ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 21, 2005 ;
 ;;7.0;IHS CLINICAL REPORTING;;JAN 24, 2007
 ;;;BGPU;;APR 21, 2005
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"393,38245-0731-10 ",.01)
 ;;38245-0731-10
 ;;9002226.02101,"393,38245-0731-10 ",.02)
 ;;38245-0731-10
 ;;9002226.02101,"393,49884-0555-01 ",.01)
 ;;49884-0555-01
 ;;9002226.02101,"393,49884-0555-01 ",.02)
 ;;49884-0555-01
 ;;9002226.02101,"393,49884-0582-01 ",.01)
 ;;49884-0582-01
 ;;9002226.02101,"393,49884-0582-01 ",.02)
 ;;49884-0582-01
 ;;9002226.02101,"393,49884-0582-10 ",.01)
 ;;49884-0582-10
 ;;9002226.02101,"393,49884-0582-10 ",.02)
 ;;49884-0582-10
 ;;9002226.02101,"393,49884-0583-01 ",.01)
 ;;49884-0583-01
 ;;9002226.02101,"393,49884-0583-01 ",.02)
 ;;49884-0583-01
 ;;9002226.02101,"393,49884-0583-10 ",.01)
 ;;49884-0583-10
 ;;9002226.02101,"393,49884-0583-10 ",.02)
 ;;49884-0583-10
 ;;9002226.02101,"393,49884-0584-01 ",.01)
 ;;49884-0584-01
 ;;9002226.02101,"393,49884-0584-01 ",.02)
 ;;49884-0584-01
 ;;9002226.02101,"393,49884-0584-10 ",.01)
 ;;49884-0584-10
 ;;9002226.02101,"393,49884-0584-10 ",.02)
 ;;49884-0584-10
 ;;9002226.02101,"393,49884-0585-01 ",.01)
 ;;49884-0585-01
 ;;9002226.02101,"393,49884-0585-01 ",.02)
 ;;49884-0585-01
 ;;9002226.02101,"393,49884-0585-10 ",.01)
 ;;49884-0585-10
 ;;9002226.02101,"393,49884-0585-10 ",.02)
 ;;49884-0585-10
 ;;9002226.02101,"393,49884-0587-01 ",.01)
 ;;49884-0587-01
 ;;9002226.02101,"393,49884-0587-01 ",.02)
 ;;49884-0587-01
 ;;9002226.02101,"393,50111-0467-01 ",.01)
 ;;50111-0467-01
 ;;9002226.02101,"393,50111-0467-01 ",.02)
 ;;50111-0467-01
 ;;9002226.02101,"393,50111-0467-03 ",.01)
 ;;50111-0467-03
 ;;9002226.02101,"393,50111-0467-03 ",.02)
 ;;50111-0467-03
 ;;9002226.02101,"393,50111-0468-01 ",.01)
 ;;50111-0468-01
 ;;9002226.02101,"393,50111-0468-01 ",.02)
 ;;50111-0468-01
 ;;9002226.02101,"393,50111-0468-03 ",.01)
 ;;50111-0468-03
 ;;9002226.02101,"393,50111-0468-03 ",.02)
 ;;50111-0468-03
 ;;9002226.02101,"393,50111-0470-01 ",.01)
 ;;50111-0470-01
 ;;9002226.02101,"393,50111-0470-01 ",.02)
 ;;50111-0470-01
 ;;9002226.02101,"393,50111-0471-01 ",.01)
 ;;50111-0471-01
 ;;9002226.02101,"393,50111-0471-01 ",.02)
 ;;50111-0471-01
 ;;9002226.02101,"393,50111-0471-02 ",.01)
 ;;50111-0471-02
 ;;9002226.02101,"393,50111-0471-02 ",.02)
 ;;50111-0471-02
 ;;9002226.02101,"393,50419-0105-10 ",.01)
 ;;50419-0105-10
 ;;9002226.02101,"393,50419-0105-10 ",.02)
 ;;50419-0105-10
 ;;9002226.02101,"393,50419-0106-10 ",.01)
 ;;50419-0106-10
 ;;9002226.02101,"393,50419-0106-10 ",.02)
 ;;50419-0106-10
 ;;9002226.02101,"393,50419-0106-11 ",.01)
 ;;50419-0106-11
 ;;9002226.02101,"393,50419-0106-11 ",.02)
 ;;50419-0106-11
 ;;9002226.02101,"393,50419-0107-10 ",.01)
 ;;50419-0107-10
 ;;9002226.02101,"393,50419-0107-10 ",.02)
 ;;50419-0107-10
 ;;9002226.02101,"393,50419-0107-11 ",.01)
 ;;50419-0107-11
 ;;9002226.02101,"393,50419-0107-11 ",.02)
 ;;50419-0107-11
 ;;9002226.02101,"393,50419-0109-10 ",.01)
 ;;50419-0109-10
 ;;9002226.02101,"393,50419-0109-10 ",.02)
 ;;50419-0109-10
 ;;9002226.02101,"393,50419-0109-11 ",.01)
 ;;50419-0109-11
 ;;9002226.02101,"393,50419-0109-11 ",.02)
 ;;50419-0109-11
 ;;9002226.02101,"393,50419-0115-06 ",.01)
 ;;50419-0115-06
 ;;9002226.02101,"393,50419-0115-06 ",.02)
 ;;50419-0115-06
 ;;9002226.02101,"393,50419-0115-11 ",.01)
 ;;50419-0115-11
 ;;9002226.02101,"393,50419-0115-11 ",.02)
 ;;50419-0115-11
 ;;9002226.02101,"393,50419-0116-06 ",.01)
 ;;50419-0116-06
 ;;9002226.02101,"393,50419-0116-06 ",.02)
 ;;50419-0116-06
 ;;9002226.02101,"393,50419-0116-11 ",.01)
 ;;50419-0116-11
 ;;9002226.02101,"393,50419-0116-11 ",.02)
 ;;50419-0116-11
 ;;9002226.02101,"393,50419-0119-06 ",.01)
 ;;50419-0119-06
 ;;9002226.02101,"393,50419-0119-06 ",.02)
 ;;50419-0119-06
 ;;9002226.02101,"393,50419-0119-11 ",.01)
 ;;50419-0119-11
 ;;9002226.02101,"393,50419-0119-11 ",.02)
 ;;50419-0119-11
 ;;9002226.02101,"393,51079-0277-19 ",.01)
 ;;51079-0277-19
 ;;9002226.02101,"393,51079-0277-19 ",.02)
 ;;51079-0277-19
 ;;9002226.02101,"393,51079-0277-20 ",.01)
 ;;51079-0277-20
 ;;9002226.02101,"393,51079-0277-20 ",.02)
 ;;51079-0277-20
 ;;9002226.02101,"393,51079-0278-20 ",.01)
 ;;51079-0278-20
 ;;9002226.02101,"393,51079-0278-20 ",.02)
 ;;51079-0278-20
 ;;9002226.02101,"393,51079-0279-20 ",.01)
 ;;51079-0279-20
 ;;9002226.02101,"393,51079-0279-20 ",.02)
 ;;51079-0279-20
 ;;9002226.02101,"393,51079-0280-20 ",.01)
 ;;51079-0280-20
 ;;9002226.02101,"393,51079-0280-20 ",.02)
 ;;51079-0280-20
 ;;9002226.02101,"393,51079-0684-19 ",.01)
 ;;51079-0684-19
 ;;9002226.02101,"393,51079-0684-19 ",.02)
 ;;51079-0684-19
 ;;9002226.02101,"393,51079-0684-20 ",.01)
 ;;51079-0684-20
 ;;9002226.02101,"393,51079-0684-20 ",.02)
 ;;51079-0684-20
 ;;9002226.02101,"393,51079-0685-20 ",.01)
 ;;51079-0685-20
 ;;9002226.02101,"393,51079-0685-20 ",.02)
 ;;51079-0685-20
 ;;9002226.02101,"393,51079-0759-19 ",.01)
 ;;51079-0759-19
 ;;9002226.02101,"393,51079-0759-19 ",.02)
 ;;51079-0759-19
 ;;9002226.02101,"393,51079-0801-19 ",.01)
 ;;51079-0801-19
 ;;9002226.02101,"393,51079-0801-19 ",.02)
 ;;51079-0801-19
 ;;9002226.02101,"393,51079-0801-20 ",.01)
 ;;51079-0801-20
 ;;9002226.02101,"393,51079-0801-20 ",.02)
 ;;51079-0801-20
 ;;9002226.02101,"393,51079-0801-24 ",.01)
 ;;51079-0801-24
 ;;9002226.02101,"393,51079-0801-24 ",.02)
 ;;51079-0801-24
