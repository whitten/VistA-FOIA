BGP14F4 ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"1108,13411-0151-09 ",.01)
 ;;13411-0151-09
 ;;9002226.02101,"1108,13411-0151-09 ",.02)
 ;;13411-0151-09
 ;;9002226.02101,"1108,13411-0151-15 ",.01)
 ;;13411-0151-15
 ;;9002226.02101,"1108,13411-0151-15 ",.02)
 ;;13411-0151-15
 ;;9002226.02101,"1108,13411-0160-01 ",.01)
 ;;13411-0160-01
 ;;9002226.02101,"1108,13411-0160-01 ",.02)
 ;;13411-0160-01
 ;;9002226.02101,"1108,13411-0160-03 ",.01)
 ;;13411-0160-03
 ;;9002226.02101,"1108,13411-0160-03 ",.02)
 ;;13411-0160-03
 ;;9002226.02101,"1108,13411-0160-06 ",.01)
 ;;13411-0160-06
 ;;9002226.02101,"1108,13411-0160-06 ",.02)
 ;;13411-0160-06
 ;;9002226.02101,"1108,13411-0160-09 ",.01)
 ;;13411-0160-09
 ;;9002226.02101,"1108,13411-0160-09 ",.02)
 ;;13411-0160-09
 ;;9002226.02101,"1108,13411-0160-15 ",.01)
 ;;13411-0160-15
 ;;9002226.02101,"1108,13411-0160-15 ",.02)
 ;;13411-0160-15
 ;;9002226.02101,"1108,16571-0011-10 ",.01)
 ;;16571-0011-10
 ;;9002226.02101,"1108,16571-0011-10 ",.02)
 ;;16571-0011-10
 ;;9002226.02101,"1108,16590-0025-20 ",.01)
 ;;16590-0025-20
 ;;9002226.02101,"1108,16590-0025-20 ",.02)
 ;;16590-0025-20
 ;;9002226.02101,"1108,16590-0860-71 ",.01)
 ;;16590-0860-71
 ;;9002226.02101,"1108,16590-0860-71 ",.02)
 ;;16590-0860-71
 ;;9002226.02101,"1108,16590-0860-73 ",.01)
 ;;16590-0860-73
 ;;9002226.02101,"1108,16590-0860-73 ",.02)
 ;;16590-0860-73
 ;;9002226.02101,"1108,17236-0324-01 ",.01)
 ;;17236-0324-01
 ;;9002226.02101,"1108,17236-0324-01 ",.02)
 ;;17236-0324-01
 ;;9002226.02101,"1108,17236-0324-10 ",.01)
 ;;17236-0324-10
 ;;9002226.02101,"1108,17236-0324-10 ",.02)
 ;;17236-0324-10
 ;;9002226.02101,"1108,17236-0325-01 ",.01)
 ;;17236-0325-01
 ;;9002226.02101,"1108,17236-0325-01 ",.02)
 ;;17236-0325-01
 ;;9002226.02101,"1108,17236-0325-10 ",.01)
 ;;17236-0325-10
 ;;9002226.02101,"1108,17236-0325-10 ",.02)
 ;;17236-0325-10
 ;;9002226.02101,"1108,17236-0335-01 ",.01)
 ;;17236-0335-01
 ;;9002226.02101,"1108,17236-0335-01 ",.02)
 ;;17236-0335-01
 ;;9002226.02101,"1108,17236-0335-05 ",.01)
 ;;17236-0335-05
 ;;9002226.02101,"1108,17236-0335-05 ",.02)
 ;;17236-0335-05
 ;;9002226.02101,"1108,21695-0196-01 ",.01)
 ;;21695-0196-01
 ;;9002226.02101,"1108,21695-0196-01 ",.02)
 ;;21695-0196-01
 ;;9002226.02101,"1108,21695-0197-01 ",.01)
 ;;21695-0197-01
 ;;9002226.02101,"1108,21695-0197-01 ",.02)
 ;;21695-0197-01
 ;;9002226.02101,"1108,21695-0221-30 ",.01)
 ;;21695-0221-30
 ;;9002226.02101,"1108,21695-0221-30 ",.02)
 ;;21695-0221-30
 ;;9002226.02101,"1108,21695-0565-30 ",.01)
 ;;21695-0565-30
 ;;9002226.02101,"1108,21695-0565-30 ",.02)
 ;;21695-0565-30
 ;;9002226.02101,"1108,23490-7355-01 ",.01)
 ;;23490-7355-01
 ;;9002226.02101,"1108,23490-7355-01 ",.02)
 ;;23490-7355-01
 ;;9002226.02101,"1108,23490-7541-01 ",.01)
 ;;23490-7541-01
 ;;9002226.02101,"1108,23490-7541-01 ",.02)
 ;;23490-7541-01
 ;;9002226.02101,"1108,23490-7542-01 ",.01)
 ;;23490-7542-01
 ;;9002226.02101,"1108,23490-7542-01 ",.02)
 ;;23490-7542-01
 ;;9002226.02101,"1108,23490-8018-03 ",.01)
 ;;23490-8018-03
 ;;9002226.02101,"1108,23490-8018-03 ",.02)
 ;;23490-8018-03
 ;;9002226.02101,"1108,23490-9405-00 ",.01)
 ;;23490-9405-00
 ;;9002226.02101,"1108,23490-9405-00 ",.02)
 ;;23490-9405-00
 ;;9002226.02101,"1108,29033-0001-01 ",.01)
 ;;29033-0001-01
 ;;9002226.02101,"1108,29033-0001-01 ",.02)
 ;;29033-0001-01
 ;;9002226.02101,"1108,29033-0002-01 ",.01)
 ;;29033-0002-01
 ;;9002226.02101,"1108,29033-0002-01 ",.02)
 ;;29033-0002-01
 ;;9002226.02101,"1108,35356-0099-14 ",.01)
 ;;35356-0099-14
 ;;9002226.02101,"1108,35356-0099-14 ",.02)
 ;;35356-0099-14
 ;;9002226.02101,"1108,35356-0126-60 ",.01)
 ;;35356-0126-60
 ;;9002226.02101,"1108,35356-0126-60 ",.02)
 ;;35356-0126-60
 ;;9002226.02101,"1108,35356-0157-01 ",.01)
 ;;35356-0157-01
 ;;9002226.02101,"1108,35356-0157-01 ",.02)
 ;;35356-0157-01
 ;;9002226.02101,"1108,35356-0494-01 ",.01)
 ;;35356-0494-01
 ;;9002226.02101,"1108,35356-0494-01 ",.02)
 ;;35356-0494-01
 ;;9002226.02101,"1108,49999-0300-28 ",.01)
 ;;49999-0300-28
 ;;9002226.02101,"1108,49999-0300-28 ",.02)
 ;;49999-0300-28
 ;;9002226.02101,"1108,49999-0533-30 ",.01)
 ;;49999-0533-30
 ;;9002226.02101,"1108,49999-0533-30 ",.02)
 ;;49999-0533-30
 ;;9002226.02101,"1108,49999-0533-90 ",.01)
 ;;49999-0533-90
 ;;9002226.02101,"1108,49999-0533-90 ",.02)
 ;;49999-0533-90
 ;;9002226.02101,"1108,49999-0614-01 ",.01)
 ;;49999-0614-01
 ;;9002226.02101,"1108,49999-0614-01 ",.02)
 ;;49999-0614-01
 ;;9002226.02101,"1108,49999-0614-12 ",.01)
 ;;49999-0614-12
 ;;9002226.02101,"1108,49999-0614-12 ",.02)
 ;;49999-0614-12
 ;;9002226.02101,"1108,49999-0819-60 ",.01)
 ;;49999-0819-60
 ;;9002226.02101,"1108,49999-0819-60 ",.02)
 ;;49999-0819-60
 ;;9002226.02101,"1108,49999-0884-30 ",.01)
 ;;49999-0884-30
 ;;9002226.02101,"1108,49999-0884-30 ",.02)
 ;;49999-0884-30
 ;;9002226.02101,"1108,49999-0884-90 ",.01)
 ;;49999-0884-90
 ;;9002226.02101,"1108,49999-0884-90 ",.02)
 ;;49999-0884-90
 ;;9002226.02101,"1108,49999-0921-30 ",.01)
 ;;49999-0921-30
 ;;9002226.02101,"1108,49999-0921-30 ",.02)
 ;;49999-0921-30
 ;;9002226.02101,"1108,49999-0952-30 ",.01)
 ;;49999-0952-30
 ;;9002226.02101,"1108,49999-0952-30 ",.02)
 ;;49999-0952-30
 ;;9002226.02101,"1108,49999-0984-60 ",.01)
 ;;49999-0984-60
 ;;9002226.02101,"1108,49999-0984-60 ",.02)
 ;;49999-0984-60
 ;;9002226.02101,"1108,49999-0985-60 ",.01)
 ;;49999-0985-60
 ;;9002226.02101,"1108,49999-0985-60 ",.02)
 ;;49999-0985-60
 ;;9002226.02101,"1108,50111-0459-01 ",.01)
 ;;50111-0459-01
 ;;9002226.02101,"1108,50111-0459-01 ",.02)
 ;;50111-0459-01
 ;;9002226.02101,"1108,50111-0459-02 ",.01)
 ;;50111-0459-02
 ;;9002226.02101,"1108,50111-0459-02 ",.02)
 ;;50111-0459-02
 ;;9002226.02101,"1108,50111-0459-03 ",.01)
 ;;50111-0459-03
 ;;9002226.02101,"1108,50111-0459-03 ",.02)
 ;;50111-0459-03
 ;;9002226.02101,"1108,50111-0482-01 ",.01)
 ;;50111-0482-01
 ;;9002226.02101,"1108,50111-0482-01 ",.02)
 ;;50111-0482-01
 ;;9002226.02101,"1108,50111-0482-02 ",.01)
 ;;50111-0482-02
 ;;9002226.02101,"1108,50111-0482-02 ",.02)
 ;;50111-0482-02
 ;;9002226.02101,"1108,50111-0482-03 ",.01)
 ;;50111-0482-03
 ;;9002226.02101,"1108,50111-0482-03 ",.02)
 ;;50111-0482-03
 ;;9002226.02101,"1108,50111-0483-01 ",.01)
 ;;50111-0483-01
 ;;9002226.02101,"1108,50111-0483-01 ",.02)
 ;;50111-0483-01
 ;;9002226.02101,"1108,50111-0483-02 ",.01)
 ;;50111-0483-02
 ;;9002226.02101,"1108,50111-0483-02 ",.02)
 ;;50111-0483-02
 ;;9002226.02101,"1108,50111-0518-01 ",.01)
 ;;50111-0518-01
 ;;9002226.02101,"1108,50111-0518-01 ",.02)
 ;;50111-0518-01
 ;;9002226.02101,"1108,50474-0100-01 ",.01)
 ;;50474-0100-01
 ;;9002226.02101,"1108,50474-0100-01 ",.02)
 ;;50474-0100-01
 ;;9002226.02101,"1108,50474-0200-01 ",.01)
 ;;50474-0200-01
 ;;9002226.02101,"1108,50474-0200-01 ",.02)
 ;;50474-0200-01
 ;;9002226.02101,"1108,50474-0200-50 ",.01)
 ;;50474-0200-50
 ;;9002226.02101,"1108,50474-0200-50 ",.02)
 ;;50474-0200-50
 ;;9002226.02101,"1108,50474-0200-60 ",.01)
 ;;50474-0200-60
 ;;9002226.02101,"1108,50474-0200-60 ",.02)
 ;;50474-0200-60
 ;;9002226.02101,"1108,50474-0300-01 ",.01)
 ;;50474-0300-01
 ;;9002226.02101,"1108,50474-0300-01 ",.02)
 ;;50474-0300-01
 ;;9002226.02101,"1108,50474-0300-50 ",.01)
 ;;50474-0300-50
 ;;9002226.02101,"1108,50474-0300-50 ",.02)
 ;;50474-0300-50
 ;;9002226.02101,"1108,50474-0300-60 ",.01)
 ;;50474-0300-60
 ;;9002226.02101,"1108,50474-0300-60 ",.02)
 ;;50474-0300-60
 ;;9002226.02101,"1108,50474-0400-01 ",.01)
 ;;50474-0400-01
 ;;9002226.02101,"1108,50474-0400-01 ",.02)
 ;;50474-0400-01
 ;;9002226.02101,"1108,51655-0256-24 ",.01)
 ;;51655-0256-24
 ;;9002226.02101,"1108,51655-0256-24 ",.02)
 ;;51655-0256-24
 ;;9002226.02101,"1108,51655-0291-24 ",.01)
 ;;51655-0291-24
 ;;9002226.02101,"1108,51655-0291-24 ",.02)
 ;;51655-0291-24
 ;;9002226.02101,"1108,52959-0131-00 ",.01)
 ;;52959-0131-00
 ;;9002226.02101,"1108,52959-0131-00 ",.02)
 ;;52959-0131-00
 ;;9002226.02101,"1108,52959-0279-30 ",.01)
 ;;52959-0279-30
 ;;9002226.02101,"1108,52959-0279-30 ",.02)
 ;;52959-0279-30
 ;;9002226.02101,"1108,52959-0286-03 ",.01)
 ;;52959-0286-03
 ;;9002226.02101,"1108,52959-0286-03 ",.02)
 ;;52959-0286-03
 ;;9002226.02101,"1108,52959-1198-00 ",.01)
 ;;52959-1198-00
 ;;9002226.02101,"1108,52959-1198-00 ",.02)
 ;;52959-1198-00
 ;;9002226.02101,"1108,52959-1447-01 ",.01)
 ;;52959-1447-01
 ;;9002226.02101,"1108,52959-1447-01 ",.02)
 ;;52959-1447-01
 ;;9002226.02101,"1108,52959-1466-01 ",.01)
 ;;52959-1466-01
 ;;9002226.02101,"1108,52959-1466-01 ",.02)
 ;;52959-1466-01
 ;;9002226.02101,"1108,53002-1436-01 ",.01)
 ;;53002-1436-01
 ;;9002226.02101,"1108,53002-1436-01 ",.02)
 ;;53002-1436-01
 ;;9002226.02101,"1108,53002-1477-01 ",.01)
 ;;53002-1477-01
 ;;9002226.02101,"1108,53002-1477-01 ",.02)
 ;;53002-1477-01
 ;;9002226.02101,"1108,53265-0379-10 ",.01)
 ;;53265-0379-10
 ;;9002226.02101,"1108,53265-0379-10 ",.02)
 ;;53265-0379-10
 ;;9002226.02101,"1108,53265-0379-50 ",.01)
 ;;53265-0379-50
 ;;9002226.02101,"1108,53265-0379-50 ",.02)
 ;;53265-0379-50
 ;;9002226.02101,"1108,53265-0380-10 ",.01)
 ;;53265-0380-10
 ;;9002226.02101,"1108,53265-0380-10 ",.02)
 ;;53265-0380-10
 ;;9002226.02101,"1108,53265-0380-50 ",.01)
 ;;53265-0380-50
 ;;9002226.02101,"1108,53265-0380-50 ",.02)
 ;;53265-0380-50
 ;;9002226.02101,"1108,53265-0381-10 ",.01)
 ;;53265-0381-10
 ;;9002226.02101,"1108,53265-0381-10 ",.02)
 ;;53265-0381-10
 ;;9002226.02101,"1108,53265-0382-10 ",.01)
 ;;53265-0382-10
 ;;9002226.02101,"1108,53265-0382-10 ",.02)
 ;;53265-0382-10
 ;;9002226.02101,"1108,54348-0155-10 ",.01)
 ;;54348-0155-10
 ;;9002226.02101,"1108,54348-0155-10 ",.02)
 ;;54348-0155-10
 ;;9002226.02101,"1108,54348-0156-14 ",.01)
 ;;54348-0156-14
 ;;9002226.02101,"1108,54348-0156-14 ",.02)
 ;;54348-0156-14
 ;;9002226.02101,"1108,54569-0049-00 ",.01)
 ;;54569-0049-00
 ;;9002226.02101,"1108,54569-0049-00 ",.02)
 ;;54569-0049-00
 ;;9002226.02101,"1108,54569-0053-00 ",.01)
 ;;54569-0053-00
 ;;9002226.02101,"1108,54569-0053-00 ",.02)
 ;;54569-0053-00
 ;;9002226.02101,"1108,54569-0067-00 ",.01)
 ;;54569-0067-00
 ;;9002226.02101,"1108,54569-0067-00 ",.02)
 ;;54569-0067-00
 ;;9002226.02101,"1108,54569-1012-00 ",.01)
 ;;54569-1012-00
 ;;9002226.02101,"1108,54569-1012-00 ",.02)
 ;;54569-1012-00
 ;;9002226.02101,"1108,54569-1013-00 ",.01)
 ;;54569-1013-00
 ;;9002226.02101,"1108,54569-1013-00 ",.02)
 ;;54569-1013-00
 ;;9002226.02101,"1108,54569-1664-01 ",.01)
 ;;54569-1664-01
 ;;9002226.02101,"1108,54569-1664-01 ",.02)
 ;;54569-1664-01
 ;;9002226.02101,"1108,54569-1666-00 ",.01)
 ;;54569-1666-00
 ;;9002226.02101,"1108,54569-1666-00 ",.02)
 ;;54569-1666-00
 ;;9002226.02101,"1108,54569-1666-01 ",.01)
 ;;54569-1666-01
 ;;9002226.02101,"1108,54569-1666-01 ",.02)
 ;;54569-1666-01
 ;;9002226.02101,"1108,54569-2032-00 ",.01)
 ;;54569-2032-00
 ;;9002226.02101,"1108,54569-2032-00 ",.02)
 ;;54569-2032-00
 ;;9002226.02101,"1108,54569-2482-00 ",.01)
 ;;54569-2482-00
 ;;9002226.02101,"1108,54569-2482-00 ",.02)
 ;;54569-2482-00
 ;;9002226.02101,"1108,54569-2482-01 ",.01)
 ;;54569-2482-01
 ;;9002226.02101,"1108,54569-2482-01 ",.02)
 ;;54569-2482-01
 ;;9002226.02101,"1108,54569-2482-05 ",.01)
 ;;54569-2482-05
 ;;9002226.02101,"1108,54569-2482-05 ",.02)
 ;;54569-2482-05
 ;;9002226.02101,"1108,54569-2483-00 ",.01)
 ;;54569-2483-00
 ;;9002226.02101,"1108,54569-2483-00 ",.02)
 ;;54569-2483-00
 ;;9002226.02101,"1108,54569-2483-01 ",.01)
 ;;54569-2483-01
 ;;9002226.02101,"1108,54569-2483-01 ",.02)
 ;;54569-2483-01
 ;;9002226.02101,"1108,54569-2483-02 ",.01)
 ;;54569-2483-02
 ;;9002226.02101,"1108,54569-2483-02 ",.02)
 ;;54569-2483-02
 ;;9002226.02101,"1108,54569-2483-03 ",.01)
 ;;54569-2483-03
 ;;9002226.02101,"1108,54569-2483-03 ",.02)
 ;;54569-2483-03
 ;;9002226.02101,"1108,54569-3855-00 ",.01)
 ;;54569-3855-00
 ;;9002226.02101,"1108,54569-3855-00 ",.02)
 ;;54569-3855-00
 ;;9002226.02101,"1108,54569-3976-00 ",.01)
 ;;54569-3976-00
 ;;9002226.02101,"1108,54569-3976-00 ",.02)
 ;;54569-3976-00
 ;;9002226.02101,"1108,54569-4540-00 ",.01)
 ;;54569-4540-00
 ;;9002226.02101,"1108,54569-4540-00 ",.02)
 ;;54569-4540-00
 ;;9002226.02101,"1108,54569-4602-00 ",.01)
 ;;54569-4602-00
 ;;9002226.02101,"1108,54569-4602-00 ",.02)
 ;;54569-4602-00
 ;;9002226.02101,"1108,54569-4603-00 ",.01)
 ;;54569-4603-00
 ;;9002226.02101,"1108,54569-4603-00 ",.02)
 ;;54569-4603-00
 ;;9002226.02101,"1108,54569-4604-00 ",.01)
 ;;54569-4604-00
 ;;9002226.02101,"1108,54569-4604-00 ",.02)
 ;;54569-4604-00
 ;;9002226.02101,"1108,54569-4604-01 ",.01)
 ;;54569-4604-01
 ;;9002226.02101,"1108,54569-4604-01 ",.02)
 ;;54569-4604-01
