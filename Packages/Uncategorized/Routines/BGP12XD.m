BGP12XD ; IHS/CMI/LAB -CREATED BY ^ATXSTX ON APR 14, 2011 ;
 ;;11.1;IHS CLINICAL REPORTING SYSTEM;;JUN 27, 2011;Build 33
 ;
START ;
 K:'$G(ATXPGMC) ^TMP("ATX",$J)
 S ATXPGMC=$G(ATXPGMC)+1
 F ATXI=1:1 S X=$P($T(TMP+ATXI),";;",2,99) Q:X=""  S X="^TMP(""ATX"",$J,"_X,ATXI=ATXI+1,Y=$P($T(TMP+ATXI),";;",2,99) S @X=Y
 Q
 ;
TMP ;;TAXONOMY (WITH BULLETIN)
 ;;9002226.02101,"686,00007-3513-20 ",.01)
 ;;00007-3513-20
 ;;9002226.02101,"686,00007-3513-20 ",.02)
 ;;00007-3513-20
 ;;9002226.02101,"686,00007-3513-59 ",.01)
 ;;00007-3513-59
 ;;9002226.02101,"686,00007-3513-59 ",.02)
 ;;00007-3513-59
 ;;9002226.02101,"686,00007-3514-20 ",.01)
 ;;00007-3514-20
 ;;9002226.02101,"686,00007-3514-20 ",.02)
 ;;00007-3514-20
 ;;9002226.02101,"686,00007-3514-59 ",.01)
 ;;00007-3514-59
 ;;9002226.02101,"686,00007-3514-59 ",.02)
 ;;00007-3514-59
 ;;9002226.02101,"686,00007-3519-20 ",.01)
 ;;00007-3519-20
 ;;9002226.02101,"686,00007-3519-20 ",.02)
 ;;00007-3519-20
 ;;9002226.02101,"686,00009-0024-01 ",.01)
 ;;00009-0024-01
 ;;9002226.02101,"686,00009-0024-01 ",.02)
 ;;00009-0024-01
 ;;9002226.02101,"686,00009-0024-02 ",.01)
 ;;00009-0024-02
 ;;9002226.02101,"686,00009-0024-02 ",.02)
 ;;00009-0024-02
 ;;9002226.02101,"686,00068-0697-61 ",.01)
 ;;00068-0697-61
 ;;9002226.02101,"686,00068-0697-61 ",.02)
 ;;00068-0697-61
 ;;9002226.02101,"686,00068-0698-61 ",.01)
 ;;00068-0698-61
 ;;9002226.02101,"686,00068-0698-61 ",.02)
 ;;00068-0698-61
 ;;9002226.02101,"686,00068-0698-62 ",.01)
 ;;00068-0698-62
 ;;9002226.02101,"686,00068-0698-62 ",.02)
 ;;00068-0698-62
 ;;9002226.02101,"686,00078-0370-05 ",.01)
 ;;00078-0370-05
 ;;9002226.02101,"686,00078-0370-05 ",.02)
 ;;00078-0370-05
 ;;9002226.02101,"686,00078-0371-05 ",.01)
 ;;00078-0371-05
 ;;9002226.02101,"686,00078-0371-05 ",.02)
 ;;00078-0371-05
 ;;9002226.02101,"686,00078-0372-05 ",.01)
 ;;00078-0372-05
 ;;9002226.02101,"686,00078-0372-05 ",.02)
 ;;00078-0372-05
 ;;9002226.02101,"686,00078-0380-05 ",.01)
 ;;00078-0380-05
 ;;9002226.02101,"686,00078-0380-05 ",.02)
 ;;00078-0380-05
 ;;9002226.02101,"686,00078-0381-05 ",.01)
 ;;00078-0381-05
 ;;9002226.02101,"686,00078-0381-05 ",.02)
 ;;00078-0381-05
 ;;9002226.02101,"686,00078-0382-05 ",.01)
 ;;00078-0382-05
 ;;9002226.02101,"686,00078-0382-05 ",.02)
 ;;00078-0382-05
 ;;9002226.02101,"686,00078-0424-05 ",.01)
 ;;00078-0424-05
 ;;9002226.02101,"686,00078-0424-05 ",.02)
 ;;00078-0424-05
 ;;9002226.02101,"686,00078-0430-05 ",.01)
 ;;00078-0430-05
 ;;9002226.02101,"686,00078-0430-05 ",.02)
 ;;00078-0430-05
 ;;9002226.02101,"686,00078-0431-05 ",.01)
 ;;00078-0431-05
 ;;9002226.02101,"686,00078-0431-05 ",.02)
 ;;00078-0431-05
 ;;9002226.02101,"686,00078-0432-05 ",.01)
 ;;00078-0432-05
 ;;9002226.02101,"686,00078-0432-05 ",.02)
 ;;00078-0432-05
 ;;9002226.02101,"686,00078-0433-05 ",.01)
 ;;00078-0433-05
 ;;9002226.02101,"686,00078-0433-05 ",.02)
 ;;00078-0433-05
 ;;9002226.02101,"686,00078-0434-05 ",.01)
 ;;00078-0434-05
 ;;9002226.02101,"686,00078-0434-05 ",.02)
 ;;00078-0434-05
 ;;9002226.02101,"686,00078-0439-05 ",.01)
 ;;00078-0439-05
 ;;9002226.02101,"686,00078-0439-05 ",.02)
 ;;00078-0439-05
 ;;9002226.02101,"686,00078-0440-05 ",.01)
 ;;00078-0440-05
 ;;9002226.02101,"686,00078-0440-05 ",.02)
 ;;00078-0440-05
 ;;9002226.02101,"686,00078-0441-05 ",.01)
 ;;00078-0441-05
 ;;9002226.02101,"686,00078-0441-05 ",.02)
 ;;00078-0441-05
 ;;9002226.02101,"686,00078-0442-05 ",.01)
 ;;00078-0442-05
 ;;9002226.02101,"686,00078-0442-05 ",.02)
 ;;00078-0442-05
 ;;9002226.02101,"686,00078-0493-05 ",.01)
 ;;00078-0493-05
 ;;9002226.02101,"686,00078-0493-05 ",.02)
 ;;00078-0493-05
 ;;9002226.02101,"686,00083-0003-30 ",.01)
 ;;00083-0003-30
 ;;9002226.02101,"686,00083-0003-30 ",.02)
 ;;00083-0003-30
 ;;9002226.02101,"686,00083-0007-30 ",.01)
 ;;00083-0007-30
 ;;9002226.02101,"686,00083-0007-30 ",.02)
 ;;00083-0007-30
 ;;9002226.02101,"686,00083-0016-30 ",.01)
 ;;00083-0016-30
 ;;9002226.02101,"686,00083-0016-30 ",.02)
 ;;00083-0016-30
 ;;9002226.02101,"686,00083-0034-30 ",.01)
 ;;00083-0034-30
 ;;9002226.02101,"686,00083-0034-30 ",.02)
 ;;00083-0034-30
 ;;9002226.02101,"686,00093-5275-01 ",.01)
 ;;00093-5275-01
 ;;9002226.02101,"686,00093-5275-01 ",.02)
 ;;00093-5275-01
 ;;9002226.02101,"686,00093-5276-01 ",.01)
 ;;00093-5276-01
 ;;9002226.02101,"686,00093-5276-01 ",.02)
 ;;00093-5276-01
 ;;9002226.02101,"686,00093-5277-01 ",.01)
 ;;00093-5277-01
 ;;9002226.02101,"686,00093-5277-01 ",.02)
 ;;00093-5277-01
 ;;9002226.02101,"686,00115-1205-01 ",.01)
 ;;00115-1205-01
 ;;9002226.02101,"686,00115-1205-01 ",.02)
 ;;00115-1205-01
 ;;9002226.02101,"686,00115-1328-01 ",.01)
 ;;00115-1328-01
 ;;9002226.02101,"686,00115-1328-01 ",.02)
 ;;00115-1328-01
 ;;9002226.02101,"686,00115-1329-01 ",.01)
 ;;00115-1329-01
 ;;9002226.02101,"686,00115-1329-01 ",.02)
 ;;00115-1329-01
 ;;9002226.02101,"686,00115-1330-01 ",.01)
 ;;00115-1330-01
 ;;9002226.02101,"686,00115-1330-01 ",.02)
 ;;00115-1330-01
 ;;9002226.02101,"686,00115-1331-01 ",.01)
 ;;00115-1331-01
 ;;9002226.02101,"686,00115-1331-01 ",.02)
 ;;00115-1331-01
 ;;9002226.02101,"686,00115-1332-01 ",.01)
 ;;00115-1332-01
 ;;9002226.02101,"686,00115-1332-01 ",.02)
 ;;00115-1332-01
 ;;9002226.02101,"686,00115-1333-01 ",.01)
 ;;00115-1333-01
 ;;9002226.02101,"686,00115-1333-01 ",.02)
 ;;00115-1333-01
 ;;9002226.02101,"686,00144-0740-01 ",.01)
 ;;00144-0740-01
 ;;9002226.02101,"686,00144-0740-01 ",.02)
 ;;00144-0740-01
 ;;9002226.02101,"686,00147-0102-20 ",.01)
 ;;00147-0102-20
 ;;9002226.02101,"686,00147-0102-20 ",.02)
 ;;00147-0102-20
 ;;9002226.02101,"686,00147-0109-10 ",.01)
 ;;00147-0109-10
 ;;9002226.02101,"686,00147-0109-10 ",.02)
 ;;00147-0109-10
 ;;9002226.02101,"686,00147-0109-20 ",.01)
 ;;00147-0109-20
 ;;9002226.02101,"686,00147-0109-20 ",.02)
 ;;00147-0109-20
 ;;9002226.02101,"686,00147-0135-20 ",.01)
 ;;00147-0135-20
 ;;9002226.02101,"686,00147-0135-20 ",.02)
 ;;00147-0135-20
 ;;9002226.02101,"686,00147-0136-10 ",.01)
 ;;00147-0136-10
 ;;9002226.02101,"686,00147-0136-10 ",.02)
 ;;00147-0136-10
 ;;9002226.02101,"686,00147-0136-20 ",.01)
 ;;00147-0136-20
 ;;9002226.02101,"686,00147-0136-20 ",.02)
 ;;00147-0136-20
 ;;9002226.02101,"686,00147-0198-10 ",.01)
 ;;00147-0198-10
 ;;9002226.02101,"686,00147-0198-10 ",.02)
 ;;00147-0198-10
 ;;9002226.02101,"686,00147-0198-20 ",.01)
 ;;00147-0198-20
 ;;9002226.02101,"686,00147-0198-20 ",.02)
 ;;00147-0198-20
 ;;9002226.02101,"686,00147-0201-10 ",.01)
 ;;00147-0201-10
 ;;9002226.02101,"686,00147-0201-10 ",.02)
 ;;00147-0201-10
 ;;9002226.02101,"686,00147-0201-20 ",.01)
 ;;00147-0201-20
 ;;9002226.02101,"686,00147-0201-20 ",.02)
 ;;00147-0201-20
 ;;9002226.02101,"686,00147-0202-10 ",.01)
 ;;00147-0202-10
 ;;9002226.02101,"686,00147-0202-10 ",.02)
 ;;00147-0202-10
 ;;9002226.02101,"686,00147-0202-20 ",.01)
 ;;00147-0202-20
 ;;9002226.02101,"686,00147-0202-20 ",.02)
 ;;00147-0202-20
 ;;9002226.02101,"686,00147-0231-10 ",.01)
 ;;00147-0231-10
 ;;9002226.02101,"686,00147-0231-10 ",.02)
 ;;00147-0231-10
 ;;9002226.02101,"686,00147-0231-20 ",.01)
 ;;00147-0231-20
 ;;9002226.02101,"686,00147-0231-20 ",.02)
 ;;00147-0231-20
 ;;9002226.02101,"686,00147-0232-10 ",.01)
 ;;00147-0232-10
 ;;9002226.02101,"686,00147-0232-10 ",.02)
 ;;00147-0232-10
 ;;9002226.02101,"686,00147-0232-20 ",.01)
 ;;00147-0232-20
 ;;9002226.02101,"686,00147-0232-20 ",.02)
 ;;00147-0232-20
 ;;9002226.02101,"686,00147-0234-10 ",.01)
 ;;00147-0234-10
 ;;9002226.02101,"686,00147-0234-10 ",.02)
 ;;00147-0234-10
 ;;9002226.02101,"686,00147-0234-20 ",.01)
 ;;00147-0234-20
 ;;9002226.02101,"686,00147-0234-20 ",.02)
 ;;00147-0234-20
 ;;9002226.02101,"686,00147-0235-10 ",.01)
 ;;00147-0235-10
 ;;9002226.02101,"686,00147-0235-10 ",.02)
 ;;00147-0235-10
 ;;9002226.02101,"686,00147-0235-20 ",.01)
 ;;00147-0235-20
 ;;9002226.02101,"686,00147-0235-20 ",.02)
 ;;00147-0235-20
 ;;9002226.02101,"686,00147-0237-10 ",.01)
 ;;00147-0237-10
 ;;9002226.02101,"686,00147-0237-10 ",.02)
 ;;00147-0237-10
 ;;9002226.02101,"686,00147-0237-20 ",.01)
 ;;00147-0237-20
 ;;9002226.02101,"686,00147-0237-20 ",.02)
 ;;00147-0237-20
 ;;9002226.02101,"686,00147-0248-10 ",.01)
 ;;00147-0248-10
 ;;9002226.02101,"686,00147-0248-10 ",.02)
 ;;00147-0248-10
 ;;9002226.02101,"686,00147-0248-20 ",.01)
 ;;00147-0248-20
 ;;9002226.02101,"686,00147-0248-20 ",.02)
 ;;00147-0248-20
 ;;9002226.02101,"686,00147-0249-10 ",.01)
 ;;00147-0249-10
 ;;9002226.02101,"686,00147-0249-10 ",.02)
 ;;00147-0249-10
 ;;9002226.02101,"686,00147-0249-20 ",.01)
 ;;00147-0249-20
 ;;9002226.02101,"686,00147-0249-20 ",.02)
 ;;00147-0249-20
 ;;9002226.02101,"686,00147-0251-10 ",.01)
 ;;00147-0251-10
 ;;9002226.02101,"686,00147-0251-10 ",.02)
 ;;00147-0251-10
 ;;9002226.02101,"686,00147-0251-20 ",.01)
 ;;00147-0251-20
 ;;9002226.02101,"686,00147-0251-20 ",.02)
 ;;00147-0251-20
 ;;9002226.02101,"686,00147-0253-10 ",.01)
 ;;00147-0253-10
 ;;9002226.02101,"686,00147-0253-10 ",.02)
 ;;00147-0253-10
 ;;9002226.02101,"686,00147-0253-20 ",.01)
 ;;00147-0253-20
 ;;9002226.02101,"686,00147-0253-20 ",.02)
 ;;00147-0253-20
 ;;9002226.02101,"686,00147-0254-10 ",.01)
 ;;00147-0254-10
 ;;9002226.02101,"686,00147-0254-10 ",.02)
 ;;00147-0254-10
 ;;9002226.02101,"686,00147-0254-20 ",.01)
 ;;00147-0254-20
 ;;9002226.02101,"686,00147-0254-20 ",.02)
 ;;00147-0254-20
 ;;9002226.02101,"686,00182-0205-01 ",.01)
 ;;00182-0205-01
 ;;9002226.02101,"686,00182-0205-01 ",.02)
 ;;00182-0205-01
 ;;9002226.02101,"686,00182-0205-10 ",.01)
 ;;00182-0205-10
 ;;9002226.02101,"686,00182-0205-10 ",.02)
 ;;00182-0205-10
 ;;9002226.02101,"686,00182-0870-01 ",.01)
 ;;00182-0870-01
 ;;9002226.02101,"686,00182-0870-01 ",.02)
 ;;00182-0870-01
 ;;9002226.02101,"686,00182-0870-02 ",.01)
 ;;00182-0870-02
 ;;9002226.02101,"686,00182-0870-02 ",.02)
 ;;00182-0870-02
 ;;9002226.02101,"686,00182-1066-01 ",.01)
 ;;00182-1066-01
 ;;9002226.02101,"686,00182-1066-01 ",.02)
 ;;00182-1066-01
 ;;9002226.02101,"686,00182-1066-10 ",.01)
 ;;00182-1066-10
 ;;9002226.02101,"686,00182-1066-10 ",.02)
 ;;00182-1066-10
 ;;9002226.02101,"686,00182-1173-01 ",.01)
 ;;00182-1173-01
 ;;9002226.02101,"686,00182-1173-01 ",.02)
 ;;00182-1173-01
 ;;9002226.02101,"686,00182-1173-10 ",.01)
 ;;00182-1173-10
 ;;9002226.02101,"686,00182-1173-10 ",.02)
 ;;00182-1173-10
 ;;9002226.02101,"686,00182-1174-01 ",.01)
 ;;00182-1174-01
 ;;9002226.02101,"686,00182-1174-01 ",.02)
 ;;00182-1174-01
 ;;9002226.02101,"686,00182-1436-01 ",.01)
 ;;00182-1436-01
 ;;9002226.02101,"686,00182-1436-01 ",.02)
 ;;00182-1436-01
 ;;9002226.02101,"686,00182-9147-01 ",.01)
 ;;00182-9147-01
 ;;9002226.02101,"686,00182-9147-01 ",.02)
 ;;00182-9147-01
 ;;9002226.02101,"686,00185-0084-01 ",.01)
 ;;00185-0084-01
 ;;9002226.02101,"686,00185-0084-01 ",.02)
 ;;00185-0084-01
 ;;9002226.02101,"686,00185-0111-01 ",.01)
 ;;00185-0111-01
 ;;9002226.02101,"686,00185-0111-01 ",.02)
 ;;00185-0111-01
 ;;9002226.02101,"686,00185-0401-01 ",.01)
 ;;00185-0401-01
 ;;9002226.02101,"686,00185-0401-01 ",.02)
 ;;00185-0401-01
 ;;9002226.02101,"686,00185-0404-01 ",.01)
 ;;00185-0404-01
 ;;9002226.02101,"686,00185-0404-01 ",.02)
 ;;00185-0404-01
 ;;9002226.02101,"686,00185-0644-01 ",.01)
 ;;00185-0644-01
 ;;9002226.02101,"686,00185-0644-01 ",.02)
 ;;00185-0644-01
 ;;9002226.02101,"686,00185-0644-10 ",.01)
 ;;00185-0644-10
 ;;9002226.02101,"686,00185-0644-10 ",.02)
 ;;00185-0644-10
 ;;9002226.02101,"686,00185-0647-01 ",.01)
 ;;00185-0647-01
 ;;9002226.02101,"686,00185-0647-01 ",.02)
 ;;00185-0647-01
 ;;9002226.02101,"686,00185-0647-10 ",.01)
 ;;00185-0647-10
 ;;9002226.02101,"686,00185-0647-10 ",.02)
 ;;00185-0647-10
 ;;9002226.02101,"686,00185-4057-01 ",.01)
 ;;00185-4057-01
 ;;9002226.02101,"686,00185-4057-01 ",.02)
 ;;00185-4057-01
 ;;9002226.02101,"686,00185-4057-10 ",.01)
 ;;00185-4057-10
 ;;9002226.02101,"686,00185-4057-10 ",.02)
 ;;00185-4057-10
 ;;9002226.02101,"686,00185-5000-01 ",.01)
 ;;00185-5000-01
 ;;9002226.02101,"686,00185-5000-01 ",.02)
 ;;00185-5000-01
 ;;9002226.02101,"686,00185-5000-10 ",.01)
 ;;00185-5000-10
 ;;9002226.02101,"686,00185-5000-10 ",.02)
 ;;00185-5000-10
 ;;9002226.02101,"686,00185-5254-01 ",.01)
 ;;00185-5254-01
 ;;9002226.02101,"686,00185-5254-01 ",.02)
 ;;00185-5254-01
 ;;9002226.02101,"686,00185-5254-10 ",.01)
 ;;00185-5254-10
 ;;9002226.02101,"686,00185-5254-10 ",.02)
 ;;00185-5254-10
 ;;9002226.02101,"686,00187-0497-01 ",.01)
 ;;00187-0497-01
 ;;9002226.02101,"686,00187-0497-01 ",.02)
 ;;00187-0497-01
 ;;9002226.02101,"686,00187-0497-02 ",.01)
 ;;00187-0497-02
 ;;9002226.02101,"686,00187-0497-02 ",.02)
 ;;00187-0497-02
 ;;9002226.02101,"686,00187-0498-01 ",.01)
 ;;00187-0498-01
 ;;9002226.02101,"686,00187-0498-01 ",.02)
 ;;00187-0498-01
 ;;9002226.02101,"686,00187-0498-02 ",.01)
 ;;00187-0498-02
 ;;9002226.02101,"686,00187-0498-02 ",.02)
 ;;00187-0498-02
 ;;9002226.02101,"686,00228-2089-10 ",.01)
 ;;00228-2089-10
