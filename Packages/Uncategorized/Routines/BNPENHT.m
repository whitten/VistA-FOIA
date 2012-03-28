BNPENHT ;IHS/OIT/MLP/ENM - communicate via http protocol
 ;;1.0;NATIONAL SITE TRACKING SYSTEM;;07/31/2009
 // *** MLP - communicate with NHIN development server to submit patch information
 //w !!,"Connecting to NHIN Dev Server, removing old package data ...",!!!
 s (bevx, bevxx)="",d="^"
 // wipe entries prior to submission - mlp
 f  s bevx=$o(^BNPENV(bevx)) q:bevx=""  d  
 . // load variables from global for submission
 .set bevrec = ^BNPENV(bevx)
 .set ttype="wipe"
 .set asufac = $p(bevrec,d)
 Set httprequest=##class(%Net.HttpRequest).%New()
 Set httprequest.Server="nhin.ihs.gov"
 Set httprequest.Port=80
 Do httprequest.SetParam("ttype",ttype)
 Do httprequest.SetParam("asufac",asufac)
 Do httprequest.Get("/preport3/sentry.php")
 // end wipe entries - mlp
 //w !!,"Connecting to NHIN Dev Server, updating package data ...",!!!
 f  s bevx=$o(^BNPENV(bevx)) q:bevx=""  d  
 	. // load variables from global for submission
 	.set bevrec = ^BNPENV(bevx)
 	.set ttype="PI"
 	.set asufac = $p(bevrec,d)
 	.set facilityname = $p(bevrec,d,2)
 	.set packagename = $p(bevrec,d,3)
 	.set pkgnamespace = $p(bevrec,d,4)
 	.set version = $p(bevrec,d,5)
 	.set lpatch = $p(bevrec,d,6)
 	.set dlpatch = $p(bevrec,d,7)
 	.set sindate = $p(bevrec,d,8)
 	.set cos = $p(bevrec,d,9) 	
 .Set httprequest=##class(%Net.HttpRequest).%New()
 .Set httprequest.Server="nhin.ihs.gov"
 .Set httprequest.Port=80
 .Do httprequest.SetParam("ttype",ttype)
 .Do httprequest.SetParam("asufac",asufac)
 .Do httprequest.SetParam("facilityname",facilityname)
 .Do httprequest.SetParam("packagename",packagename)
 .Do httprequest.SetParam("pkgnamespace",pkgnamespace)
 .Do httprequest.SetParam("version",version)
 .Do httprequest.SetParam("lpatch",lpatch)
 .Do httprequest.SetParam("dlpatch",dlpatch)
 .Do httprequest.SetParam("sindate",sindate)
 .Do httprequest.SetParam("cos", cos)
 .Do httprequest.Get("/preport3/sentry.php")
 .//Do httprequest.HttpResponse.OutputToDevice()
 //.write !,bevrec
 // *****
 // MLP DONE NHIN communication
