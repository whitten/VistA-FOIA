FHINI0GX	; ; 11-OCT-1995
	;;5.0;Dietetics;;Oct 11, 1995
	Q:'DIFQR(112)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q	Q
	;;^UTILITY(U,$J,112,10232,1)
	;;=4^10^70^380^3^^^70^2^30^80^380^3510^.4^.1^^^1590^10^.1
	;;^UTILITY(U,$J,112,10232,2)
	;;=.1^1^.3^0^20^0^^^0^4^^0
	;;^UTILITY(U,$J,112,10232,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10233,0)
	;;=COATINGS,SHAKE & BAKE,ORIG RECIPE FOR CHICKEN^BC-03506^1/4-pouch^19
	;;^UTILITY(U,$J,112,10233,1)
	;;=8.421^8.947^73.684^405.263^5.789^^^31.579^1.737^36.842^115.789^205.263^2373.684^.842^.216^^^1657.895^0^.316
	;;^UTILITY(U,$J,112,10233,2)
	;;=.368^4.211^.263^.368^94.737^1^^^0^3.158^^1.579
	;;^UTILITY(U,$J,112,10233,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10234,0)
	;;=COATINGS,SHAKE & BAKE,ORIG RECIPE FOR FISH^BC-03507^1/4-pouch^19
	;;^UTILITY(U,$J,112,10234,1)
	;;=6.842^6.842^74.737^384.211^5.263^^^21.053^2.053^15.789^57.895^126.316^2136.842^.737^.095^^^768.421^0^.053
	;;^UTILITY(U,$J,112,10234,2)
	;;=.053^.526^^.053^5.263^0^^^0^1.053^^2.632
	;;^UTILITY(U,$J,112,10234,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10235,0)
	;;=COATINGS,SHAKE & BAKE,ORIG RECIPE FOR PORK^BC-03508^1/8-pouch^11
	;;^UTILITY(U,$J,112,10235,1)
	;;=9.091^9.091^72.727^372.727^2.727^^^36.364^3.636^18.182^72.727^72.727^2736.364^.909^.091^^^745.455^0^0
	;;^UTILITY(U,$J,112,10235,2)
	;;=0^0^^^^0^^^0^.909^^1.818
	;;^UTILITY(U,$J,112,10235,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10236,0)
	;;=PICKLE RELISH,HAMBURGER,DEL MONTE^BC-03509^oz.^28
	;;^UTILITY(U,$J,112,10236,1)
	;;=.714^.714^36.429^135.714^60.714^^^7.143^1.214^7.143^17.857^78.571^1100^.107^.082^^^271.429^3.571^.036
	;;^UTILITY(U,$J,112,10236,2)
	;;=.036^.714
	;;^UTILITY(U,$J,112,10236,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10237,0)
	;;=PICKLE RELISH,HOT DOG,DEL MONTE^BC-03510^oz.^28
	;;^UTILITY(U,$J,112,10237,1)
	;;=1.786^.357^22.857^89.286^73.214^^^17.857^1.429^17.857^39.286^78.571^1171.429^.214^.082^^^167.857^0^.036
	;;^UTILITY(U,$J,112,10237,2)
	;;=.036^.357
	;;^UTILITY(U,$J,112,10237,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10238,0)
	;;=PICKLE RELISH,PICALILLI,HEINZ^BC-03511^oz.^28
	;;^UTILITY(U,$J,112,10238,1)
	;;=0^0^25^107.143^^^^^^^^^500
	;;^UTILITY(U,$J,112,10238,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10239,0)
	;;=PICKLE RELISH,SWEET DEL MONTE^BC-03512^oz.^28
	;;^UTILITY(U,$J,112,10239,1)
	;;=.357^.357^36.071^135.714^62.143^^^0^1^3.571^14.286^25^839.286^.143^^^^157.143^0
	;;^UTILITY(U,$J,112,10239,2)
	;;=.036^.357
	;;^UTILITY(U,$J,112,10239,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10240,0)
	;;=PICKLES,BREAD & BUTTER,CLAUSSEN^BC-03513^oz.^28
	;;^UTILITY(U,$J,112,10240,1)
	;;=.714^.357^16.786^71.429^80.357^^^78.571^.393^10.714^32.143^117.857^607.143^.393^.107^^^^3.571^0
	;;^UTILITY(U,$J,112,10240,2)
	;;=0^0^^.036^^^^^0
	;;^UTILITY(U,$J,112,10240,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10241,0)
	;;=PICKLES,DILL HAMBURGER CHIPS,DEL MONTE^BC-03514^oz.^28
	;;^UTILITY(U,$J,112,10241,1)
	;;=.357^.357^1.786^7.143^94.286^^^3.571^1^3.571^10.714^35.714^1764.286^.071^.1^^^53.571^0
	;;^UTILITY(U,$J,112,10241,2)
	;;=0
	;;^UTILITY(U,$J,112,10241,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10242,0)
	;;=PICKLES,DILL,KOSHER HALVES,CLAUSSEN^BC-03515^oz.^28
	;;^UTILITY(U,$J,112,10242,1)
	;;=.357^.357^2.143^14.286^93.929^^^14.286^.25^7.143^14.286^100^1160.714^.107^.036^^^^3.571^0
	;;^UTILITY(U,$J,112,10242,2)
	;;=0^0^.179^.036^3.571^^^^0
	;;^UTILITY(U,$J,112,10242,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10243,0)
	;;=PICKLES,DILL,KOSHER,SLICES,CLAUSSEN^BC-03516^oz.^28
	;;^UTILITY(U,$J,112,10243,1)
	;;=.357^.357^1.429^10.714^94.286^^^17.857^.286^7.143^14.286^107.143^1125^.107^.036^^^^0^0
	;;^UTILITY(U,$J,112,10243,2)
	;;=.036^0^^.036
	;;^UTILITY(U,$J,112,10243,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10244,0)
	;;=PICKLES,DILL,KOSHER,WHOLE,CLAUSSEN^BC-03517^oz.^28
	;;^UTILITY(U,$J,112,10244,1)
	;;=.714^.357^1.786^10.714^93.929^^^14.286^.321^7.143^17.857^117.857^1153.571^.107^.107^.061^^^3.571^0
	;;^UTILITY(U,$J,112,10244,2)
	;;=.036^.357^.214^.036^3.571
	;;^UTILITY(U,$J,112,10244,20)
	;;=Bowes & Church's Food Values, Sixteenth Edition.
	;;^UTILITY(U,$J,112,10245,0)
	;;=PICKLES,DILL,NO GARLIC,CLAUSSEN^BC-03518^oz.^28
	;;^UTILITY(U,$J,112,10245,1)
	;;=.714^.357^3.571^21.429^92.143^^^35.714^.286^7.143^17.857^192.857^1060.714^.107^.036
