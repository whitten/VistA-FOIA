BHLINIT5 ; ; 7-JUL-1997
 ;;1.0;IHS SUPPORT FOR HL7 INTERFACES;;JUL 7, 1997
 K ^UTILITY("DIF",$J) S DIFRDIFI=1 F I=1:1:0 S ^UTILITY("DIF",$J,DIFRDIFI)=$T(IXF+I),DIFRDIFI=DIFRDIFI+1
 Q
IXF ;;IHS SUPPORT FOR HL7 INTERFACES^BHL
