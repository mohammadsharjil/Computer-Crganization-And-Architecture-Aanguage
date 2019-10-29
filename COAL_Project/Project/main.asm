


TITLE MASM Template						(main.asm)

; Description:
; 
; Revision date:

INCLUDE Irvine32.inc
.data

	covers BYTE "-------Welcome to Physics-------",0ah,0dh,0		
	sel BYTE "Select your desired formula: ",0ah,0dh,
	"Press 1: Coulomb's Law ",0ah,0dh,
	"Press 2: Energy Equation ",0ah,0dh,
	"Press 3: Instructions",0ah,0dh,0
	
	sel1 BYTE " Coulombs Law ",0ah,0dh,
	"1: Find Force F",0ah,0dh,
	"2: Find Charge QA",0ah,0dh,
	"3: Find Charge QB",0ah,0dh,
	"4: Find Distance r",0ah,0dh,0

	sel2 BYTE "  Energy Equation  ",0ah,0dh,
	"1: Find Kinetic Energy E",0ah,0dh,
	"2: Find Mass m",0ah,0dh,0

	findE BYTE "E = m * c^2",0ah,0dh,0
	findm BYTE "m = E / c^2",0ah,0dh,0

	


	findforce BYTE "F = k * (q1*q2 / r^2)",0ah,0dh,0
	findQA BYTE "QA = (F * r^2) / (k * q2)",0ah,0dh,0
	findQB BYTE "QB = (F * r^2) / (k * q1) ",0ah,0dh,0
	finddist BYTE " r = k * (q1 * q2 / F) ",0ah,0dh,0

	wf BYTE "    Force F:     ",0ah,0dh,0
	wqa BYTE "   Charge QA:   ",0ah,0dh,0
	wqb BYTE "   Charge QB:   ",0ah,0dh,0
	wr BYTE "    Distance r:  ",0ah,0dh,0


	ks  BYTE "k (constant) = +9E-9",0
	q1s BYTE "Enter Charge: q1 = ",0
	q2s BYTE "Enter Charge: q2 = ",0
	rs BYTE "Enter Distance: r = ",0
	fss BYTE "Enter Force: f = ",0
	chq BYTE "Force F = ",0
	chq1 byte "Charge QA = ",0
	chq2 BYTE "Charge QB = ",0
	chq3 BYTE "Distance r = ",0

	;--Declaring Coulomb's variables--:
	k real4 0.000000009
	q1 real4 ?
	q2 real4 ?
	r real4 ?
	tempq real4 ?
	tempr real4 ?
	res real4 ?
	fres real4 ?
	f real4 ?
	tempf real4 ?
	tempq1 real4 ?
	tempq2 real4 ?

	;--Declaring Energy variables--:
	spe real4 300000000.0
	mass real4 ?
	energy real4 ?
	temps real4 ?
	rese real4 ?

	speed BYTE "Speed of Light c = 3 x 10^8 m/s",0ah,0dh,0
	masses BYTE "Enter Mass: m = ",0
	Ens BYTE "Enter Energy: E = ",0
	fress BYTE "Energy E = ",0
	fmass BYTE "Mass m = ",0 

	fsdas BYTE "NOTE: User can only give integer or float values",0ah,0dh,
	"value of k (Coulomb's constant) = 9E-9",0ah,0dh,
	"Value of speed of light : c (constant) = 3E8 m/s",0ah,0dh,
	"User can find any parameters according to the particular formula",0ah,0dh,0

	CAPTION BYTE "FOR QUERY",0AH,0DH,0
	question BYTE "Are you want to continue?",0AH,0DH,0

	loads BYTE "Please wait....",0ah,0dh,0


.code
main PROC
	
	;------Coulomb's law------:
	 mov edx, offset covers
	 call writestring
	 call crlf
	 finit
	 L1: mov edx, offset sel
	 call writestring

	 call crlf
	 call Readint
	 .if eax == 1
	 call crlf
	 mov edx, offset sel1
	 call writestring

	 ;-----For F-----:
	 
	 call Readint
	 .if eax == 1
	 call crlf
	 mov edx, offset wf
	 call writestring
	 
	 call crlf
	 mov edx, offset findforce
	 call writestring
	 mov edx, offset ks
	 call writestring
	 call crlf

	 mov edx, offset q1s
	 call writestring
	 call Readfloat
	 fstp q1
	 mov edx, offset q2s
	 call writestring
	 call Readfloat
	 fstp q2
	 mov edx, offset rs
	 call writestring
	 call Readfloat
	 fstp r

	 fld q1
	 fld q2
	 fmul ST(0), ST(1)
	 fstp tempq
	 
	
	 fld r
	 fld r
	 fmul st(0),st(1)
	 
	 fstp tempr
	 
	 fld tempq
	 fdiv tempr

	 fstp res
	
	 fld k
	 fld res
	 fmul st(0),st(1)
	 
	 mov edx, offset loads
	 call writestring
	 call crlf
	 mov edx, offset chq
	 call writestring
	 
	 
	 mov eax,2000
	 call delay

	 call writefloat
	 call crlf
	 call waitmsg
	 call clrscr
	 call crlf
	 call crlf


	 MOV EBX,OFFSET CAPTION
	 MOV EDX,OFFSET QUESTION
	 call MsgBoxAsk
	 .IF EAX ==6
	 JMP L1
	 
	 .ELSEIF EAX==7
	 	 EXIT
	 call waitmsg
	 

	 .ENDIF
	 
	 ;------For QA-------:
	 .elseif eax == 2

	 mov edx, offset wqa
	 call writestring
	 mov edx, offset findQA
	 call writestring
	 call crlf
	 mov edx, offset ks
	 call writestring
	 call crlf

	 mov edx, offset fss
	 call writestring 

	 call Readfloat
	 fstp f

	 mov edx, offset rs
	 call writestring

	 call Readfloat
	 fstp r

	 mov edx, offset q2s
	 call writestring

	 call Readfloat
	 fstp q2

	 fld r
	 fld r
	 fmul st(0),st(1)
	 fstp tempr
	

	 fld f
	 fld tempr
	 fmul st(0),st(1)
	 fstp tempf
	 

	 fld k
	 fld q2
	 
	 fmul st(0),st(1)
	 fstp tempq2

	 fld tempf
	 fdiv tempq2
	 mov edx, offset loads
	 call writestring
	 mov edx,offset chq1
	 call writestring
	  mov eax,2000
	 call delay
	 call writefloat
	
	 call crlf
	 call waitmsg
	 call clrscr
	 call crlf
	 call crlf

	  MOV EBX,OFFSET CAPTION
	 MOV EDX,OFFSET QUESTION
	 call MsgBoxAsk
	 .IF EAX ==6
	 JMP L1
	 
	 .ELSEIF EAX==7
	 	 EXIT
	 
	 

	 .ENDIF

	 ;------For QB------:
	 
	 .elseif eax == 3
	 call crlf
	 mov edx, offset wqb
	 call writestring
	 call crlf
	 mov edx, offset findQB
	 call writestring
	 call crlf
	 mov edx, offset ks
	 call writestring
	 call crlf
	 
	 mov edx, offset fss
	 call writestring 

	 call Readfloat
	 fstp f

	 mov edx, offset rs
	 call writestring

	 call Readfloat
	 fstp r
	 mov edx, offset q1s
	 call writestring

	 call Readfloat
	 fstp q1

	 fld r
	 fld r
	 fmul st(0),st(1)
	 fstp tempr
	 
	 
	 fld f
	 fld tempr
	 fmul st(0),st(1)
	 fstp tempf
	 
	 fld q1
	 fld k
	 fmul st(0),st(1)
	 fstp tempq1

	 fld tempf
	 fdiv tempq1
	 mov edx, offset loads
	 call writestring
	 mov edx, offset chq2
	 call writestring
	  mov eax,2000
	 call delay
	 call writefloat
	 call crlf
	 call waitmsg
	 call clrscr
	 call crlf
	 call crlf

	  MOV EBX,OFFSET CAPTION
	 MOV EDX,OFFSET QUESTION
	 call MsgBoxAsk
	 .IF EAX ==6
	 JMP L1
	 
	 .ELSEIF EAX==7
	 	 EXIT
	 
	 

	 .ENDIF
		
	 ;-------Distance r------:
	 .elseif eax == 4
	 mov edx, offset wr
	 call writestring
	 call crlf
	 mov edx, offset finddist
	 call writestring
	 call crlf
	 mov edx, offset ks
	 call writestring
	 call crlf

	 mov edx, offset q1s
	 call writestring
	 call Readfloat
	 fstp q1

	 mov edx, offset q2s
	 call writestring
	 call Readfloat
	 fstp q2

	 mov edx, offset fss
	 call writestring
	 call Readfloat
	 fstp f

	 fld q1
	 fld q2
	 fmul st(0),st(1)
	 fstp tempq

	 fld tempq
	 fdiv f	
	 fstp tempf


	 
	 fld k
	 fld tempf
	 fmul st(0),st(1)
	
	 mov edx, offset loads
	 call writestring
	 mov edx, offset chq2
	 call writestring
	  mov eax,2000
	 call delay
	 call writefloat
	 call crlf
	 call waitmsg
	 call clrscr
	 call crlf
	 call crlf

	  MOV EBX,OFFSET CAPTION
	 MOV EDX,OFFSET QUESTION
	 call MsgBoxAsk
	 .IF EAX ==6
	 JMP L1
	 
	 .ELSEIF EAX==7
	 	 EXIT
	 
	 

	 .ENDIF

	 

	 .endif
	 .endif

	 ;-------Enregy Equation-------:
	 .if eax == 2
	 mov edx, offset sel2
	 call writestring
	 ;-------For E------:
	 call Readint
	 .if eax == 1
	 mov edx, offset findE
	 call writestring
	 call crlf
	 mov edx, offset speed
	 call writestring
	 call crlf

	 mov edx, offset masses
	 call writestring
	 call Readfloat
	 fstp mass
	 
	 fld spe
	 fld spe
	 fmul st(0),st(1)
	 fstp temps

	 fld mass
	 fld temps
	 fmul st(0),st(1)
	 
	 mov edx, offset loads
	 call writestring
	 mov edx, offset fress
	 call writestring
	 mov eax,2000
	 call delay
	 call writefloat
	 call crlf
	 call waitmsg
	 call clrscr
	 call crlf
	 call crlf

	  MOV EBX,OFFSET CAPTION
	 MOV EDX,OFFSET QUESTION
	 call MsgBoxAsk
	 .IF EAX ==6
	 JMP L1
	 
	 .ELSEIF EAX==7
	 	 EXIT
	 
	 

	 .ENDIF

	 ;-----For m-----:
	 .elseif eax == 2
	 mov edx, offset findm
	 call writestring

	 mov edx, offset speed
	 call writestring
	 call crlf

	 mov edx, offset Ens
	 call writestring
	 call Readfloat
	 fstp energy

	 fld spe
	 fld spe
	 fmul st(0),st(1)
	 fstp temps
	 
	 fld energy
	 fdiv temps

	 mov edx, offset loads
	 call writestring
	 mov edx, offset fmass
	 call writestring
	 mov eax,2000
	 call delay
	 call writefloat

	 
	 call crlf
	 call waitmsg
	 call clrscr
	 call crlf
	 call crlf

	 
	  MOV EBX,OFFSET CAPTION
	 MOV EDX,OFFSET QUESTION
	 call MsgBoxAsk
	 .IF EAX ==6
	 JMP L1
	 
	 .ELSEIF EAX==7
	 	 EXIT
	 
	 

	 .ENDIF
	 .endif
	 .endif

	 
	 .if eax == 3
	 mov edx, offset fsdas
	 call writestring
	 call crlf

	 MOV EBX,OFFSET CAPTION
	 MOV EDX,OFFSET QUESTION
	 call MsgBoxAsk
	 .IF EAX ==6
	 JMP L1
	 .ELSEIF EAX==7
	 EXIT

	 .ENDIF
	 .endif


	exit
main ENDP

END main