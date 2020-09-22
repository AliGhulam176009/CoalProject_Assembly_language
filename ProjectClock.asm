INCLUDE Irvine32.inc
INCLUDE Macros.inc
			;;/////////// Structure defined getting system time, also used 
			;;/////////// in other function 
SYSTEMTIME STRUCT
	wYear WORD ?			; year (4 digits)
	wMonth WORD ?			; month (1-12)
	wDayOfWeek WORD ?		; day of week (0-6)
	wDay WORD ?			; day (1-31)
	wHour WORD ?			; hours (0-23)
	wMinute WORD ?			; minutes (0-59)
	wSecond WORD ?			; seconds (0-59)
	wMilliseconds WORD ?	; milliseconds (0-999)
SYSTEMTIME ENDS
			;;//////////// Data segemntn,defining Data Variables

.data
sysTime SYSTEMTIME <>

 remindDate word 0 
 remindMonth word 0
 remindYear word 0
 isReminder byte 0			;(0/1 only two values)
 reminderMsg byte 20 dup(?)

 newDate  word ?
 newMonth word ?
 newYear  word ?

startTime DWORD ?
timeElasped DWORD ?
countDays word 0
index word 0

PMonth word ?
PYear word ?
PDate word ?

showMonth WORD ?
showYear WORD ?

IsLeap byte 0
M30or31 word ?
DayOf1 byte ?
divisor word ?
monthCheck2 WORD 4,6,9,11

DAYS byte  'SUN ','MON ' ,'TUE ' ,'WED ' ,'THU ' ,'FRI ' ,'SAT ',0

MonthInteger byte 0,1,2,3,4,5,6,7,8,9,10,11
MONTHS	byte 'JAN' ,'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC',0

			;;/////////////CODE SEGMENT starts here!!
.code

;;///////////// PRINT FUNCTION//////////////////;
;* Recieves address of structure conataning Date and Time information in ESI 
;* ESI = offset of Structure 
;*
;*show time and date on console 
;* STATUS= OK
;************************************************************
print PROC uses eax ebx ecx edx edi
	call ClearTimeArea

	mov dl,40
	mov dh,10
	call Gotoxy
							;;///// display time /////////////;
	movzx eax,(SYSTEMTIME PTR [esi]).wHour
	call WriteDec               ; Displays hour (hh)

	mov al,':'
	call WriteChar				    
	movzx eax,(SYSTEMTIME PTR [esi]).wMinute
	call WriteDec
 
	mov al,':'
	call WriteChar
	movzx eax,(SYSTEMTIME PTR [esi]).wSecond
	call WriteDec
 
 	;mov al,':'
	;call WriteChar
	;movzx eax,(SYSTEMTIME PTR [esi]).wMilliseconds
	;call WriteDec
	 
	mov dl,40
	mov dh,11
	call Gotoxy
							;;///// Displaying Date!!! /////////////;
	;mWrite <" Current Date:",0dh,0ah>

	movzx eax,(SYSTEMTIME PTR [esi]).wDay
	call WriteDec
 
	mov al,'-'
	call WriteChar
	movzx eax,(SYSTEMTIME PTR [esi]).wMonth
	call WriteDec

	mov al,'-'
	call WriteChar
	movzx eax,(SYSTEMTIME PTR [esi]).wYear
	call WriteDec
	

	mov dl,40
	mov dh,12
	call Gotoxy

	mov edx,0				    
	mov dx,(SYSTEMTIME PTR [esi]).wDayOfWeek
	mov ecx, 4
	imul dx,4
	mov edi, offset DAYS
	add edi, edx
	call PrintSubString
	call crlf
ret
print ENDP				;;;PRINT FUNCTION ends here!!


;****************************************************
;;///////////// Print Sub String FUNCTION//////////////////;
;* recieves:
;* EDI = starting offset
;* ECX = no of chars to write on console
;*returns:
; just prints on console
;
;STATUS=  OK
;****************************************************
PrintSubString PROC uses eax
	continue:
		mov al,[edi]
		call WriteChar
		inc edi
	loop continue
ret 
PrintSubString ENDP


;****************************************************
;;///////////// LEAP FUNCTION//////////////////;
;* PYear= pass the to check
;* formula: ()
;*returns:
; IsLeap=0/1
;
;STATUS= OK
;****************************************************
Leap PROC uses eax edx
	movzx eax,PYear
	mov edx,0
	mov divisor, 4
	DIV divisor
	cmp dx,0
	jne noLeap

yesLeap:
	mov IsLeap,1
	jmp wapis
noLeap:	
	mov IsLeap,0
wapis:
ret
Leap ENDP

;****************************************************
;;///////////// MonthDays FUNCTION//////////////////;
;* Recieves:
;* PMonth = month
;* PYear= year
;*
;*returns:
; M30or31 = (28/29/30/31)?
;
;STATUS= OK
;****************************************************
MonthDays PROC uses ecx edx edi
push PMonth
	mov edi,offset monthCheck2 
	mov ecx,lengthof monthCheck2
	testing:
		mov dx,[edi]
		cmp PMonth,dx
		jne continueIt
			mov M30or31,30
			jmp khatam
		continueIt:
	add edi,2
	loop testing

		cmp PMonth,2
		je february
		mov M30or31, 31
		jmp khatam

	february:
		call Leap
		cmp IsLeap,0
		je leapNi
		mov M30or31,29
		jmp khatam
		leapNi:
		mov M30or31,28
	khatam:
pop PMonth
ret
MonthDays ENDP

;****************************************************
;;///////////// FirstDay FUNCTION//////////////////;
;* Recieves:
;* PMonth = month next /previous to current month
;* PYear = year accordingly
;*
;*returns:
; DayOf1=(0-6)
;* STATUS= OK
;****************************************************
FirstDay PROC uses eax ebx ecx edx 
push M30or31
push PMonth 
push PYear
mov bx,(SYSTEMTIME PTR [esi]).wMonth			;current month
mov cx,(SYSTEMTIME PTR [esi]).wYear			; current year
	
	;determine 1st of current month:
	mov ax, (SYSTEMTIME PTR [esi]).wDay
	mov divisor,7
	mov dx,0
	div divisor
	mov ax,dx

	.IF ax ==0
		mov dx,(SYSTEMTIME PTR [esi]).wDayOfWeek
		inc dx
	.ELSEIF ax ==1
		mov dx,(SYSTEMTIME PTR [esi]).wDayOfWeek
	.ELSEIF ax==2
		mov dx,(SYSTEMTIME PTR [esi]).wDayOfWeek
		sub dx,1
	.ELSEIF ax == 3
		mov dx,(SYSTEMTIME PTR [esi]).wDayOfWeek
		sub dx,2
	.ELSEIF ax == 4
		mov dx,(SYSTEMTIME PTR [esi]).wDayOfWeek
		sub dx,3
	.ELSEIF ax == 5
		mov dx,(SYSTEMTIME PTR [esi]).wDayOfWeek
		sub dx,4
	.ELSEIF ax == 6
		mov dx,(SYSTEMTIME PTR [esi]).wDayOfWeek
		sub dx,5
	.ELSEIF ax == 7
		mov dx,(SYSTEMTIME PTR [esi]).wDayOfWeek
		sub dx,6
	.ENDIF

	.IF dx < 0
		imul dx,-1
	
	.WHILE dx > 0
		cmp (SYSTEMTIME PTR [esi]).wDayOfWeek,6
	jbe moveToDays
		mov (SYSTEMTIME PTR [esi]).wDayOfWeek,0
	moveToDays:
	.ENDW

	.ENDIF
	
	mov DayOf1,dl
	
	;; now mov to next part (finding 1st day of required month)
	cmp PYear, cx
	ja greaterMonth
	jb smallerMonth
equalYear:
		cmp  PMonth, bx
		jb smallerMonth
		je equalDate
greaterMonth:				;;greater month than current month

	.WHILE (bx < PMonth  && cx == PYear) || cx < PYear
		mov PMonth, bx
		mov PYear, cx
		call MonthDays
		mov ax, M30or31
		movzx dx, DayOf1
		add ax, dx
		mov dl,7
		div dl
		mov DayOf1, ah
		inc bx
		.IF  bx > 12
			inc cx
			mov bx, 1
		.ENDIF
					;restore values of PMonth and PYear
		pop PYear
		pop PMonth
		
		push PMonth 
		push PYear

	.ENDW
	jmp returning

smallerMonth:
	.WHILE (bx > PMonth  && cx == PYear) || cx > PYear
		mov PMonth, bx
		dec PMonth
		mov PYear, cx
		.IF  PMonth < 1
			mov PMonth, 12 
			dec PYear
		.ENDIF
		call MonthDays
		mov ax, M30or31
		mov dl, 7
		div dl
		;;this loop implemented for reversing DayOf1 value to 6 atrer decreasing from 0 (i.e. -1 =6) 
		.WHILE ah > 0
			dec ah
			dec DayOf1
			cmp DayOf1,255
			jb stillPositive
			mov DayOf1,6	
			stillPositive:
		.ENDW
		dec bx		;decreasing Month
		.IF  bx < 1
			dec cx
			mov bx, 12
		.ENDIF	
		pop PYear					;restore values of PMonth and PYear
		pop PMonth
		
		push PMonth 
		push PYear
	.ENDW
	jmp returning

equalDate:
returning:
	pop PYear
	pop PMonth
	pop M30or31
ret
FirstDay ENDP

;****************************************************
;;///////////// CALENDAR FUNCTION//////////////////;
;*
;* ==>>prints Calendar of Month and year passed
;*
;*  PMonth= month to be printed
;*  PYear= year to be pritned
;* STATUS= OK
;****************************************************
Calendar PROC uses eax ebx ecx edx 
	
	call ClearCalendarArea

	mov dl,6	  ;col
	mov dh,10	  ;row
	call Gotoxy
					;;; print month NAME to be printed in EAX
	movzx eax, PMonth
	dec eax
	imul eax,3
	mov edi,[offset MONTHS]
	add edi,eax
	mov ecx,3
	call PrintSubString

	mov al,','
	call WriteChar
	movzx eax,PYear
	call WriteDec
	call Crlf
				;;printing table of calendar
	mov edx, offset DAYS
	call WriteString

	call MonthDays
	call FirstDay			;returned in DayOf1=(0-6)
	movzx bx,DayOf1

 	mov eax,1
	movzx ecx,M30or31
	mov dh,12

	.WHILE ecx > 0
		.WHILE bx <= 6
			call MonthDays
			cmp ax,M30or31
			ja completed
			push bx
			imul bx, 4
			mov dl,bl
			call Gotoxy
			call WriteDec
			inc ax
			pop bx
			inc bx
			dec ecx
		.ENDW
		inc dh
		mov bx,0
	.ENDW
completed:
call crlf

ret
Calendar ENDP


;****************************************************
;;///////////// CHECK REMINDER FUNCTION//////////////////;
;* Recieves:
;* remindDate, remindMonth, remindYear
;* ==>>show message only for 1 minute if date of reminder matches with current DATE
;*
;* STATUS= OK
;****************************************************
checkReminder PROC uses edx eax 
	
	
	mov ax,(SYSTEMTIME PTR [esi]).wDay
	cmp remindDate,ax
	jne endReminder
	mov ax,(SYSTEMTIME PTR [esi]).wMonth
	cmp remindMonth, ax
	jne endReminder
	mov ax, (SYSTEMTIME PTR [esi]).wYear
	cmp remindYear, ax
	jne endReminder
				;;remind User
	mov dl,0
	mov dh,20
	call Gotoxy 
	call ClearArea
	mov dl,0
	mov dh,20
	call Gotoxy 
	mWrite <" REMINDER:",0dh,0ah>
	mov edx, offset reminderMsg
	call WriteString
	mov isReminder,0
endReminder:

ret 
checkReminder ENDP

;****************************************************
;;///////////// Clear Interupt Message Area FUNCTION//////////////////;
;* Recieves: Nothing
;* 
;* Clears area where all interupts messages will be shown!!
;****************************************************
ClearArea PROC uses edx ecx
	mov dl, 0
	mov dh, 20

	mov ecx,12
	looping:
	call Gotoxy
	mWrite <"                                                                      ",0dh,0ah>
	inc dh
	loop looping
ret 
ClearArea ENDP

;****************************************************
;;///////////// Clear Time Area FUNCTION//////////////////;
;* Recieves: Nothing
;* Clears area where time is shown!!
;****************************************************
ClearTimeArea PROC uses edx ecx
	mov dl, 39
	mov dh, 10

	mov ecx,3
	looping:
	call Gotoxy
	mWrite <"                                    ",0dh,0ah>
	inc dh
	loop looping
ret 
ClearTimeArea ENDP

;****************************************************
;;///////////// Clear Calendar Area FUNCTION//////////////////;
;* Recieves: Nothing
;* Clears area where calendar  is shown!!
;****************************************************
ClearCalendarArea PROC uses edx ecx
	mov dl, 0
	mov dh, 10

	mov ecx,8
	looping:
	call Gotoxy
	mWrite <"                                  ",0dh,0ah>
	inc dh
	loop looping
ret 
ClearCalendarArea ENDP


;****************************************************
;;///////////// CORRECT TIME FUNCTION//////////////////;
;* Recieves: 
;*	timeElasped = time used during any execution
;* Output:
;* changes the values of SystemTime Struct accordingly!
;****************************************************
CorrectTime PROC uses eax edx ebx ecx
	mov edx, 0
	mov eax, timeElasped
	mov ebx,1000
	div ebx
	
	cmp  eax,0
	jbe skipping
		mov ecx, eax
		loopStructure:
			inc (SYSTEMTIME PTR [esi]).wSecond
			cmp (SYSTEMTIME PTR [esi]).wSecond,59
			jbe continueIt
			mov (SYSTEMTIME PTR [esi]).wSecond,0
			inc (SYSTEMTIME PTR [esi]).wMinute
			continueIt:
		loop loopStructure
	skipping:
ret
CorrectTime ENDP

;****************************************************
;;///////////// CLOCK FUNCTION//////////////////;
;* Recieves address of structure conataning Date and Time information in ESI 
;* ESI = offset of Structure
;*
;*Prints clock,date and also controls the calendar display!!!
;*
;*Actually it the most important function!
;
; Press "F1" to end program!
;* Press "F2" for Next month
;* Press "F3" for Previous month
;* Press "F4" to Reset time
;* Press "F5" to Reset Date
;* Press "F6" to Set Reminder
;****************************************************
Clock PROC

mov ax,(SYSTEMTIME PTR [esi]).wMonth
mov PMonth, ax
mov showMonth, ax
mov ax,(SYSTEMTIME PTR [esi]).wyear
mov PYear, ax
mov showYear, ax
call Calendar

again:					;;main loop starts here 


	call print
	call GetMseconds				; get starting tick count in EAX
	mov startTime,eax				; save it in 'startTime' function!

	inc (SYSTEMTIME PTR [esi]).wSecond
									;; Seconds Checking
	cmp (SYSTEMTIME PTR [esi]).wSecond, 59
	jbe cont
	mov (SYSTEMTIME PTR [esi]).wSecond,0
	inc (SYSTEMTIME PTR [esi]).wMinute
cmp isReminder,1
jne noReminder
call checkReminder
jmp hadReminder
NoReminder:				
	call ClearArea						
hadReminder:
									;; Minutes Checking
	cmp (SYSTEMTIME PTR [esi]).wMinute, 59 
	jbe cont
	mov (SYSTEMTIME PTR [esi]).wMinute,0
	inc (SYSTEMTIME PTR [esi]).wHour
									;; Hours Checking
	cmp (SYSTEMTIME PTR [esi]).wHour, 23
	jbe cont
	mov (SYSTEMTIME PTR [esi]).wHour,0
	inc (SYSTEMTIME PTR [esi]).wDay
	inc (SYSTEMTIME PTR [esi]).wDayOfWeek
									;;Day Of Week Checking
	cmp (SYSTEMTIME PTR [esi]).wDayOfWeek,6
	jbe moveToDays
	mov (SYSTEMTIME PTR [esi]).wDayOfWeek,0
moveToDays:
										;;checking all month days (28/29/30/31)?
	mov ax,(SYSTEMTIME PTR [esi]).wMonth
	mov PMonth,ax
	mov ax,(SYSTEMTIME PTR [esi]).wyear
	mov PYear,ax
	call MonthDays
	mov dx, M30or31

	cmp (SYSTEMTIME PTR [esi]).wDay,dx
	jbe cont
	mov (SYSTEMTIME PTR [esi]).wDay,1
	inc (SYSTEMTIME PTR [esi]).wMonth
										;; Months Checking
	cmp (SYSTEMTIME PTR [esi]).wMonth,12
	jbe newCalendar
	mov (SYSTEMTIME PTR [esi]).wMonth,1
	inc (SYSTEMTIME PTR [esi]).wYear
	newCalendar:
		mov ax,(SYSTEMTIME PTR [esi]).wMonth
		mov PMonth, ax
		mov showMonth, ax
		mov ax,(SYSTEMTIME PTR [esi]).wyear
		mov PYear, ax
		mov showYear, ax
		call Calendar
	cont:

	call GetMseconds				; get new tick count
	sub eax,startTime				; get elapsed milliseconds
	;call WriteDec				
	;mWrite <" milliseconds have elapsed",0dh,0ah>
	
	mov ebx,1000
	;sub ebx,eax
	mov eax,ebx
	call delay
	call ReadKey				; wait for a keypress
	
	cmp ah,3Bh				; F1 for ending program
	je EndHere

	cmp ah,3Ch				; F2 for showing next month
	je NextMonth

	cmp ah,3Dh				; F3 for showing prev month
	je PrevMonth
		
	cmp ah,3Eh				; F4 for resetting time
	je ResetTime	

	cmp ah,3Fh				; F5 for resetting Date
	je ResetDate

	cmp ah,40h				; F6 for setting reminder
	je SetReminder
	jmp ending

NextMonth:
	
	inc showMonth
	cmp showMonth, 12
	jbe chalnyDo
		inc showYear
		mov showMonth, 1
	chalnyDo:
	
	mov ax,showYear
	mov PYear,ax	
	mov ax,showMonth
	mov PMonth,ax
		call Calendar
		jmp ending

PrevMonth:

	dec showMonth
	cmp showMonth, 1
	jae janyDo
		sub showYear,1
		mov showMonth, 12
	janyDo:
	
	mov ax,showYear
	mov PYear,ax	
	mov ax,showMonth
	mov PMonth,ax
		call Calendar
		jmp ending
	
ResetTime:
	call ClearArea
	mov dl,0	  ;col
	mov dh,20	  ;row
	call Gotoxy

	mWrite <"Enter Hour? (0-23)",0dh,0ah>
	call ReadDec
	mov (SYSTEMTIME PTR [esi]).wHour, ax

	mWrite <"Enter Minutes? (0-59)",0dh,0ah>
	call ReadDec
	mov (SYSTEMTIME PTR [esi]).wMinute, ax

	mWrite <"EnterSecond? (0-59)",0dh,0ah>
	call ReadDec
	mov (SYSTEMTIME PTR [esi]).wSecond, ax
	jmp ending

ResetDate:	 
	call GetMseconds
	mov timeElasped,eax	
	
	call ClearArea
	mov dl,0	  ;col
	mov dh,20	  ;row
	call Gotoxy
	
	mWrite <"Enter Date? (1-30)",0dh,0ah>
	call ReadDec
	mov newDate, ax

	mWrite <"Enter Month? (1-12)",0dh,0ah>
	call ReadDec
	mov newMonth, ax

	mWrite <"Enter Year? ",0dh,0ah>
	call ReadDec
	mov newYear, ax
						;correcting Day Of Week after changing Date
	mov ax,newMonth
	mov PMonth, ax
	mov ax, newYear
	mov PYear, ax
	call FirstDay
	push dx
	movzx dx,DayOf1
	mov ax, newDate
	add ax,dx
	dec ax
	mov dx,0
	mov divisor,7
	div divisor
	mov (SYSTEMTIME PTR [esi]).wDayOfWeek, dx
	pop dx
	mov ax,newDate
	mov (SYSTEMTIME PTR [esi]).wDay, ax
	mov ax,newMonth
	mov (SYSTEMTIME PTR [esi]).wMonth, ax
	mov ax, newYear
	mov (SYSTEMTIME PTR [esi]).wYear, ax

	call GetMseconds				; get new tick count
	sub eax,timeElasped				; get elapsed milliseconds
	mov timeElasped, eax
	call CorrectTime
	jmp ending

SetReminder:
	call GetMseconds	
	mov timeElasped,eax	
	
	call ClearArea
	mov dl,0	  ;col
	mov dh,20	  ;row
	call Gotoxy

	mWrite <"Enter Date for reminder? ",0dh,0ah>
	call ReadDec
	mov remindDate, ax

	mWrite <"Enter Month for reminder? ",0dh,0ah>
	call ReadDec
	mov remindMonth, ax

	mWrite <"Enter Year for reminder? ",0dh,0ah>
	call ReadDec
	mov remindYear, ax

	mWrite <"Enter Message for reminder?    (0-20 characters)",0dh,0ah>
	mov edx, offset reminderMsg
	mov ecx,20
	call ReadString 
	mov isReminder, 1

	call GetMseconds				; get new tick count
	sub eax,timeElasped				; get elapsed milliseconds
	mov timeElasped, eax
	call CorrectTime
ending:
jmp again

EndHere:
ret
Clock ENDP


;****************************************************
;;///////////// PRINT BORDER FUNCTION//////////////////;
;*
;* Recieves: Nothing
;* ==>>Just Print outer border!
;* STATUS= OK
;****************************************************
PrintBorder PROC uses edx eax 
	mov eax,185
	call SetTextColor
	
	mov dl, 0
	mov dh, 7
	call Gotoxy
	mov ecx,80
	printingBorder:
	mWrite <"=">
	loop printingBorder

	mov dl,35
	mov dh,8
	call Gotoxy
	mov ecx, 10
	printingBorder1:
	mWrite <"||">
	add dh,1
	call Gotoxy
	loop printingBorder1

	mov dl, 0
	mov dh, 18
	call Gotoxy
	mov ecx,80
	printingBorder4:
	mWrite <"=">
	loop printingBorder4

	mov eax,15
	call SetTextColor
ret
PrintBorder ENDP

;;****************************************
;	MAIN FUNCTION					
;*****************************************

main PROC
		
mov dl,0
mov dh, 0
call Gotoxy
mWrite <"Project: Clock And Calendar Application",0dh,0ah>
mWrite <"Developers Team: ",0dh,0ah,0dh,0ah>
mWrite <"Muhammad Umair Qadir    2014-cs-4",0dh,0ah>
mWrite <"Anees Akhtar            2014-cs-15 ",0dh,0ah>
mWrite <"Saqib Faiz              2014-cs-16 ",0dh,0ah,0dh,0ah>

mov dl,0
mov dh,35
call Gotoxy
mWrite <"Press F1 to End Program ",0dh,0ah>
mWrite <"Press F2 to View Calendar Of Next Month ",0dh,0ah>
mWrite <"Press F3 to View Calendar Of Previous Month",0dh,0ah>
mWrite <"Press F4 to Reset Time",0dh,0ah>
mWrite <"Press F5 to Reset Date",0dh,0ah>
mWrite <"Press F6 to Set Reminder ",0dh,0ah>
mov dl,0
mov dh,0
call Gotoxy
								;;;;; => passing address of structure to register ESI
	call PrintBorder
	INVOKE GetLocalTime, ADDR sysTime
	mov esi,OFFSET sysTime
	call Clock

	;mov PMonth,6
	;mov PYear,2016
	;call FirstDay

	;movzx eax, DayOf1
	;call WriteDec


call ClearArea
mov dl,0
mov dh, 20
call Gotoxy
mWrite <"Thanx for Visiting !!! ",0dh,0ah>

exit
main ENDP
END main