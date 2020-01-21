; Comment block below must be filled out completely for each assignment
; *************************************************************
; Student Name: Alyssa Kongswangwongsa
; COMSC-260 Fall 2019
; Date: 11/02/2019
; Assignment # 6
; Version of Visual Studio used (2015)(2017)(2019): 2019
; Did program compile? Yes
; Did program produce correct results? Yes
; Is code formatted correctly including indentation, spacing and vertical alignment? Yes
; Is every line of code commented? Yes
;
; Estimate of time in hours to complete assignment: 48 hours
;
; In a few words describe the main challenge in writing this program: 
; figuring out which digit should I use bt to copy and what kind of register I should use
;
; Short description of what program does:
; This program simulate a 32 bit roght rotator given by the prompt.
;
; *************************************************************
; Reminder: each assignment should be the result of your
; individual effort with no collaboration with other students.
;
; Reminder: every line of code must be commented and formatted
; per the ProgramExpectations.pdf file on the class web site
; *************************************************************


.386									;identifies minimum CPU for this program

.MODEL flat,stdcall						;flat - protected mode program
										;stdcall - enables calling of MS_windows programs

										;allocate memory for stack
										;(default stack size for 32 bit implementation is 1MB without .STACK directive 
										;  - default works for most situations)

.STACK 4096								;allocate 4096 bytes (1000h) for stack

mPrtChar  MACRO  arg1					;arg1 is replaced by the name of character to be displayed
         push eax						;save eax
         mov al, arg1					;character to display should be in al
         call WriteChar					;display character in al
         pop eax						;restore eax
ENDM


mPrtStr macro   arg1					;arg1 is replaced by the name of character to be displayed
         push edx						;save eax
         mov edx, offset arg1			;character to display should be in al
         call WriteString				;display character in al
         pop edx						;restore eax
ENDM

;*************************PROTOTYPES*****************************

ExitProcess PROTO, dwExitCode:DWORD    ;from Win32 api not Irvine to exit to dos with exit code

ReadChar PROTO						   ;Irvine code for getting a single char from keyboard
									   ;Character is stored in the al register.
									   ;Can be used to pause program execution until key is hit.

WriteChar PROTO						   ;Irvine code to write character stored in al to console

WriteDec PROTO                         ;Irvine code to write number stored in eax
                                       ;to console in decimal

WriteString proto		               ;Irvine code to write null-terminated string to output
						               ;EDX points to string

;************************  Constants  ***************************

LF       equ     0Ah                   ; ASCII Line Feed
$parm1 equ dword ptr [ebp + 8]
$parm2 equ dword ptr [ebp + 12]

;************************DATA SEGMENT***************************

.data

    ;inputs for testing the Shifter function
    inputA  byte 0,1,0,1,0,1,0,1
    inputB  byte 0,0,1,1,0,0,1,1
    inputC  byte 1,1,1,1,0,0,0,0
    ARRAY_SIZE equ $ - inputC         

    ;numbers for testing DoRightRotate
    nums   dword 10101010101010101010101010101011b
           dword 01010101010101010101010101010101b
           dword 11010101011101011101010101010111b
    NUM_SIZE EQU $-nums               ;total bytes in the nums array

    NUM_OF_BITS EQU SIZEOF(DWORD) * 8 ;Total bits for a dword

    ;You can add LFs to the strings below for proper output line spacing
    ;but do not change anything between the quotes "do not change".
    ;You can also combine messages where appropriate.

    ;I will be using a comparison program to compare your output to mine and
    ;the spacing must match exactly.

    endingMsg				byte "Hit any key to exit!",0

    ;Change my name to your name
    titleMsg				byte "Program 6 by Alyssa Kongswangwongsa",LF,0

    testingShifterMsg		byte LF, "Testing Shifter",0
    enabledMsg				byte LF,"(Shifting enabled C = 1, Disabled C = 0)",0

    testingDoRightRotateMsg byte LF,"Testing DoRightRotate",0

    header					byte "A B C | Output",LF,0

    original				byte " Original",LF,0
    disableShift			byte " Disable Rotate",LF,0
    enableShift				byte " Enable Rotate",LF,0
    shiftInst				byte " Rotate Instruction",LF,LF,0
    blankLine				byte LF,LF,0   ;blankLine may not be necessary

    dashes					byte LF,"------------------------------------",LF,0



;************************CODE SEGMENT****************************

.code

Main PROC

;start student code here
;See the pdf file for the pseudo code for the main function

	mPrtStr		titleMsg					;print out the titleMsg

	mPrtStr		testingShifterMsg			;print out the testingShifterMsg
	mPrtStr		enabledMsg					;print out the enabledMsg
	mPrtStr		dashes						;print out dashes
	mPrtStr		header						;print out header
	 
    mov			esi, 0						;esi is the offset from the beginning of the array.
											;Initialize esi to 0 since the first element of the
											;is at testArray + 0
loopTop1:

    cmp			esi, ARRAY_SIZE				;compare esi  to the total bytes of the array
    jae			done1						;if we have processed all bytes then done
    
	movzx		eax,  inputA[esi]			;copy the byte at [inputA + esi] into eax 
	call		WriteDec					;print out inputA in eax

	mPrtChar	' '							;print out a space

	movzx		eax,  inputB[esi]			;copy the byte at [inputB + esi] into eax
	mov			ebx,  eax					;ebx = eax (ready to be use as an input of Shifter function)
	call		WriteDec					;print out inputB in eax
	
	mPrtChar	' '							;print out a spac

	movzx		eax,  inputC[esi]			;copy the byte at [inputC + esi] into ecx
	mov			ecx, eax					;ecx = eax (ready to be use as an input of Shifter function)
	call		WriteDec					;print out inputC in eax

	mPrtChar	' '							;print out a spac
	
	movzx		eax,  inputA[esi]			;put inputA back into al to be use as an input of Shifter function

	mPrtChar	'|'							;print out a '|'
	mPrtChar	' '							;print out a spac

	call		Shifter						;call Shifter function to get the output
	call		WriteDec					;print out the output of Shifter in eax

	mov			al,  LF						;al = 0Ah
	call		WriteChar					;print out linefeed

    inc			esi							;esi contains the offset from the beginning of the array. 
											;add 1 to esi so that testArray + esi points to the 
											;next element of the byte array 
    jmp			loopTop1					;repeat
    
done1: 

	mPrtStr		testingDoRightRotateMsg		;print out the testingDoRightRotateMsg
	mPrtStr		dashes						;print out dashes

	mov			edi, 0						;edi is the offset from the beginning of the array.
											;Initialize edi to 0 since the first element of the
											;is at nums + 0

loopTop2:

    cmp			edi, NUM_SIZE				;compare esi to the total bytes of the array
    jae			done2						;if we have processed all bytes then done
    
	push		nums[edi]					;push the dword at [nums + edi] on to the stack
	call		DspBin						;print the original number in binary
	mPrtchar	' '							;print out a space
	mPrtStr		original					;print out the "original" message

	push		0							;push parameter 2 "0" on the stack (ready to be used as an input of DoRightRotate)
	push		nums[edi]					;push parameter 1 [nums + esi} on the stack (ready to be used as an input of DorightRotate)
	call		DoRightRotate				;call DoRightRotate function

	push		eax							;push eax onto the stack to be print out
	call		DspBin						;print out the binary in eax
	mPrtChar	' '							;print out a space
	mPrtStr		disableShift				;print out disableShift

	push		1							;push parameter 2 "1" on the stack (ready to be used as an input of DoRightRotate)
	push		nums[edi]					;push parameter 1 [nums + esi] on the stack (ready to be used as an input of DoRightRotate)
	call		DoRightRotate				;call DoRightRotate function

	push		eax							;push eax onto the stack to be print out
	call		DspBin						;print out the binary in eax
	mPrtChar	' '							;print out a space
	mPrtStr		enableShift					;print out enableShift

	mov			ebx, nums[edi]				;copy the dword at [nums + edi] into ebx
	ror			ebx, 1						;rotate to shift the bits of the binary (operand1) to the right
	
	push		ebx							;push the shifted binary in ebx onto the stack (ready to be print out)
	call		DspBin						;print out the binary in ebx
	mPrtChar ' '							;print out a space
	mPrtStr	shiftInst						;print out shiftInst

	add		edi, 4							;esi contains the offset from the beginning of the array. 
											;add 4 to edi so that nums + edi points to the 
											;next element of the dword array 

    jmp     loopTop2						;repeat
    
done2: 

	mPrtStr endingMsg						;print out endingMsg

    call    ReadChar						;pause execution
	INVOKE  ExitProcess,0					;exit to dos: like C++ exit(0)

Main ENDP


;************** DoRightRotate - Shift a dword right by 1
;
;       ENTRY – operand 2 (enable,disable shift) and operand 1 (number to shift) are on the stack
;                         
;       EXIT  - EAX = shifted or non shifted number
;       REGS  - EBX, ECX, EDX, EDI, AL, BL, DX
;
;       note: Before calling DoRightRotate push operand 2 onto the stack and then push operand 1.
;
;	    note: DoRightRotate calls the Shifter function to shift 1 bit.
;
;       to call DoRightRotate in main function:
;                                   push  0 or 1            ;1 to shift, 0 to disable shift
;                                   push  numberToShift     ;32 bit operand1
;                                   call DoRightRotate      ;result in eax
;
;       Note; at the end of this function use ret 8 (instead of just ret) to remove the parameters from the stack.
;                 Do not use add esp, 8 in the main function.
;--------------
;Do not access the arrays in main directly in the DoRightRotate function. 
;The data must be passed into this function via the stack.
;Note: the DoRightRotate function does not do any output. All the output is done in the main function
;
;Note: if shifting is disabled ($parm2 = 0) do not hardcode the return value to be
;equal to $parm1. If shifting is disabled you must still process all the bits
;through your Shifter function.
;
;In this function you will examine the bits from operand 1 in order from left to right using the BT instruction.

;See BT.asm on the class web site.

;You will use the BT instruction to copy the bits from operand 1 to the carry flag.
    
;Before the loop you will use the BT instruction to copy bit 0 to the carry flag then use a rotate instruction to copy the
;carry flag to the right end of al. This is to account for the rightmost bit being copied to the left end
;during a right rotate.
    
;Before the loop you will also set bl to the value of bit 31 (NUM_OF_BITS - 1) by using the following method:

;You will use the BT instruction to copy bit 31 (NUM_OF_BITS - 1) to the carry flag then use a rotate instruction to copy the
;carry flag to the right end of bl. Do not hardcode 31. Use NUM_OF_BITS - 1.

;Then you will populate ecx with operand 2 which is the enable (1) or disable bit(0).

;then call the shifter function.

;after calling the shifter function you will transfer the return value from al to 
;the right end of the register you are using to accumulate shifted or non shifted bits which should have been initialized to 0.

;Then you will have a loop that will execute (NUM_OF_BITS - 1) times(31 times).
;You should use 0 as the terminating loop condition.

;The counter for the loop begins at NUM_OF_BITS - 1
;NOTE: you cannot use ecx for the counter since it contains the enable or disable bit.

;In the loop you will do the following:
;clear al and bl
;Use the BT instruction to copy the bit at position of the counter to the carry flag 
;and from the carry flag to the right end of al.

;Then use the BT instruction to copy the bit at position of the counter - 1 to the carry flag 
;and from the carry flag to the right end of bl.

;ecx should still be populated with the value of operand2 assigned before the loop.

;Then call the shifter function

;The bit returned by the Shifter function in eax should be copied to the carry flag using the BT instruction.
;You cannot use the BT instruction on a byte like al. You must use BT on a word (AX) or dword (EAX).

;then copy the bit from the carry flag using a rotate instruction to the right end
;of the register used to accumulate the shifted or non shifted bits.

;after the loop exits make sure the shifted or non shifted bits are in eax

;Each iteration of the loop should process the bits as follows:
    
;al = bit at position counter except when rightmost bit rotated into right end(shifted bit)
;bl = bit a position counter - 1 (original bit)
;
;
;		  Bit #	
;	counter | al  bl
;	    31	|  0  31   (rightmost bit rotated into left end)
;    Above is before loop
;    Below is in loop
;	    31	| 31  30
;	    30	| 30  29
;	    29	| 29  28
;	    28	| 28  27
;	    27	| 27  26
; etc down to bit 0 (when counter is 0, the loop is done)
;        1  |  1   0
;        0  |  done   



;You should save any registers whose values change in this function 
;using push and restore them with pop.
;
;The saving of the registers should
;be done at the top of the function and the restoring should be done at
;the bottom of the function.
;
;Note: do not save any registers that return a value (eax).
;
;Each line of the Shifter function must be commented and you must use the 
;usual indentation and formating like in the main function.
;
;Don't forget the "ret 8" instruction at the end of the function
;
;Do not delete this comment block. Every function should have 
;a comment block before it describing the function. FA18

DoRightRotate proc
	
	push	ebp							;save original value of EBP on the stack
	mov		ebp, esp					;EBP = address of original EBP on the stack (00f8)

	push	ebx							;save ebx on the stack 
	push	ecx							;save ecx on the stack 
	push	edx							;save edx on the stack
	push	esi							;save esi on the stack 
	push	edi							;save edi on the stack

	mov		edx, 0						;edx = 0 (ready to be used as accumulator)
		
	bt		$parm1, 0					;copy bit 0 in parm1 to CF
	rcl		al, 1					    ;copy CF to the right end of al
	
	bt		$parm1, NUM_OF_BITS - 1		;copy bit NUM_OF_BITS - 1 in parm1 to CF
	rcl		bl, 1					    ;copy CF to the right end of bl

	mov		ecx, $parm2					;ecx = parm2 (determine whether the shift is enabled (1) or disabled (0))
	call	Shifter						;call Shifter function

	mov		dl, al						;transfer al into edi to be used as accumulator for shifted and non-shifted bits
	
	mov		esi, NUM_OF_BITS - 1		;initialize loop counter		

loopTop:

    cmp     esi, 0 						;compare esi to 0
    je      done						;if we have processed all the bits then done

    xor     al, al						;clear al
	xor		bl, bl						;clear bl

    bt      $parm1, esi					;copy bit in parm1 at position in esi to CF
    rcl     al, 1						;copy carry flag to right end of al

	mov		edi, esi					;edi = esi 
	sub		edi, 1						;subtract the counter by 1
	bt		$parm1, edi					;copy bit in parm1 at position in edi to CF 
	rcl		bl, 1						;copy CF into the right end of bl

    call	Shifter						;call Shifter 

	bt		eax, 0					    ;copy bit returned by Shifter in al to CF
	rcl		edx, 1						;copy bit from CF to right end of edx (the accumulator) 

    dec     esi							;decrement counter   
    jmp     loopTop						;repeat
    
done: 

	mov		eax, edx					;move the all the shifted or non-shifted bits into eax ready to be the output

	pop		edi							;restore edi
	pop		esi							;restore edi
	pop		edx							;restore edx
	pop		ecx							;restore ecx
	pop		ebx							;restore ebx

	pop		ebp							;restore ebp
	
	ret 8								;ret 8 removes 2 dword parameters from stack

DoRightRotate endp





;************** Shifter – Simulate a partial shifter circuit per the circuit diagram in the pdf file.  
;  Shifter will simulate part of a shifter circuit that will input 
;  3 bits and output a shifted or non-shifted bit.
;
;
;   CL--------------------------
;              |               |    
;              |               |     
;              |    AL        NOT    BL
;              |     |         |     |
;              --AND--         --AND--
;                 |                |
;                 --------OR--------
;                          |
;                          AL
;
; NOTE: To implement the NOT gate use XOR to flip a single bit.
;
; Each input and output represents one bit.
;
;  Note: do not access the arrays in main directly in the Adder function. 
;        The data must be passed into this function via the required registers below.
;
;       ENTRY - AL = input bit A 
;               BL = input bit B
;               CL = enable (1) or disable (0) shift
;       EXIT  - AL = shifted or non-shifted bit
;       REGS  - EDX, DL
;
;       For the inputs in the input columns you should get the 
;       output in the output column below.
;
;The chart below shows the output for 
;the given inputs if shifting is enabled (cl = 1)
;If shift is enabled (cl = 1) then output should be the shifted bit (al).
;In the table below shifting is enabled (cl = 1)
;
;        input      output
;     al   bl  cl |   al 
;--------------------------
;      0   0   1  |   0 
;      1   0   1  |   1 
;      0   1   1  |   0 
;      1   1   1  |   1   
;
;The chart below shows the output for 
;the given inputs if shifting is disabled (cl = 0)
;If shift is disabled (cl = 0) then the output should be the non-shifted bit (B).

;        input      output
;     al   bl  cl |   al 
;--------------------------
;      0   0   0  |   0 
;      1   0   0  |   0 
;      0   1   0  |   1 
;      1   1   0  |   1   

;
;Note: the Shifter function does not do any output to the console.All the output is done in the main function
;
;Do not access the arrays in main directly in the shifter function. 
;The data must be passed into this function via the required registers.
;
;Do not change the name of the Shifter function.
;
;See additional specifications for the Shifter function on the 
;class web site.
;
;You should use AND, OR and XOR to simulate the shifter circuit.
;
;Note: to flip a single bit use XOR do not use NOT.
;
;You should save any registers whose values change in this function 
;using push and restore them with pop.
;
;The saving of the registers should
;be done at the top of the function and the restoring should be done at
;the bottom of the function.
;
;Note: do not save any registers that return a value (eax).
;
;Each line of this function must be commented and you must use the 
;usual indentation and formating like in the main function.
;
;Don't forget the "ret" instruction at the end of the function
;
;Do not delete this comment block. Every function should have 
;a comment block before it describing the function. FA18


Shifter proc

	push	edx		    ;save edx on the stack 
	
	mov		dl, cl		;dl = cl 

	and		al, cl		;al = al and cl 
	xor		dl, 1		;dl = not dl (use xor to perform not instruction)
	and		bl, dl		;bl = bl and dl
	or		al, bl		;al = al or bl

	pop		edx			;restore edx

	ret				    ;ret pops return address off stack and puts it into eip register

Shifter endp




;************** DspBin - display a Dword in binary including leading zeros
;
;       ENTRY –operand1, the number to print in binary, is on the stack
;
;       For Example if parm1 contained contained AC123h the following would print:
;                00000000000010101100000100100011
;       For Example if parm1 contained 0005h the following would print:
;                00000000000000000000000000000101
;
;       EXIT  - None
;       REGS  - List registers you use
;
; to call DspBin:
;               push 1111000110100b    ;number to print in binary is on the stack
;               call DspBin            ; 00000000000000000001111000110100 should print
;     
;       Note: leading zeros do print
;       Note; at the end of this function use ret 4 (instead of just ret) to remove the parameter from the stack
;                 Do not use add esp, 4 in the main function.
;--------------

    ;You should have a loop that will do the following:
    ;The loop should execute NUM_OF_BITS times(32 times) times so that all binary digits will print including leading 0s.
    ;You should use the NUM_OF_BITS constant as the terminating loop condition and not hard code it.
    
    ;You should start at bit 31 down to and including bit 0 so that the digits will 
    ;   print in the correct order, left to right.
    ;Each iteration of the loop will print one binary digit.

    ;Each time through the loop you should do the following:
    
    ;You should use the BT instruction to copy the bit starting at position 31 to the carry flag 
    ;   then use a rotate command to copy the carry flag to the right end of al.

    ;then convert the 1 or 0 to a character ('1' or '0') and print it with WriteChar.
    ;You should keep processing the number until all 32 bits have been printed from bit 31 to bit 0. 
    
    ;Efficiency counts.

    ;DspBin just prints the raw binary number.

    ;No credit will be given for a solution that uses mul, imul, div or idiv. 
    ;
    ;You should save any registers whose values change in this function 
    ;using push and restore them with pop.
    ;
    ;The saving of the registers should
    ;be done at the top of the function and the restoring should be done at
    ;the bottom of the function.
    ;
    ;Each line of this function must be commented and you must use the 
    ;usual indentation and formating like in the main function.
    ;
    ;Don't forget the "ret 4" instruction at the end of the function
    ;
    ;
    ;Do not delete this comment block. Every function should have 
    ;a comment block before it describing the function. FA17


DspBin proc
	
	push	ebp						;save original value of EBP on the stack
	mov		ebp, esp				;EBP = address of original EBP on the stack (00f8)

	push	eax						;save eax on the stack
	push	ebx						;save ebx on the stack
	
    mov     ebx, NUM_OF_BITS-1	    ;initialize counter

loopTop:

    cmp     ebx, -1	                ;compare ebx to the total bits of the number
    je      done					;if we have processed all the bits then done

	xor		al, al					;clear al
    bt      $parm1, ebx             ;copy bit in parm1 at position ebx to carry flag
    rcl     al, 1					;copy carry flag to right end of al
    or	    al, 00110000b	        ;convert the digit in al to ascii character to be printed
    call    WriteChar               ;Print digit to screen		
    dec     ebx                     ;decrement counter   
    jmp     loopTop                 ;repeat
    
done: 

	pop		ebx					    ;restore ebx
	pop		eax					    ;restore eax
	pop		ebp					    ;restore ebp

	ret 4					        ;ret 4 removes a deword parameter from stack

DspBin endp

END Main
