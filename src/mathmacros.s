.define MULTINA			$d770
.define MULTINB			$d774

.define MULTOUT			$d778

.define DIVOUTWHOLE		$d768
.define DIVOUTFRACT		$d76c

.define sx $40			; sin
.define sy $44
.define sz $48
.define cx $4c			; cos
.define cy $50
.define cz $54

.define t1 $60
.define t2 $64
.define t3 $68
.define t4 $6c
.define t5 $70
.define t6 $74

.define FP_A			$80
.define FP_B 			$84
.define FP_C			$88
.define FP_R			$8c

.define m11 $90
.define m12 $94
.define m13 $98
.define m21 $9c
.define m22 $a0
.define m23 $a4
.define m31 $a8
.define m32 $ac
.define m33 $b0

.define fx $d0
.define fy $d4
.define fz $d8

.macro MATH_ADD from, with, to
.scope
		ldq from
		clc
		adcq with
        stq to
.endscope
.endmacro

.macro MATH_SUB from, with, to
.scope
		ldq from
		sec
		sbcq with
        stq to
.endscope
.endmacro

.macro MATH_MOV from, to
		ldq from
		stq to
.endmacro

.macro MATH_NEG from, to
        lda #0
        tax
        tay
        taz
        sec
        sbcq from
        stq to
.endmacro

.macro MATH_NEG_DIRECT from
		lda #0
		tax
		tay
		taz
		sec
		sbcq from
.endmacro

.macro MATH_ABS from, to
.scope
        bit from+3
        bpl pos
        MATH_NEG from, to
        bra end
pos		MATH_MOV from, to
end
.endscope
.endmacro

.macro MATH_DOT3 px, m1, py, m2, pz, m3, output
		MATH_MUL pz, m3, t2
		MATH_MUL py, m2, t1
		MATH_MUL_DIRECT px, m1
		clc
		adcq t1
		adcq t2
		stq output
.endmacro

.macro MATH_MUL_APOS opB, result
.scope
						bit opB+3
						bpl posresult
negresult:				MATH_NEG opB, MULTINB			; a is positive, b is negative - use negative result
						MATH_NEG MULTOUT+2, result
						bra end
posresult:				MATH_MOV opB, MULTINB			; a is positive, b is positive - use positive result
						MATH_MOV MULTOUT+2, result
end:
.endscope
.endmacro

.macro MATH_MUL_ANEG opB, result
.scope
						bit opB+3
						bpl negresult
						MATH_NEG opB, MULTINB			; a is negative, b is negative - use positive result
						MATH_MOV MULTOUT+2, result
						bra end
negresult:				MATH_MOV opB, MULTINB			; a is negative, b is positive - use negative result
						MATH_NEG MULTOUT+2, result
end:
.endscope
.endmacro

.macro MATH_MUL_VEC3_MAT3x3	px, py, pz, mat, pox, poy, poz
.scope
pxmult:
						bit px+3
						bmi negpx
						jmp pospx
negpx:					MATH_NEG px, MULTINA
						MATH_MUL_ANEG mat+0*4, matrix4x4_TEMP+0*4
						MATH_MUL_ANEG mat+3*4, matrix4x4_TEMP+1*4
						MATH_MUL_ANEG mat+6*4, matrix4x4_TEMP+2*4
						jmp pymult
pospx:					MATH_MOV px, MULTINA
						MATH_MUL_APOS mat+0*4, matrix4x4_TEMP+0*4
						MATH_MUL_APOS mat+3*4, matrix4x4_TEMP+1*4
						MATH_MUL_APOS mat+6*4, matrix4x4_TEMP+2*4

pymult:
						bit py+3
						bmi negpy
						jmp pospy
negpy:					MATH_NEG py, MULTINA
						MATH_MUL_ANEG mat+1*4, matrix4x4_TEMP+3*4
						MATH_MUL_ANEG mat+4*4, matrix4x4_TEMP+4*4
						MATH_MUL_ANEG mat+7*4, matrix4x4_TEMP+5*4
						jmp pzmult
pospy:					MATH_MOV py, MULTINA
						MATH_MUL_APOS mat+1*4, matrix4x4_TEMP+3*4
						MATH_MUL_APOS mat+4*4, matrix4x4_TEMP+4*4
						MATH_MUL_APOS mat+7*4, matrix4x4_TEMP+5*4

pzmult:
						bit pz+3
						bmi negpz
						jmp pospz
negpz:					MATH_NEG pz, MULTINA
						MATH_MUL_ANEG mat+2*4, matrix4x4_TEMP+6*4
						MATH_MUL_ANEG mat+5*4, matrix4x4_TEMP+7*4
						MATH_MUL_ANEG mat+8*4, matrix4x4_TEMP+8*4
						jmp muldone
pospz:					MATH_MOV pz, MULTINA
						MATH_MUL_APOS mat+2*4, matrix4x4_TEMP+6*4
						MATH_MUL_APOS mat+5*4, matrix4x4_TEMP+7*4
						MATH_MUL_APOS mat+8*4, matrix4x4_TEMP+8*4

muldone:				clc
						ldq  matrix4x4_TEMP+0*4
						adcq matrix4x4_TEMP+3*4
						adcq matrix4x4_TEMP+6*4
						stq pox

						clc
						ldq  matrix4x4_TEMP+1*4
						adcq matrix4x4_TEMP+4*4
						adcq matrix4x4_TEMP+7*4
						stq poy

						clc
						ldq  matrix4x4_TEMP+2*4
						adcq matrix4x4_TEMP+5*4
						adcq matrix4x4_TEMP+8*4
						stq poz
.endscope
.endmacro

.macro MATH_DIV_APOS_BPOS numerator, denominator, result
.scope
						MATH_MOV numerator, MULTINA
						MATH_MOV denominator, MULTINB
						MATH_MOV DIVOUTWHOLE+2, result	; add 2 to get new 16.16 fixed point result
end:
.endscope
.endmacro

.macro MATH_DIV numerator, denominator, result
.scope
						MATH_ABS numerator, MULTINA
						MATH_ABS denominator, MULTINB
						bit numerator+3
						bmi negtivea					; a is negative
						bit denominator+3
						bmi negtiveb					; a is positive, but b is not - use negative result
						bra plus						; a is positive and so is b - use positive result
negtivea:				bit denominator+3
						bmi plus						; a is negative and so is b - use plus result
negtiveb:				MATH_NEG DIVOUTWHOLE+2, result	; add 2 to get new 16.16 fixed point result
						bra end
plus:					MATH_MOV DIVOUTWHOLE+2, result	; add 2 to get new 16.16 fixed point result
end:
.endscope
.endmacro

.macro MATH_DIV_BPOS numerator, denominator, result
.scope
						MATH_ABS numerator, MULTINA
						MATH_MOV denominator, MULTINB
						bit numerator+3
						bmi negtivea					; a is negative and b is positive - use negative result
						bra postivea					; a is positive and so is b - use positive result
negtivea:				MATH_NEG DIVOUTWHOLE+2, result	; add 2 to get new 16.16 fixed point result
						bra end
postivea:				MATH_MOV DIVOUTWHOLE+2, result	; add 2 to get new 16.16 fixed point result
end:
.endscope
.endmacro

.macro MATH_MUL_APOS_BPOS opA, opB, result
.scope
				MATH_MOV opA, MULTINA			; a is positive
				MATH_MOV opB, MULTINB			; a is positive, b is positive - use positive result
				MATH_MOV MULTOUT+2, result		; add 2 to get new 16.16 fixed point result
.endscope
.endmacro

.macro MATH_MUL opA, opB, result
.scope
				bit opA+3
				bpl posa
				MATH_NEG opA, MULTINA			; a is negative
nega:			bit opB+3
				bpl posb1
				MATH_NEG opB, MULTINB			; a is negative, b is negative - use positive result
				bra posresult
posb1:			MATH_MOV opB, MULTINB			; a is negative, b is positive - use negative result
				bra negresult

posa:			MATH_MOV opA, MULTINA			; a is positive
				bit opB+3
				bpl posb2
				MATH_NEG opB, MULTINB			; a is positive, b is negative - use negative result
				bra negresult
posb2:			MATH_MOV opB, MULTINB			; a is positive, b is positive - use positive result
				bra posresult

negresult:		MATH_NEG MULTOUT+2, result		; add 2 to get new 16.16 fixed point result
				bra end
posresult:		MATH_MOV MULTOUT+2, result		; add 2 to get new 16.16 fixed point result
end:
.endscope
.endmacro

.macro MATH_MUL_DIRECT	opA, opB
.scope
						bit opA+3
						bpl posa
						MATH_NEG opA, MULTINA			; a is negative
nega:					bit opB+3
						bpl posb1
						MATH_NEG opB, MULTINB			; a is negative, b is negative - use positive result
						bra posresult
posb1:					MATH_MOV opB, MULTINB			; a is negative, b is positive - use negative result
						bra negresult

posa:					MATH_MOV opA, MULTINA			; a is positive
						bit opB+3
						bpl posb2
						MATH_NEG opB, MULTINB			; a is positive, b is negative - use negative result
						bra negresult
posb2:					MATH_MOV opB, MULTINB			; a is positive, b is positive - use positive result
						bra posresult

negresult:				MATH_NEG_DIRECT MULTOUT+2		; add 2 to get new 16.16 fixed point result
						bra end
posresult:				ldq MULTOUT+2					; add 2 to get new 16.16 fixed point result
end:
.endscope
.endmacro