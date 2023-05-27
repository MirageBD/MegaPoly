.define MULTINA			$d770
.define MULTINB			$d774

.define MULTOUT			$d778

.define DIVOUTWHOLE		$d76c
.define DIVOUTFRACT		$d768

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

.macro MATH_BUILD_ROTMAT
.scope
		MATH_MUL sz, sx, t1
		MATH_MUL sz, cx, t2
		MATH_MUL sz, sy, t3
		MATH_MUL sz, cy, m32

		MATH_MUL cz, cy, m33
		MATH_MUL cz, sx, t4
		MATH_MUL cz, cx, t5

		MATH_NEG sy, m31

		MATH_MUL cz, sy, t6

		MATH_MUL sx, cy, m21

		MATH_MUL sx, t3, m22
		MATH_ADD m22, t5, m22

		MATH_MUL sx, t6, m23
		MATH_SUB m23, t2, m23

		MATH_MUL cx, cy, m11

		MATH_MUL cx, t3, m12
		MATH_SUB m12, t4, m12

		MATH_MUL cx, t6, m13
		MATH_ADD m13, t1, m13
.endscope
.endmacro

.macro MATH_DOT px, m1, py, m2, pz, m3, tmp1, tmp2, tmp3, output
		MATH_MUL px, m1, tmp1
		MATH_MUL py, m2, tmp2
		MATH_MUL pz, m3, tmp3

		clc
		ldq tmp1
		adcq tmp2
		adcq tmp3
		stq output
.endmacro

.macro MATH_MUL_VEC_MAT
		;p2.x = p.x * mXYZ.m11 + p.y * mXYZ.m12 + p.z * mXYZ.m13;
        ;p2.y = p.x * mXYZ.m21 + p.y * mXYZ.m22 + p.z * mXYZ.m23;
        ;p2.z = p.x * mXYZ.m31 + p.y * mXYZ.m32 + p.z * mXYZ.m33;

		; optimize this? sx * m11, * m21, * m31, etc.

		MATH_DOT sx, m11, sy, m12, sz, m13, t1, t2, t3, fx
		MATH_DOT sx, m21, sy, m22, sz, m23, t1, t2, t3, fy
		MATH_DOT sx, m31, sy, m32, sz, m33, t1, t2, t3, fz
.endmacro

.macro MATH_DIV numerator, denominator, result
.scope
		MATH_ABS numerator, MULTINA
		MATH_ABS denominator, MULTINB

		lda	$d020
		sta	$d020
		lda	$d020
		sta	$d020
		lda	$d020
		sta	$d020
		lda	$d020
		sta	$d020

		lda DIVOUTFRACT+2
		sta FP_A+0
		lda DIVOUTFRACT+3
		sta FP_A+1
		lda DIVOUTWHOLE+0
		sta FP_A+2
		lda DIVOUTWHOLE+1
		sta FP_A+3

        bit numerator+3
        bmi negtive						; a is not negative
        bit denominator+3
        bmi nnegtive					; a is negative, but b is not, use negative result
        bra plus						; a is negative and b also. use result as is
negtive
		bit denominator+3
		bmi plus						; b is also not negative. use result as is
nnegtive
		MATH_NEG FP_A, result
		bra end
plus
		MATH_MOV FP_A, result
end
.endscope
.endmacro

.macro MATH_MUL opA, opB, result
.scope
		MATH_ABS opA, MULTINA
		MATH_ABS opB, MULTINB

        bit opA+3
        bmi negtive						; a is not negative
        bit opB+3
        bmi nnegtive					; a is negative, but b is not, use negative result
        bra plus						; a is negative and b also. use result as is
negtive
		bit opB+3
		bmi plus						; b is also not negative. use result as is
nnegtive
		MATH_NEG MULTOUT+2, result		; add 2 to get new 16.16 fixed point result
		bra end
plus
		MATH_MOV MULTOUT+2, result		; add 2 to get new 16.16 fixed point result
end
.endscope
.endmacro
