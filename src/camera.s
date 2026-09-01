; .segment "CAMERA"

; ----------------------------------------------------------------------------------------------------

setuprotationmatrix:

			ldq q0										; calculate distance of camera
			stq qdistance

			ldx frame
			lda sin8,x
			lsr ; put into 0-128 range
			sta qdistance+2
			; divide by 32
			MATH_DIV qdistance, q32, qdistance
			clc
			lda qdistance+2
			adc #$04
			sta qdistance+2

			lda frame
			sta angle+0
			lda #$00
			sta angle+1

			; multiply by 4 because sin/cos values are stored as 16.16 fixed point
			asl angle+0
			rol angle+1
			asl angle+0
			rol angle+1

			; feed angle in as value for sinx, siny, sinz

			clc
			lda angle+0
			adc #<sin32
			sta foo1+3		; +3 to skip LDQ (= NEG NEG LDA)
			lda angle+1
			adc #>sin32
			sta foo1+4

			; feed angle in as value for sinx, siny, sinz

			clc
			lda angle+0
			adc #<cos32
			sta foo2+3		; +3 to skip LDQ (= NEG NEG LDA)
			lda angle+1
			adc #>cos32
			sta foo2+4

foo1		ldq sin32
			stq sx
			stq sy
			;ldq q0
			stq sz
foo2		ldq cos32
			stq cx
			stq cy
			;ldq q1
			stq cz

            rts

; ----------------------------------------------------------------------------------------------------

buildrotationmatrix:

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

		rts

; ----------------------------------------------------------------------------------------------------