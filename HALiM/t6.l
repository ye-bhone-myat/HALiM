;; HALiM
;; Task 6: The Die
;; Ye Bhone Myat

;; Load previous task

( load "t5.l" )

;; ==========

;; Declare constants

( setf *die* '( 1 2 2 3 3 4 ) )

;; ==========

;; function roll
;; ==========
;; Takes nothing
;; -----
;; Randomly picks an element from *die*
;; -----
( defun roll ()
	( pick *die* )
)
;; ==========

;; function populate
;; ==========
;; Takes the number of iterations the function
;; should run
;; -----
;; Simulates an APPROXIMATION of what the
;; game board should look like after
;; the players have made a total of
;; iterations moves.
;; The generated board is an approximation
;; in that the legality of moves in each
;; iteration is not checked, and the board
;; could therefore have a configuration
;; that cannot be obtained through normal
;; gameplay.
;; -----
( defun populate ( iterations )
	( cond
		( ( = iterations 0 )
			( format t "Done~%" )
			nil
		)
		( t
			( carpet-place ( pick ( mapcar #'cdr ( tile-neighbors ( assam-tile *assam* ) ) ) ) ( pick ( mapcar #'car *directions* ) ) ( pick ( mapcar #'car *colors* ) ) )
			( assam-face ( pick ( mapcar #'car *directions* ) ) *assam* )
			( assam-move ( roll ) *assam* )
			( populate ( - iterations 1 ) )
		)
	)
)
;; ==========
