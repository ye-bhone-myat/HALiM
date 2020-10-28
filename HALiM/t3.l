;; HALiM
;; Task 3: Hi Assam
;; Ye Bhone Myat

;; Load previous task

( load "t2.l" )

;; ==========

;; Declare constants

( open-for-business )

;; ==========

;; === Assam class ===
( defclass assam ()
	(
		( current-tile
			:accessor assam-tile
			:initarg :tile
		)
		( current-heading
			:accessor assam-heading
			:initform ( nth ( random 4 ) *directions* )
		)
	)
)
;; ==========

;; method assam-move
;; ==========
;; Takes the number of tiles to move, and
;; an assam object
;; -----
;; Moves assam by the given number of tiles
;; in the direction he is currently facing
;; -----
( defmethod assam-move ( moves ( a assam ) &aux new-tile-pairlis )
	( setf new-tile-pairlis ( project ( assam-tile a ) ( car ( assam-heading a ) ) moves ) )
	( setf ( assam-tile a ) ( cdr new-tile-pairlis ) )
	( setf ( assam-heading a ) ( assoc ( car new-tile-pairlis ) *directions* ) )
	;;( display )
)
;; ==========

;; method assam-face
;; ==========
;; Takes the direction to face, and
;; an assam object
;; -----
;; Changes Assam's current-heading to the given
;; direction
;; -----
( defmethod assam-face ( direction ( a assam ) )
	( if ( assoc direction *directions* )
		( let ()
			( setf ( assam-heading a ) ( assoc direction *directions* ) )
			;;( display )
		)
		( format t "~A is not a recognized direction. Try one of ~A.~%" direction ( mapcar #'car *directions* ) )
	)
)
;; ==========

;; function hi-assam
;; ==========
;; Takes nothing.
;; -----
;; Creates an assam object with a random heading
;; at tile ( 4, 4 ), and binds it to the
;; symbol *assam*.
;; -----
( defun hi-assam ()
	( setf *assam* ( make-instance 'assam :tile ( get-tile 4 4 *market-square* ) ) )
)
