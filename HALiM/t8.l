;; HALiM
;; Task 8: Game Signature Representation
;; Ye Bhone Myat

;; Load previous task
( load "t7.l" )
;; ==========


( defun load-file ( path )
	( with-open-file ( s path :direction :input )
		( let ( ( *read-eval* nil ) )
			( read s )
		)
	)
)

( defun load-kbs ( path-kb placement-kb )
	( setf *path-kb* ( load-file path-kb ) )
	( setf *placement-kb* ( load-file placement-kb ) )
)


( defun look-ahead-colors ( ( tl tile ) direction n &aux neighbor-pairlis )
	( cond
		( ( = n 0 ) 
			( cons tl () )
		)
		( t
			( setf neighbor-pairlis ( get-neighbor-pairlis tl direction ) )
			( cons tl ( look-ahead-colors ( cdr neighbor-pairlis ) ( car neighbor-pairlis ) ( - n 1 ) ) )
		)
	)
)

( defun convert-colors-to-affiliations ( ( p halim ) look-ahead-list &aux affiliation )
	( cond
		( ( null look-ahead-list )
			()
		)
		( t
			( setf affiliation
				( cond
					( ( null ( tile-carpet-color ( car look-ahead-list ) ) )
						0
					)
					( ( assoc ( tile-carpet-color ( car look-ahead-list ) ) ( player-carpets p ) )
						1
					)
					( t
						-1
					)
				)
			)
			( cons affiliation ( convert-colors-to-affiliations p ( cdr look-ahead-list ) ) )
		)
	)
)

( defun get-placeable-tile-group ( ( target tile ) &aux placeables)
	( if ( setf placeables ( mapcar #'cdr ( filter-out 'null ( carpet-placeables target ) ) ) )
		( cons `( ,target ) placeables )
		nil
	)
)

( defun group-same-color-tiles ( tiles &aux orient )
	( cond
		( ( = ( length tiles ) 1 )
			tiles
		)
		( ( setf orient
				( position-if 
					( lambda ( x ) ( eq ( tile-carpet-color ( caar tiles ) ) ( tile-carpet-color x ) ) )
					( cdr tiles )
				)
		  )
			( setf ( car tiles ) ( snoc ( nth orient ( cdr tiles ) ) ( car tiles ) ) )
			( group-same-color-tiles ( remove-nth ( + 1 orient ) tiles ) )
		)
		( t
			tiles
		)
	)
)

( defun classify-path-score ( ( p halim ) affiliations ) 
	( setf ( nth 1 affiliations ) ( * 2 ( nth 1 affiliations ) ) )
	( setf ( nth 2 affiliations ) ( * 2 ( nth 2 affiliations ) ) )
	( sum affiliations )
)

( defun classify-path-state ( affiliations &aux state )
	( setf state ( list ( classify-path-pair-states 0 3 affiliations ) ( classify-path-pair-states 1 2 affiliations ) ) )
)

( defun classify-path-pair-states ( elem1 elem2 affiliations )
	( cond
		( ( and ( = 0 ( nth elem1 affiliations ) ) ( = 0 ( nth elem2 affiliations ) ) )
			'E
		)
		( ( = 0 ( + ( nth elem1 affiliations ) ( nth elem2 affiliations ) ) )
			'f1h1
		)
		( ( = 1 ( + ( nth elem1 affiliations ) ( nth elem2 affiliations ) ) )
			'f1
		)
		( ( = -1 ( + ( nth elem1 affiliations ) ( nth elem2 affiliations ) ) )
			'h1
		)
		( ( = 2 ( + ( nth elem1 affiliations ) ( nth elem2 affiliations ) ) )
			'f2
		)
		( ( = -2 ( + ( nth elem1 affiliations ) ( nth elem2 affiliations ) ) )
			'h2
		)
	)
)

( defun game-phase ( ( p player ) )
	( cond
		( ( > ( sum ( mapcar #'cdr ( player-carpets p ) ) ) 16 )
			0
		)
		( ( > ( sum ( mapcar #'cdr ( player-carpets p ) ) ) 8 )
			1
		)
		( t
			2
		)
	)
)

( defun phase-change-p ( ( p halim ) )
	( not ( = ( game-phase p ) ( halim-game-phase p ) ) )
)

( defun finances-change-p ( ( p halim ) )
	( not ( = ( finances p ) ( halim-finances p ) ) )
)

( defun change-phase ( ( p halim ) ) 
	( if ( phase-change-p p )
		( setf ( halim-game-phase p ) ( game-phase p ) )
		( halim-game-phase p )
	)
)

( defun change-finances ( ( p halim ) ) 
	( if ( finances-change-p p )
		( setf ( halim-finances p ) ( finances p ) )
		( halim-finances p )
	)
)

( defun finances ( ( p halim ) )
	( cond
		( ( > ( player-coins p ) 20 )
			2
		)
		( ( > ( player-coins p ) 10 )
			1
		)
		( t
			0
		)
	)
)
