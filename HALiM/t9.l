( load "t8.l" )

( load-kbs "kb-empty.l" "kb-empty.l" )

( setf *play-modes* '( ( aggressive . aggressive-p ) ( neutral . neutral-p ) ( defensive . defensive-p ) ) )



( defun path-select ( ( p halim ) turnables &aux affiliations path-scores game-phase finances entry selection index )
	( setf affiliations ( mapcar
			( lambda (x)
				( convert-colors-to-affiliations
					p
					( cdr ( look-ahead-colors ( assam-tile *assam* ) x 4 ) )
				)
			)
			( mapcar #'car turnables )
	) )
	( setf path-scores ( mapcar ( lambda (x) ( classify-path-score p x ) ) affiliations ) )
	( if ( and ( phase-change-p p ) ( < ( performance-eval p ) 0 ) )
		( let () 
			( rotate-play-modes ( halim-game-phase p ) ( halim-finances p ) *path-kb* )
		)
	)
	( setf game-phase ( change-phase p ) )
	( setf finances ( change-finances p ) )
	( setf entry ( retrieve-kb game-phase finances *path-kb* ) )
	( setf index ( path-aggression path-scores entry 0 ) )
	( loop while ( null index )
		do ( setf entry ( unique ( cons ( nth ( random ( length *play-modes* ) ) ( mapcar #'car *play-modes* ) ) entry ) ) )
		do ( setf index ( path-aggression path-scores entry 0 ) )
	)
	( setf selection ( nth index ( mapcar #'car turnables ) ) )
	( add-to-entry game-phase finances entry *path-kb* )
	;;( format t "Turning ~:(~A~)~%" ( cdr ( assoc selection *directions* ) ) )
	selection
)

( defun path-aggression ( path-scores aggression-list index &aux mode score )
	( cond
		( ( = index ( length aggression-list ) )
			( if *verbose*
				( format t "No applicable play style found.~% Randomly choosing a play stile...~%" )
			)
			nil
		)
		( ( null aggression-list )
			( if *verbose*
				( format t "No applicable play style found.~% Randomly choosing a play stile...~%" )
			)
			nil
		)
		( ( not ( funcall ( cdr ( assoc ( nth index aggression-list ) *play-modes* ) ) path-scores ) )
			( if *verbose* ( format t "Play style not applicable, trying next play style...~%" ) )
			( path-aggression path-scores aggression-list ( + index 1 ) )
		)
		( t
			( cond
				( ( eq 'aggressive ( nth index aggression-list ) )
					( if *verbose* ( format t "Playing aggressive...~%" ) )
					( position ( eval ( cons 'min path-scores ) ) path-scores )
				)
				( ( eq 'neutral ( nth index aggression-list ) )
					( if *verbose* ( format t "Playing neutral...~%" ) )
					( position ( eval ( cons 'min ( mapcar #'abs path-scores ) ) ) ( mapcar #'abs path-scores ) )
				)
				( ( eq 'defensive ( nth index aggression-list ) )
					( if *verbose* ( format t "Playing defensive...~%" ) )
					( position ( eval ( cons 'max path-scores ) ) path-scores )
				)
			)
		)
	)
)

( defun placement-aggression ( path-scores aggression-list index &aux mode score )
	( cond
		( ( = index ( length aggression-list ) )
			( if *verbose* ( format t "No applicable play style found.~% Randomly choosing a play style...~%" ) )
			nil
		)
		( ( null aggression-list )
			( if *verbose* ( format t "No applicable play style found.~% Randomly choosing a play style...~%" ) )
			nil
		)
		( ( not ( funcall ( cdr ( assoc ( nth index aggression-list ) *play-modes* ) ) path-scores ) )
			( if *verbose* ( format t "Play style not applicable, trying next play style...~%" ) )
			( placement-aggression path-scores aggression-list ( + index 1 ) )
		)
		( t
			( cond
				( ( eq 'aggressive ( nth index aggression-list ) )
					( if *verbose* ( format t "Playing aggressive...~%" ) )
					( position ( eval ( cons 'min path-scores ) ) path-scores )
				)
				( ( eq 'neutral ( nth index aggression-list ) )
					( if *verbose* ( format t "Playing neutral...~%" ) )
					( position ( eval ( cons 'min ( mapcar #'abs path-scores ) ) ) ( mapcar #'abs path-scores ) )
				)
				( ( eq 'defensive ( nth index aggression-list ) )
					( if *verbose* ( format t "Playing defensive...~%" ) )
					( position ( eval ( cons 'min ( filter-in ( lambda (x) ( > x 1 ) ) path-scores ) ) ) ( filter-in ( lambda (x) ( > x 1 ) ) path-scores ) )
				)
			)
		)
	)
)

( defun placement-select ( ( p halim ) directions &aux tile-groups scores game-phase finances index entry )
	( setf tile-groups
		( mapcar #'group-same-color-tiles 
			( mapcar #'get-placeable-tile-group
				directions
			)
		)
	)
	
	( setf scores ( mapcar
		( lambda (x)
			( convert-tile-group-to-score p x )
		)
		tile-groups
	) )
	( if ( and ( phase-change-p p ) ( < ( performance-eval p ) 0 ) )
		( let () 
			( rotate-play-modes ( halim-game-phase p ) ( halim-finances p ) *placement-kb* )
		)
	)
	( setf game-phase ( change-phase p ) )
	( setf finances ( change-finances p ) )
	( setf entry ( retrieve-kb game-phase finances *placement-kb* ) )
	( setf index ( placement-aggression scores entry 0 ) )
	( loop while ( null index )
		do ( setf entry ( unique ( cons ( nth ( random ( length *play-modes* ) ) ( mapcar #'car *play-modes* ) ) entry ) ) )
		do ( setf index ( placement-aggression scores entry 0 ) )
	)
	( add-to-entry game-phase finances entry *placement-kb* )
	index
)

( defun orient-color-select ( ( p halim ) target-tile &aux orients scores selection )
	( setf orients ( filter-out 'null ( carpet-orientables target-tile ) ) )
	( setf scores ( mapcar
		( lambda ( x )
			( mapcar 
				( lambda (y)
					( test-add-carpet p target-tile x y )
				)
				( mapcar #'car ( available-carpets p ) )
			)
		)
		( mapcar #'cdr orients )
	) )
	( setf selection ( orient-color-choose-best scores ) )
	( list ( car ( nth ( car selection ) orients ) ) ( car ( nth ( cadr selection ) ( player-carpets p ) ) ) )
)

( defun test-add-carpet ( ( p halim ) ( t1 tile ) ( t2 tile ) color &aux tiles originals final initial )
	( setf initial ( count-contiguous-carpets t2 ( tile-carpet-color t2 ) ) )
	( setf initial ( * ( car ( convert-colors-to-affiliations p ( list t2 ) ) ) initial ) )
	( setf tiles ( list t1 t2 ) )
	( setf originals ( mapcar #'tile-carpet-color tiles ) )
	( mapcar ( lambda (x) ( setf ( tile-carpet-color x ) color ) ) tiles )
	( setf final ( count-contiguous-carpets t2 ( tile-carpet-color t2 ) ) )
	( setf final ( * ( car ( convert-colors-to-affiliations p ( list t2 ) ) ) final ) )
	( setf ( tile-carpet-color t1 ) ( nth 0 originals ) ) ( setf ( tile-carpet-color t2 ) ( nth 1 originals ) )
	( - final initial )
)

( defun orient-color-choose-best ( scores &aux 1st-color 2nd-color 1st-max 2nd-max abs-max )
	( setf 1st-color ( filter-out #'null ( mapcar #'car scores ) ) )
	( setf 2nd-color ( filter-out #'null ( mapcar #'cadr scores ) ) )
	( setf 1st-max ( eval ( cons 'max 1st-color ) ) )
	( cond
		( 2nd-color
			( setf 2nd-max ( eval ( cons 'max 2nd-color ) ) )
			( cond
				( ( > 1st-max 2nd-max )
					( list ( position 1st-max 1st-color ) 0 )
				)
				( t
					( list ( position 2nd-max 2nd-color ) 1 )
				)
			)
		)
		( t
			( list ( position 1st-max 1st-color ) 0 )
		)
	)
	
)

( defun convert-tile-group-to-score ( ( p halim ) tile-group &aux score-group )
	( setf score-group ( list 
		( convert-colors-to-affiliations p ( car tile-group ) )
		( if ( cdr tile-group )
			( convert-colors-to-affiliations p ( cdr tile-group ) )
		)
	) )
	( sum
		( list 
			( * ( length ( car score-group ) ) ( sum ( car score-group ) ) )
			( sum ( cadr score-group ) )
		)
	)
)

( defun convert-tile-group-to-colors ( tile-group &aux color-group )
	( mapcar #'tile-carpet-color tile-group )
)

( defun aggressive-p ( scores )
	( > -1 ( eval ( cons 'min scores ) ) )
)

( defun neutral-p ( scores )
	( > 3 ( eval ( cons 'min ( mapcar #'abs scores ) ) ) )
)

( defun defensive-p ( scores )
	( < 1 ( eval ( cons 'max scores ) ) )
)

( defun retrieve-kb ( x y kb )
	( nth y ( nth x kb ) )
)

( defun add-to-entry ( game-phase finances state kb )
	( setf ( nth finances ( nth game-phase kb ) ) state )
)

( defun rotate-play-modes ( game-phase finances tmp-kb &aux new-rank entry )
	( if *verbose* ( format t "Rotated play styles at ~A, ~A~%" game-phase finances ) )
	( setf entry ( retrieve-kb game-phase finances tmp-kb ) )
	( setf new-rank 
		( if entry
			( snoc ( car entry ) ( cdr entry ) )
			nil
		)
	)
	( setf ( nth finances ( nth game-phase tmp-kb ) ) new-rank )
)

( defun performance-eval ( ( p halim ) )
	( if ( finances-change-p p )
		( cond
			( ( > ( finances p ) ( halim-finances p ) )
				1
			)
			( ( < ( finances p ) ( halim-finances p ) )
				-1
			)
			( t
				0
			)
		)
		0
	)
)
