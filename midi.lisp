(in-package #:simulacrum)

(midi:initialize)

(defvar *midi-source* (or (midi:find-source "Faderfox UC4") 0))

(midi:set-midi-callback *midi-source* :cc
			(lambda (chan cc val)
			  (declare (ignore chan))
			  (cond
			    ;; encoders
			    ((<= 8 cc 11)
			     (incf-pos (aref *grains* (- cc 8)) (- val 64)))
			    ((= cc 15) ; 8: crossfader tanh
			     (ctrl *generic-ctrl-node* :val (/ val 127)))
			    ;; faders
			    ((<= 32 cc 35)
			     (ctrl (play-node (aref *grains* (- cc 32)))
				   :amp (/ val 127)))
			    ((= cc 37) ; 6: tremelo
			     (if (zerop val)
				 (progn (free *tremelo-node*)
					(setf *tremelo-node* nil))
				 (progn (unless *tremelo-node* (setf *tremelo-node* (synth 'tremelo :to *fx-group*)))
					(ctrl *tremelo-node* :speed (lin-lin val 0 127 0 30)))))
			    ((= cc 112)
			     (if (or (zerop val) (= 127 val))
				 (ping-pong-stop)
				 (progn (unless *ping-pong-nodes* (ping-pong-play))
					(ctrl *crossfade-node* :which (/ val 127))))))))

(midi:set-midi-callback *midi-source* :note-on
			(lambda (chan note vel)
			  (declare (ignore chan vel))
			  (cond
			    ((<= 64 note 67) ; buttons 1-4
			     (start-rec (aref *grains* (- note 64)))))))

(midi:set-midi-callback *midi-source* :note-off
			(lambda (chan note vel)
			  (declare (ignore chan vel))
			  (cond
			    ((<= 64 note 67) ; buttons 1-4
			     (stop-rec (aref *grains* (- note 64)))))))
