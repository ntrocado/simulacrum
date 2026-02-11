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
			    ;; faders
			    ((<= 32 cc 35)
			     (ctrl (play-node (aref *grains* (- cc 32)))
				   :amp (/ val 127))))))

(midi:set-midi-callback *midi-source* :note-on
			(lambda (chan note vel)
			  (declare (ignore chan vel))
			  (cond
			    ((<= 64 note 67)
			     (start-rec (aref *grains* (- note 64)))))))

(midi:set-midi-callback *midi-source* :note-off
			(lambda (chan note vel)
			  (declare (ignore chan vel))
			  (cond
			    ((<= 64 note 67)
			     (stop-rec (aref *grains* (- note 64)))))))
