(in-package #:simulacrum)

(setf *s*
      (make-external-server
       "localhost"
       :server-options (make-server-options :num-input-bus 10
					    :num-output-bus 10
					    :realtime-mem-size 131072
					    :block-size 64
					    :hardware-samplerate 44100
					    :device "Babyface (23509327)")
       :port 4444
       :just-connect-p nil))

(defun start-sc ()
  (unless (boot-p *s*)
    (loop :initially (server-boot *s*)
	  :with start := (get-universal-time)
	  :do (sleep 1)
	  :until (or (boot-p *s*)
		     ;; 10 second timeout
		     (> (get-universal-time) (+ start 10))))))

(start-sc)
