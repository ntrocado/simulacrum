;;;; simulacrum.lisp

(in-package #:simulacrum)

(defparameter *output-bus* 0)
(defparameter *always-on* (make-group :pos :head))

(defsynth record (buffer)
  (record-buf.ar (sound-in.ar 0) buffer))

(defsynth grains (buffer (rate 32) (pos .5) (amp 0) (gate 1))
  (let* ((t-rate #+mouse (mouse-y.kr 8 120 :exp)
		 #-mouse rate)
	 (dur (/ 12 t-rate))
	 (clk (impulse.kr t-rate))
	 (position (+ (* (buf-dur.kr buffer) pos)
		      (t-rand.kr 0 0.01 clk)))
	 (pan (lf-noise1.kr 5 2 -1)))
    (out.ar *output-bus* (* (tgrains.ar 2 clk buffer 1 position dur pan 0.5)
			    (env-gen.ar (asr 1) :gate gate :act :free)
			    amp))))

(defclass grain ()
  ((buf
    :initform (buffer-alloc (* 4 (sr *s*)))
    :accessor buf)
   (rec-node
    :accessor rec-node)
   (play-node
    :accessor play-node)
   (encoder-val
    :initform .5
    :accessor encoder-val)))

(defmethod initialize-instance :after ((new-obj grain) &key)
  (setf (play-node new-obj) (synth 'grains :buffer (buf new-obj))))

(defmethod start-rec ((grain grain))
  (with-accessors ((buf buf) (rec-node rec-node))
      grain
    (setf rec-node (synth 'record :buffer buf))))

(defmethod stop-rec ((grain grain))
  (free (rec-node grain)))

(defmethod incf-pos ((grain grain) (delta integer))
  (with-accessors ((play-node play-node) (encoder-val encoder-val))
      grain
    (ctrl play-node :pos (setf encoder-val
			       (mod (+ encoder-val (/ delta 64))
				    1)))))

(defparameter *grains*
  (make-array 4 :initial-contents (loop :repeat 4
					:collect (make-instance 'grain))))


;;;;


(defsynth onset-detect ()
  (send-trig.kr (coyote.kr (sound-in.ar 0)) 0))

(defparameter *onset-detect-node* (synth 'onset-detect :to *always-on*))

(defsynth auto-rec (buffer)
  (record-buf.ar (sound-in.ar 0) buffer :loop 0 :act :free))

(defparameter *auto-rec-bufs*
  (make-array 20 :initial-contents (loop :repeat 20
					 :collect (buffer-alloc (* (+ .125 (random .95))
								   (sr *s*))))))

(let ((cur 0))
  (defun next-auto-rec-buf ()
    (aref *auto-rec-bufs* (setf cur (mod (1+ cur) 20)))))

(add-reply-responder "/tr" (lambda (in id val)
			     (declare (ignore in val))
			     (when (zerop id)
			       (synth 'auto-rec :buffer (next-auto-rec-buf)))))

(defsynth ping-pong ((out 0)
		     (buf 0)
		     (rate 1)           ;; 1 = Normal speed
		     (start-pos 0)    ;; 0 to 1
		     (end-pos 1)      ;; 0 to 1
		     (gate 1)
		     (amp .5))
  (let* ((total-frames (buf-frames.ir buf))
         (start-frame (* start-pos total-frames))
         (end-frame (* end-pos total-frames))
         
         ;; Calculate the length of the segment in frames
         (segment-len (abs (- end-frame start-frame)))
         
         ;; --- The Pointer Logic ---
         ;; Run Phasor from 0 to (2 * length)
         (raw-phase (phasor.ar 0 
                               (* rate (buf-rate-scale.kr buf))
                               0 
                               (* segment-len 2)))
         
         ;; Fold it to bounce: 0->Len->0
         ;; Add start-frame to offset it to the correct part of the buffer
         (pointer (+ start-frame (fold raw-phase 0 segment-len)))

         ;; --- The Window Logic ---
	 (norm-pos (/ (- pointer start-frame) segment-len))
         
         ;; 2. Basic Sine Shape (0 -> 1 -> 0)
         (sine-shape (sin (* norm-pos pi)))
         
         ;; 3. Sharpen it!
         ;; If 'curve' is 0.1, the sine wave becomes almost a square wave
         ;; but still touches exactly 0.0 at the edges.
         (window (expt sine-shape .5))

         ;; --- Sound Generation ---
         (sig (tanh (* 30 (buf-rd.ar 1 buf pointer 1 4))))
	 (env (env-gen.ar (asr .005 1 .15) :gate gate :act :free)))

    (out.ar out (* sig window env amp))))

(defparameter *crossfade-buses* (list (bus-audio) (bus-audio)))

(defsynth crossfade (which)
  (out.ar 0 (pan2.ar (select-x-focus.ar which (mapcar #'in.ar *crossfade-buses*) 1.5)
		     0)))

(defparameter *crossfade-node* (synth 'crossfade :which 0 :pos :tail :to *always-on*))

(defparameter *ping-pong-nodes* nil)

(defsynth foo (out) (out.ar out (sin-osc.ar 220 0 .2)))

(let ((cur-buf -2))
  (defun ping-pong-play ()
    (print cur-buf)
    (setf cur-buf (mod (+ cur-buf 2) (length *auto-rec-bufs*))
	  *ping-pong-nodes* (list (synth 'ping-pong :buf (aref *auto-rec-bufs* cur-buf)
						    :out (first *crossfade-buses*)
						    :pos :before :to *crossfade-node*)
				  (synth 'ping-pong :buf (aref *auto-rec-bufs* (1+ cur-buf))
						    :out (second *crossfade-buses*)
						    :pos :before :to *crossfade-node*)))))

(defun ping-pong-stop ()
  (mapc (lambda (node) (ctrl node :gate 0)) *ping-pong-nodes*)
  (setf *ping-pong-nodes* nil))
