;;;; simulacrum.lisp

(in-package #:simulacrum)

(defparameter *output-bus* 0)

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
