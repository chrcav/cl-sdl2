(in-package :sdl2-examples)

(require :sdl2)

(defun note-to-freq (note &optional (octave 4))
  (* (expt 2 octave)
     (case note
       ((:b# :c) 16.352f0)
       ((:c# :db) 17.324f0)
       (:d 18.354f0)
       ((:d# :eb) 19.445f0)
       ((:e :fb) 20.602f0)
       ((:e# :f) 21.827f0)
       ((:f# :gb) 23.125f0)
       (:g 24.500f0)
       ((:g# :ab) 25.957f0)
       (:a 27.50f0)
       ((:a# :bb) 29.135f0)
       ((:b :cb) 30.868f0)
       (t 0f0))))

(defun make-osc (duration frequency &optional (phase (asin -1)))
  (let ((data (make-array (truncate (* duration 44100)) :element-type 'single-float)))
    (loop for i below (length data)
          with step = (* frequency (/ (* 2f0 (float pi 1f0)) 44100f0))
	  for x = (+ (* i step) phase)
          for v = (/ (1+ (sin x)) 2)
	  do (setf (aref data i) v)
	  finally (return (values data x)))))

(defun queue-notes (device notes-and-durations)
  (loop for l in notes-and-durations
        with last-v = 0
        do (destructuring-bind (note duration octave) l
             (multiple-value-bind (data phase)
		 (make-osc duration (note-to-freq note octave) last-v)
               (sdl2:queue-audio device data)
               (setf last-v phase)))))

(defun audio-queue-example ()
  (sdl2:with-init (:audio)
    (let ((device (sdl2::open-audio-device 44100 :f32 1 1024)))
      (sdl2::unpause-audio-device device)
      (queue-notes device
                   '((:a 0.25 4)
                     (:c 0.25 5)
                     (:d 0.25 5)

                     (nil 0.25 4)

                     (:a 0.25 4)
                     (:c 0.25 5)
                     (:d# 0.125 5)
                     (:d 0.25 5)

                     (nil 0.125 4)

                     (:a 0.25 4)
                     (:c 0.25 5)
                     (:d 0.25 5)

                     (nil 0.125 4)

                     (:c 0.25 5)
                     (:a 0.5 4)))
      (loop while (not (zerop (sdl2:get-queued-audio-size device))))
      (sdl2::close-audio-device device))))
