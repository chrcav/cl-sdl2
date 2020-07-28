(in-package :sdl2)

;; A start to the SDL2 audio interface.

;; These are #defines, but really should have been enums....

(autowrap:define-enum-from-constants (sdl-audio-mask)
  sdl2-ffi:+sdl-audio-mask-bitsize+
  sdl2-ffi:+sdl-audio-mask-datatype+
  sdl2-ffi:+sdl-audio-mask-endian+
  sdl2-ffi:+sdl-audio-mask-signed+)

(autowrap:define-enum-from-constants (sdl-audio)
  sdl2-ffi:+audio-u8+
  sdl2-ffi:+audio-s8+
  sdl2-ffi:+audio-u16lsb+
  sdl2-ffi:+audio-s16lsb+
  sdl2-ffi:+audio-u16msb+
  sdl2-ffi:+audio-s16msb+
  sdl2-ffi:+audio-u16+
  sdl2-ffi:+audio-s16+
  sdl2-ffi:+audio-s32lsb+
  sdl2-ffi:+audio-s32msb+
  sdl2-ffi:+audio-s32+
  sdl2-ffi:+audio-f32lsb+
  sdl2-ffi:+audio-f32msb+
  sdl2-ffi:+audio-f32+
  sdl2-ffi:+audio-u16sys+
  sdl2-ffi:+audio-s16sys+
  sdl2-ffi:+audio-s32sys+
  sdl2-ffi:+audio-f32sys+)

(autowrap:define-bitmask-from-constants (sdl-audio-allow)
  sdl2-ffi:+sdl-audio-allow-frequency-change+
  sdl2-ffi:+sdl-audio-allow-format-change+
  sdl2-ffi:+sdl-audio-allow-channels-change+
  sdl2-ffi:+sdl-audio-allow-any-change+)


(defun audio-bitsize (x)
  (logand x (enum-value 'sdl-audio-mask :bitsize)))

(defun audio-float-p (x)
  (logand x (enum-value 'sdl-audio-mask :datatype)))

(defun audio-big-endian-p (x)
  (logand x (enum-value 'sdl-audio-mask :endian)))

(defun audio-signed-p (x)
  (logand x (enum-value 'sdl-audio-mask :signed)))

(defun audio-int-p (x)
  (not (audio-float-p x)))

(defun audio-little-endian-p (x)
  (not (audio-big-endian-p x)))

(defun audio-unsigned-p (x)
  (not (audio-signed-p x)))

(defun audio-init (driver-name)
  "Use this only if you need a specific audio driver."
  (check-rc (sdl-audio-init (symbol-name driver-name))))

(defun audio-quit ()
  "Use this function to shut down audio if you initialized it with audio-init"
  (sdl-audio-quit))

(defun get-current-audio-driver ()
  (let ((driver (sdl-get-current-audio-driver)))
    (when driver
      (make-keyword (string-upcase driver)))))

(defun audio-drivers ()
  "Returns a list of audio drivers present in system"
  (loop for i below (sdl-get-num-audio-drivers)
	collect (make-keyword (string-upcase (sdl-get-audio-driver i)))))

(defun audio-devices ()
  (loop for i below (sdl-get-num-audio-devices 0)
	collect (sdl-get-audio-device-name i 0)))

(define-struct-accessors (audio-spec sdl2-ffi:sdl-audio-spec)
  :freq :format :channels :samples :callback :userdata)

(defun open-audio-device (freq format channels samples
			  &key
			    (device)
			    (callback)
			    (user-data)
			    (allowed-changes '()))
  "Opens audio device. Specify device if you need a particular one.
Returns device-id and actual values for freq format channels and samples.
allowed-changes should be a list with some combination of :frequency-change :format-change :channels-change :any-change
SDL will convert between requested and actual format on the fly, when changes are not allowed."
  (c-with ((desired sdl2-ffi:sdl-audio-spec)
	   (obtained sdl2-ffi:sdl-audio-spec))
    (setf (audio-spec-freq desired) freq
	  (audio-spec-format desired) (autowrap:enum-value 'sdl-audio format)
	  (audio-spec-channels desired) channels
	  (audio-spec-samples desired) samples
	  (audio-spec-callback desired) (if callback (callback callback)
					    (cffi:null-pointer))
	  (audio-spec-userdata desired) (if user-data user-data
					    (cffi:null-pointer)))
    (let ((device (sdl-open-audio-device
		   (if device device
		       (cffi:null-pointer))
		   0
		   desired
		   obtained
		   (autowrap:mask-apply 'sdl-audio-allow allowed-changes))))
      (check-zero device)
      (values device
	      (audio-spec-freq obtained)
	      (autowrap:enum-key 'sdl-audio (audio-spec-format obtained))
	      (audio-spec-channels obtained)
	      (audio-spec-samples obtained)))))

(defun close-audio-device (device)
  (sdl-close-audio-device device))

(defun pause-audio-device (device)
  (sdl-pause-audio-device device 1))

(defun unpause-audio-device (device)
  (sdl-pause-audio-device device 0))
;; TODO, everything else. :)
