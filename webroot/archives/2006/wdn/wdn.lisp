(load "win32")

(defpackage :wdn
  (:use :common-lisp :ffi)
  (:export :start-main))

(in-package :wdn)

;; Constants
(defconstant +class-name+ "wdn")
(defconstant +window-title+ "Windows Desktop Notes")
(defconstant +window-left+ 20)
(defconstant +window-top+ 20)
(defconstant +window-width+ 500)
(defconstant +window-height+ 500)
(defconstant +settings-filename+ "settings.cfg")
(defconstant +text-filename+ "text.cfg")

;; Strings
(defconstant +settingsfile-not-found+
  ";; File ~A was not found. Default settings will be used.~%")
(defconstant +settingsfile-empty+
  ";; File ~A was empty. Default settings will be used.~%")

(defconstant +text-not-found+
  ";; File ~A was not found. Default text will be used.~%")
(defconstant +text-empty+
  ";; File ~A was empty. Default text will be used.~%")

(defconstant +create-window-fail+
  "Unable to create window.")

;; Globals
(defparameter *moving* nil)
(defparameter *editing* nil)
(defparameter *text* "Hello")
(defparameter *text-length* (length *text*))
(defparameter *font-family* "Verdana")
(defparameter *font-size* 12)
(defparameter *font* nil)
(defparameter *settings* nil)
(defparameter *editing* nil)
(defparameter *edit-hwnd* nil)

(defun save-to-file (filename data)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print data out))))

(defun load-from-file (filename)
  (with-open-file (in filename)
    (when (> (file-length in) 0)
      (with-standard-io-syntax
	(read in)))))

(defun load-settings ()
  (if (probe-file +settings-filename+)
      (unless (setf *settings* (load-from-file +settings-filename+))
	(format t +settingsfile-empty+ +settings-filename+))
      (format t +settingsfile-not-found+ +settings-filename+)))

(defun save-settings ()
  (save-to-file +settings-filename+ *settings*))

(defun load-text ()
  (if (probe-file +text-filename+)
      (unless (setf *text* (load-from-file +text-filename+))
	(format t +text-empty+ +text-filename+))
      (format t +text-not-found+ +text-filename+)))

(defun save-text ()
  (save-to-file +text-filename+ *text*))

(defun create-font (hwnd)
  (let* ((hdc (win32::getdc hwnd))
	 (font-size (win32::muldiv *font-size* (win32::getdevicecaps hdc win32::*LOGPIXELSY*) 72)))
    (setf *font* (win32::createfont font-size 0 0 0 0 0 0 0 0 0 0 0 0 *font-family*))
    (win32::releasedc hwnd hdc)))

(defun create-edit-control (hwnd)
  (setf *edit-hwnd*
	(win32::createwindowex
	 0
	 "EDIT"
	 *text*
	 win32::*WS_CHILD*
	 0 0
	 +window-width+ +window-height+
	 hwnd win32::*NULL* win32::*NULL*  win32::*NULL*)))
 ; (win32::sendmessage *edit-hwnd* win32::*WM_SETFONT* *font* win32::*TRUE*))

(defun get-text ()
  (let* ((len (1+ (win32::getwindowtextlength *edit-hwnd*))))
    (with-foreign-string (s (make-string len))
      (format t "Length got: ~D~%" (win32::getwindowtext-i *edit-hwnd* s len)))))
      ;(format t "String got: \"~A\"~%" s))))

;; Main event handler
(defun window-proc (hwnd umsg wparam lparam)
  (cond
    ((= umsg win32::*WM_CREATE*)
     (create-font hwnd)
     (create-edit-control hwnd)
     0)
    ((= umsg win32::*WM_DESTROY*)
     (win32::postquitmessage 0)
     0)
    ((= umsg win32::*WM_LBUTTONDOWN*)
     (setf *moving* t)
     (win32::sendmessage hwnd win32::*WM_NCLBUTTONDOWN* win32::*HTCAPTION* 0)
     0)
    ((= umsg win32::*WM_LBUTTONDBLCLK*)
     (setf *editing* t)
     (win32::showwindow *edit-hwnd* win32::*SW_SHOW*)
     (win32::setfocus *edit-hwnd*)
     0)
    ((= umsg win32::*WM_MOUSEMOVE*)
     (when *moving*
       (win32::InvalidateRect hwnd win32::*NULL* win32::*FALSE*)
       (setf *moving* nil))
     0)
    ((= umsg win32::*WM_PAINT*)
     (win32::with-begin-paint
	 hwnd (ps hdc)
	 (win32::paintdesktop hdc)
	 (with-foreign-objects ((text-position 'win32::RECT))
	   (win32::getclientrect hwnd text-position)
	   (win32::setbkmode hdc win32::*TRANSPARENT*)
	   (win32::with-selected-object
	       hdc (make-pointer *font* :pointer-void)
	       (win32::drawtext hdc *text* *text-length* text-position
				(logior win32::*DT_TOP* win32::*DT_LEFT*
					win32::*DT_WORDBREAK* win32::*DT_EDITCONTROL*)))))
     0)
    ((= umsg win32::*WM_COMMAND*)
     (if (= (win32::hiword wparam) win32::*EN_KILLFOCUS*)
	 (progn
	   (win32::showwindow *edit-hwnd* win32::*SW_HIDE*)
	   (get-text)
	   ;(save-text)
	   ;(format t "Text: ~A~%" *text*)
	   0)
	 (win32::defwindowproc hwnd umsg wparam lparam)))
    (t
     (win32::defwindowproc hwnd umsg wparam lparam))))

;; Main entry point
(defun start-main ()
  (load-settings)
  (load-text)
  (win32::make-wndclass +class-name+
    :lpfnWndProc #'window-proc)
  (let ((hwnd (win32::createwindowex
	       0
	       +class-name+
	       +window-title+
	       win32::*WS_POPUP*
	       +window-left+ +window-top+
	       +window-width+ +window-height+
	       win32::*NULL* win32::*NULL* win32::*NULL*  win32::*NULL*)))
    (when (si::null-pointer-p hwnd)
      (error +create-window-fail+))
    (win32::showwindow hwnd win32::*SW_SHOW*)
    (win32::updatewindow hwnd)
    (win32::event-loop)
    (win32::unregisterclass +class-name+ win32::*NULL*))
  (save-settings)
  (save-text))
