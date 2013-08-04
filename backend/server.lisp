(in-package :asterblaster)

(setf clws:*debug-on-server-errors* t)
(setf clws:*debug-on-resource-errors* t)

(defvar *user-id-seq* 0)

(defclass api-resource (ws-resource)
  ())

(defmethod resource-received-text ((res api-resource) client message)
  (format t "[got message: ~S]~%" message)
  (write-to-client-text client message))

(defmethod resource-client-connected ((res api-resource) client)
  (format t "[connection on api server from ~s : ~s]~%"
          (client-host client) (client-port client))
  t)

(defmethod resource-client-disconnected ((resource api-resource) client)
  (format t "[disconnected from resource ~A: ~A]~%" resource client))


(register-global-resource "/api"
                          (make-instance 'api-resource)
                          (origin-prefix nil))

(def class* server-message ()
  ((msg-type "unknown" :type string)
   (data nil)))

(def class* ping-server-message ()
  ())

(defparameter *server-message-names-alist*
  '((ping-server-message . "ping")))

(defun class-to-msg-type (class)
  (assoc-cdr class *server-message-names-alist*))

(defun make-server-message (class &rest args)
  (make-instance 'server-message
                 :msg-type (class-to-msg-type class)
                 :data (apply #'make-instance class args)))


(def class* client-message ()
  ((msg-type "unknown" :type string)
   (data nil)))

(def class* hello-client-message ()
  ((name :type string)))

(def class* rotate-client-message ()
  ((direction)))

(def class* accelerate-client-message ()
  ((throttle)))

(def class* pong-client-message ()
  ())

(defparameter *client-message-classes-alist*
  '(("hello" . hello-client-message)
    ("rotate" . rotate-client-message)
    ("accelerate" . accelerate-client-message)
    ("pong" . pong-client-message)))

(defun json-to-client-message (json)
  (let* ((alist (decode-json-from-string json))
         (msg-type-string (assoc-cdr :msg-type alist))
         (msg-class (assoc-cdr msg-type-string *client-message-classes-alist*
                               :test #'equal)))
    (when (or (not msg-type-string) (not msg-class))
      (error "wrong message type"))
    (make-instance 'client-message :msg-type msg-type-string
                   :data (apply #'make-instance msg-class 
                                (alist-plist (assoc-cdr :data alist))))))



(defparameter *update-state-channel* (make-instance 'unbounded-channel))

(def class* stop-message () ())

(def class* user-join-message ()
  ((name)
   (id (incf *user-id-seq*))))
