(in-package :asterblaster)

(setf clws:*debug-on-server-errors* t)
(setf clws:*debug-on-resource-errors* t)

(defvar *user-id-seq* 0)

(defparameter *client-db-lock* (make-lock "client db lock"))

(defparameter *waiting-clients* (make-hash-table))

(defparameter *connected-clients* (make-hash-table))

(defun get-client-by-id (id)
  (with-lock-held (*client-db-lock*)
    (gethash id *connected-clients*)))

(defparameter *client-to-id* (make-hash-table :test 'equal))

(defun client-address (client)
  (format nil "~A:~D" 
          (client-host client)
          (client-port client)))

(defun client-id (client)
  (let ((addr (client-address client)))
    (with-lock-held (*client-db-lock*)
      (gethash addr *client-to-id*))))

(defclass api-resource (ws-resource)
  ())

(defmethod resource-received-text ((res api-resource) client json)
  (format t "[got message: ~S]~%" json)
  (let ((message (json-to-client-message json))
        (id (client-id client)))
    (case (type-of message)
      (hello-client-message 
       (unless id 
         (format t "id: ~A???~%" id)
         (return-from resource-received-text))
       (with-lock-held (*client-db-lock*)
         (remhash id *waiting-clients*)
         (setf (gethash id *connected-clients*) client))
       (send-state-update 'user-join-message 
                          :name (name-of message)
                          :id id)
       (send-to-client client (make-server-message 'hello-reply-message
                                                   :id id)))
      (accelerate-client-message 
       (send-state-update 'user-accelerate-message
                          :id id
                          :status (status-of message)))
      (rotate-client-message
       (send-state-update 'user-rotate-message
                           :id id
                           :status (status-of message)
                           :direction (direction-of message)))
      (shot-client-message
       (send-state-update 'user-shot-message
                          :id id
                          :status (status-of message))))))

(defmethod resource-client-connected ((res api-resource) client)
  (format t "[connection on api server from ~s : ~s]~%"
          (client-host client) (client-port client))
  (let ((new-id (incf *user-id-seq*)))
    (with-lock-held (*client-db-lock*)
      (setf (gethash new-id *waiting-clients*) client)
      (setf (gethash (client-address client) *client-to-id*)
            new-id))))

(defmethod resource-client-disconnected ((resource api-resource) client)
  (format t "[disconnected from resource ~A: ~A]~%" resource client)
  (let ((id (client-id client)))
    (unless id
      (return-from resource-client-disconnected))
    (with-lock-held (*client-db-lock*)
      (if (gethash id *connected-clients*)
          (remhash id *connected-clients*)
          (remhash id *waiting-clients*))
      (remhash (client-address client) *client-to-id*))
    (send-state-update 'user-leave-message
                       :id id)))


(register-global-resource "/api"
                          (make-instance 'api-resource)
                          (origin-prefix nil))

(def class* server-message ()
  ((msg-type "unknown" :type string)
   (data nil)))

(def class* hello-reply-message ()
  ((id)))

(def class* ping-server-message ()
  ())

(def class* state-server-message ()
  ((players)
   (asteroids)
   (projectiles)
   (ufo)
   (collisions)))

(defparameter *server-message-names-alist*
  '((ping-server-message . "ping")
    (hello-reply-message . "helloReply")
    (state-server-message . "state")))

(defun class-to-msg-type (class)
  (assoc-cdr class *server-message-names-alist*))

(defun make-server-message (class &rest args)
  (make-instance 'server-message
                 :msg-type (class-to-msg-type class)
                 :data (apply #'make-instance class args)))

(defun send-to-client (client message)
  (write-to-client-text client (encode-json-to-string message)))

(def class* hello-client-message ()
  ((name :type string)))

(def class* rotate-client-message ()
  ((status)
   (direction)))

(def class* accelerate-client-message ()
  ((status)))

(def class* shot-client-message ()
  ((status)))


(def class* pong-client-message ()
  ())

(defparameter *client-message-classes-alist*
  '(("hello" . hello-client-message)
    ("rotate" . rotate-client-message)
    ("accelerate" . accelerate-client-message)
    ("shot" . shot-client-message)
    ("pong" . pong-client-message)))

(defun json-to-client-message (json)
  (let* ((alist (decode-json-from-string json))
         (msg-type-string (assoc-cdr :msg-type alist))
         (msg-class (assoc-cdr msg-type-string *client-message-classes-alist*
                               :test #'equal)))
    (when (or (not msg-type-string) (not msg-class))
      (error "wrong message type"))
    (apply #'make-instance msg-class 
           (alist-plist (assoc-cdr :data alist)))))



(defparameter *update-state-channel* (make-instance 'unbounded-channel))

(def class* stop-message () ())

(def class* user-join-message ()
  ((name)
   (id)))

(def class* user-leave-message ()
  ((id)))

(def class* user-accelerate-message ()
  ((id)
   (status)))

(def class* user-rotate-message ()
  ((id)
   (status)
   (direction)))

(def class* user-shot-message ()
  ((id)
   (status)))

(defun send-state-update (class &rest args)
  (send *update-state-channel* 
        (apply #'make-instance class args)))
