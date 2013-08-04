(in-package :asterblaster)

(defvar *server-thread*)
(defvar *resource-listener-thread*)
(defvar *update-game-state-thread*)
(defvar *send-state-update-thread*)

(defun start-asterblaster-server ()
  (setf *connected-clients* (make-hash-table))
  (setf *client-to-id* (make-hash-table :test 'equal))
  (setf *waiting-clients* (make-hash-table))
  (setf *global-game-state* (make-instance 'game-state))
  (setf *server-thread* (bordeaux-threads:make-thread 
                         (lambda ()
                           (run-server 13373))
                         :name "Asterblaster"))
  (setf *resource-listener-thread* (bordeaux-threads:make-thread
                                    (lambda ()
                                      (run-resource-listener
                                       (find-global-resource "/api")))
                                    :name "resource listener for /api"))
  (setf *update-game-state-thread* (bordeaux-threads:make-thread 
                                    #'update-game-state
                                    :name "game state updater"))  
  (setf *send-state-update-thread* (bordeaux-threads:make-thread 
                                    #'send-state-to-clients
                                    :name "client state notifier")))

(defun stop-asterblaster-server ()
  (sb-thread:destroy-thread *server-thread*)
  (sb-thread:destroy-thread *resource-listener-thread*)
  (sb-thread:destroy-thread *update-game-state-thread*)
  (sb-thread:destroy-thread *send-state-update-thread*))

(defun restart-asterblaster-server ()
  (stop-asterblaster-server)
  (start-asterblaster-server))
