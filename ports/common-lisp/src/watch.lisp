(in-package :sprite-tool)

;;; ============================================================
;;; sprite-tool watch: Poll beads task for progress
;;; ============================================================

(defun watch-usage ()
  "Print usage for the watch subcommand and exit."
  (format t "Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]~%~%")
  (format t "Arguments:~%")
  (format t "  sprite-name     Name of the sprite to watch~%")
  (format t "  task-id         Beads task ID to track (default: auto-detect first open critical task)~%")
  (format t "  poll-interval   Seconds between polls (default: 30)~%~%")
  (format t "Examples:~%")
  (format t "  sprite-tool watch ember-red-hawk~%")
  (format t "  sprite-tool watch ember-red-hawk CRM-1~%")
  (format t "  sprite-tool watch ember-red-hawk CRM-1 60~%")
  (sb-ext:exit :code 1))

(defun watch-sx (sprite-name cmd)
  "Run a command inside a sprite for watch, capturing output. Suppresses stderr."
  (let ((output (handler-case
                    (uiop:run-program
                     (list "sprite" "exec" "-s" sprite-name "bash" "-c" cmd)
                     :output '(:string :stripped t)
                     :error-output nil
                     :ignore-error-status t)
                  (error () ""))))
    (if (stringp output)
        (string-trim '(#\Space #\Tab #\Newline #\Return) output)
        "")))

(defun watch-sx-print (sprite-name cmd)
  "Run a command inside a sprite, printing output directly. Returns the output."
  (let ((output (watch-sx sprite-name cmd)))
    (when (plusp (length output))
      (format t "~A~%" output))
    output))

(defun current-time-string ()
  "Return current time as HH:MM:SS."
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))

(defun run-watch (args)
  "Execute the watch subcommand."
  (when (< (length args) 1)
    (watch-usage))

  (let* ((sprite-name (first args))
         (task-id (if (>= (length args) 2) (second args) ""))
         (poll-interval 30))

    ;; Parse poll interval if provided
    (when (>= (length args) 3)
      (handler-case
          (setf poll-interval (parse-integer (third args)))
        (error ()
          (format *error-output* "Error: invalid poll-interval '~A' (must be integer)~%"
                  (third args))
          (sb-ext:exit :code 1))))

    ;; Auto-detect tracker task if not specified
    (when (zerop (length task-id))
      (format t "Detecting tracker task...~%")
      (setf task-id
            (watch-sx sprite-name
                      "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'"))
      (when (zerop (length task-id))
        (format t "No critical task found. Falling back to first open task...~%")
        (setf task-id
              (watch-sx sprite-name
                        "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'")))
      (when (zerop (length task-id))
        (format t "ERROR: No beads tasks found on sprite '~A'~%" sprite-name)
        (format t "Specify a task ID manually: sprite-tool watch ~A <task-id>~%" sprite-name)
        (sb-ext:exit :code 1))
      (format t "Tracking task: ~A~%" task-id))

    (format t "Watching sprite '~A' task '~A' (every ~Ds)~%" sprite-name task-id poll-interval)
    (format t "Press Ctrl+C to stop~%")
    (format t "~%")

    ;; Main polling loop
    (handler-case
        (loop
          ;; Clear screen using ANSI escape codes
          (format t "~C[2J~C[H" #\Escape #\Escape)
          (force-output)
          (let ((now (current-time-string)))
            (format t "=== sprite-watch: ~A / ~A === ~A ===~%" sprite-name task-id now))
          (format t "~%")

          ;; Show task status
          (let ((output (watch-sx-print
                         sprite-name
                         (format nil "cd /home/sprite && bd show ~A 2>/dev/null" task-id))))
            (when (zerop (length output))
              (format t "(could not read task)~%")))
          (format t "~%")

          ;; Show recent comments
          (format t "--- Recent updates ---~%")
          (let ((output (watch-sx-print
                         sprite-name
                         (format nil "cd /home/sprite && bd comments ~A 2>/dev/null | tail -8"
                                 task-id))))
            (when (zerop (length output))
              (format t "(no comments)~%")))
          (format t "~%")

          ;; Check if done
          (let ((status (watch-sx
                         sprite-name
                         (format nil "cd /home/sprite && bd show ~A 2>/dev/null | grep -i status"
                                 task-id))))
            (when (plusp (length status))
              (let ((status-lower (string-downcase status)))
                (when (or (search "closed" status-lower)
                          (search "done" status-lower)
                          (search "completed" status-lower))
                  (format t "==========================================~%")
                  (format t "PROJECT COMPLETE~%")
                  (format t "==========================================~%")
                  (return)))))

          (sleep poll-interval))

      ;; Handle Ctrl+C gracefully
      (sb-sys:interactive-interrupt ()
        (format t "~%Stopped.~%")))))
