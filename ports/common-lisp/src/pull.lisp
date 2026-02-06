(in-package :sprite-tool)

;;; ============================================================
;;; sprite-tool pull: Pull file or directory from a sprite
;;; ============================================================

(defun pull-usage ()
  "Print usage for the pull subcommand and exit."
  (format t "Usage: sprite-tool pull <remote-path> <local-path> [sprite-name]~%~%")
  (format t "Examples:~%")
  (format t "  sprite-tool pull /home/sprite/file.txt ./file.txt~%")
  (format t "  sprite-tool pull /home/sprite/mydir ./mydir~%")
  (format t "  sprite-tool pull /tmp/file.txt ./file.txt ember-red-hawk~%")
  (sb-ext:exit :code 1))

(defun run-pull (args)
  "Execute the pull subcommand."
  (when (< (length args) 2)
    (pull-usage))

  (let* ((remote-path (first args))
         (local-path (second args))
         (sprite-name (third args))
         (sprite-args (if sprite-name
                          (list "-s" sprite-name)
                          nil)))

    ;; Check if remote is directory or file
    (let* ((check-cmd (append (list "sprite" "exec")
                              sprite-args
                              (list "bash" "-c"
                                    (format nil "[ -d '~A' ] && echo dir || echo file"
                                            remote-path))))
           (result (run-program-capture check-cmd))
           (is-dir (string= (string-trim '(#\Space #\Tab #\Newline #\Return) result) "dir")))

      (if is-dir
          ;; Pull directory
          (progn
            (format t "Pulling directory: ~A -> ~A~%" remote-path local-path)
            (ensure-local-directory local-path)
            ;; sprite exec ... tar czf - -C REMOTE . | tar xzf - -C LOCAL
            (uiop:run-program
             (format nil "sprite exec ~{~A ~} tar czf - -C '~A' . | tar xzf - -C '~A'"
                     sprite-args remote-path local-path)
             :output t :error-output t :ignore-error-status nil))

          ;; Pull file
          (progn
            (format t "Pulling file: ~A -> ~A~%" remote-path local-path)
            (ensure-local-directory (posix-dirname local-path))
            ;; sprite exec ... cat REMOTE > local
            (let* ((sprite-cmd (append (list "sprite" "exec")
                                       sprite-args
                                       (list "cat" remote-path)))
                   (content (run-program-capture sprite-cmd)))
              (with-open-file (out local-path :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                (write-string content out))))))

    (format t "Done.~%")))

(defun ensure-local-directory (path)
  "Ensure local directory PATH exists, creating it if needed."
  (ensure-directories-exist
   (uiop:parse-unix-namestring
    (if (uiop:string-suffix-p "/" path)
        path
        (concatenate 'string path "/")))))
