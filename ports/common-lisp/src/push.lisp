(in-package :sprite-tool)

;;; ============================================================
;;; sprite-tool push: Push local file or directory to a sprite
;;; ============================================================

(defun push-usage ()
  "Print usage for the push subcommand and exit."
  (format t "Usage: sprite-tool push <local-path> <remote-path> [sprite-name]~%~%")
  (format t "Examples:~%")
  (format t "  sprite-tool push ./file.txt /home/sprite/file.txt~%")
  (format t "  sprite-tool push ./mydir /home/sprite/mydir~%")
  (format t "  sprite-tool push ./file.txt /tmp/file.txt ember-red-hawk~%")
  (sb-ext:exit :code 1))

(defun local-path-is-directory-p (path)
  "Check if local PATH is an existing directory."
  (uiop:directory-exists-p
   (uiop:parse-unix-namestring
    (if (uiop:string-suffix-p "/" path)
        path
        (concatenate 'string path "/")))))

(defun local-path-exists-p (path)
  "Check if local PATH exists as either a file or directory."
  (or (uiop:file-exists-p path)
      (local-path-is-directory-p path)))

(defun run-push (args)
  "Execute the push subcommand."
  (when (< (length args) 2)
    (push-usage))

  (let* ((local-path (first args))
         (remote-path (second args))
         (sprite-name (third args))
         (sprite-args (if sprite-name
                          (list "-s" sprite-name)
                          nil)))

    ;; Check local path exists
    (unless (local-path-exists-p local-path)
      (format *error-output* "Error: ~A does not exist~%" local-path)
      (sb-ext:exit :code 1))

    (if (local-path-is-directory-p local-path)
        ;; Push directory
        (let* ((src (uiop:truenamize (uiop:parse-unix-namestring
                                      (if (uiop:string-suffix-p "/" local-path)
                                          local-path
                                          (concatenate 'string local-path "/")))))
               (parent (namestring (uiop:pathname-parent-directory-pathname src)))
               (base (car (last (pathname-directory src)))))
          (format t "Pushing directory: ~A -> ~A~%" local-path remote-path)
          ;; tar czf - -C parent base | sprite exec ... bash -c "mkdir -p DEST && tar xzf - -C DEST --strip-components=1"
          (let ((sprite-args-joined (format nil "~{~A ~}" sprite-args)))
            (uiop:run-program
             (format nil "tar czf - -C '~A' '~A' | sprite exec ~Abash -c \"mkdir -p '~A' && tar xzf - -C '~A' --strip-components=1\""
                     parent base sprite-args-joined remote-path remote-path)
             :output t :error-output t :ignore-error-status nil)))

        ;; Push file
        (let ((remote-dir (posix-dirname remote-path)))
          (format t "Pushing file: ~A -> ~A~%" local-path remote-path)
          (let ((sprite-cmd (append (list "sprite" "exec")
                                    sprite-args
                                    (list "bash" "-c"
                                          (format nil "mkdir -p '~A' && cat > '~A'"
                                                  remote-dir remote-path)))))
            (run-program-pipe-file sprite-cmd local-path))))

    (format t "Done.~%")))
