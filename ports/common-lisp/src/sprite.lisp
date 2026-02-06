(in-package :sprite-tool)

;;; ============================================================
;;; Subprocess wrapper for the sprite CLI
;;; ============================================================

(defun run-program-capture (cmd)
  "Run CMD (a list of strings) and return stdout as a string.
Returns empty string on failure."
  (handler-case
      (uiop:run-program cmd :output '(:string :stripped t)
                            :error-output nil
                            :ignore-error-status t)
    (error () "")))

(defun run-program-passthrough (cmd)
  "Run CMD (a list of strings) with stdout/stderr inherited.
Returns the exit code."
  (nth-value 2
    (uiop:run-program cmd :output t
                          :error-output t
                          :ignore-error-status t)))

(defun run-program-with-input (cmd input-string)
  "Run CMD (a list of strings) with INPUT-STRING piped to stdin.
Returns the exit code."
  (nth-value 2
    (uiop:run-program cmd :input (make-string-input-stream input-string)
                          :output t
                          :error-output t
                          :ignore-error-status t)))

(defun run-program-pipe-file (cmd file-path)
  "Run CMD (a list of strings) with FILE-PATH as stdin.
Returns the exit code."
  (with-open-file (in file-path :direction :input
                                :element-type '(unsigned-byte 8))
    (nth-value 2
      (uiop:run-program cmd :input in
                            :output t
                            :error-output t
                            :ignore-error-status t))))

;;; ------------------------------------------------------------
;;; sprite exec helpers
;;; ------------------------------------------------------------

(defun sx (sprite-name cmd &key (dry-run nil))
  "Run a command inside a sprite via bash, capturing output.
Equivalent to: sprite exec -s SPRITE bash -c \"CMD\"
Returns stdout as a string, or nil if dry-run."
  (if dry-run
      (progn
        (format t "  [dry-run] sprite exec -s ~A bash -c \"~A\"~%" sprite-name cmd)
        nil)
      (let ((output (run-program-capture
                     (list "sprite" "exec" "-s" sprite-name "bash" "-c" cmd))))
        (if (stringp output)
            (string-trim '(#\Space #\Tab #\Newline #\Return) output)
            ""))))

(defun sx-passthrough (sprite-name cmd &key (dry-run nil))
  "Run a command inside a sprite, passing stdout/stderr through.
Returns the exit code."
  (if dry-run
      (progn
        (format t "  [dry-run] sprite exec -s ~A bash -c \"~A\"~%" sprite-name cmd)
        0)
      (run-program-passthrough
       (list "sprite" "exec" "-s" sprite-name "bash" "-c" cmd))))

(defun push-file (sprite-name src dest &key (dry-run nil))
  "Push a local file SRC to DEST on the sprite.
Equivalent to:
  sprite exec -s SPRITE bash -c \"mkdir -p $(dirname DEST)\"
  sprite exec -s SPRITE bash -c \"cat > DEST\" < src"
  (if dry-run
      (format t "  [dry-run] push ~A -> sprite:~A~%" src dest)
      (let ((parent (posix-dirname dest)))
        (uiop:run-program
         (list "sprite" "exec" "-s" sprite-name "bash" "-c"
               (format nil "mkdir -p '~A'" parent))
         :output nil :error-output nil :ignore-error-status t)
        (run-program-pipe-file
         (list "sprite" "exec" "-s" sprite-name "bash" "-c"
               (format nil "cat > '~A'" dest))
         src))))

(defun posix-dirname (path)
  "Return the directory portion of a POSIX PATH string.
E.g., \"/home/sprite/.env\" -> \"/home/sprite\"
      \"/home/sprite/plan.md\" -> \"/home/sprite\""
  (let ((pos (position #\/ path :from-end t)))
    (if (and pos (plusp pos))
        (subseq path 0 pos)
        "/")))

(defun posix-basename (path)
  "Return the base name of a POSIX PATH string.
E.g., \"/home/sprite/mydir\" -> \"mydir\""
  (let ((pos (position #\/ path :from-end t)))
    (if pos
        (subseq path (1+ pos))
        path)))

(defun push-dir (sprite-name src dest &key (dry-run nil))
  "Push a local directory SRC to DEST on the sprite via tar.
Equivalent to:
  sprite exec -s SPRITE bash -c \"mkdir -p DEST\"
  tar czf - -C parent base | sprite exec -s SPRITE bash -c \"tar xzf - -C parent_of_dest\""
  (if dry-run
      (format t "  [dry-run] push dir ~A -> sprite:~A~%" src dest)
      (let* ((src-path (uiop:truenamize (uiop:parse-unix-namestring src)))
             (parent (namestring (uiop:pathname-parent-directory-pathname src-path)))
             (base (car (last (pathname-directory src-path))))
             (dest-parent (posix-dirname dest)))
        ;; mkdir -p dest on sprite
        (uiop:run-program
         (list "sprite" "exec" "-s" sprite-name "bash" "-c"
               (format nil "mkdir -p '~A'" dest))
         :output nil :error-output nil :ignore-error-status t)
        ;; tar czf - -C parent base | sprite exec ... bash -c "tar xzf - -C dest_parent"
        (uiop:run-program
         (format nil "tar czf - -C '~A' '~A' | sprite exec -s '~A' bash -c \"tar xzf - -C '~A'\""
                 parent base sprite-name dest-parent)
         :output t :error-output t :ignore-error-status t))))

(defun sprite-list ()
  "Run 'sprite ls' and return stdout as a string."
  (run-program-capture (list "sprite" "ls")))

(defun sprite-exists-p (sprite-name)
  "Check if a sprite with the given name already exists."
  (let ((output (sprite-list)))
    (when (and (stringp output) (plusp (length output)))
      (dolist (line (split-lines output))
        (let ((parts (split-words line)))
          (when (member sprite-name parts :test #'string=)
            (return-from sprite-exists-p t)))))
    nil))

(defun split-words (str)
  "Split STR on whitespace into a list of non-empty strings."
  (let ((result nil)
        (len (length str))
        (start 0))
    (loop
      ;; skip whitespace
      (loop while (and (< start len)
                       (member (char str start) '(#\Space #\Tab)))
            do (incf start))
      (when (>= start len) (return))
      ;; find end of word
      (let ((end start))
        (loop while (and (< end len)
                         (not (member (char str end) '(#\Space #\Tab))))
              do (incf end))
        (push (subseq str start end) result)
        (setf start end)))
    (nreverse result)))
