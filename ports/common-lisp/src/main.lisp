(in-package :sprite-tool)

;;; ============================================================
;;; Entry point and subcommand dispatch
;;; ============================================================

(defparameter *version* "0.1.0")

(defun print-help ()
  "Print top-level help text."
  (format t "sprite-tool: manage sprites with coding agents, git, and beads~%~%")
  (format t "Usage: sprite-tool <command> [options] [args...]~%~%")
  (format t "Commands:~%")
  (format t "  launch    Create and configure a sprite with coding agent, git, beads~%")
  (format t "  push      Push local file or directory to a sprite~%")
  (format t "  pull      Pull file or directory from a sprite~%")
  (format t "  watch     Poll beads task for progress~%~%")
  (format t "Run 'sprite-tool <command> --help' for command-specific help.~%~%")
  (format t "Global options:~%")
  (format t "  --help, -h    Show this help~%")
  (format t "  --version     Show version~%"))

(defun main ()
  "Main CLI entry point. Dispatches to subcommands."
  (let* ((argv (uiop:command-line-arguments))
         (argc (length argv)))

    ;; No arguments or help requested
    (when (or (zerop argc)
              (member (first argv) '("-h" "--help" "help") :test #'string=))
      (print-help)
      (sb-ext:exit :code 0))

    ;; Version
    (when (string= (first argv) "--version")
      (format t "sprite-tool ~A~%" *version*)
      (sb-ext:exit :code 0))

    (let ((subcommand (first argv))
          (sub-args (rest argv)))

      (cond
        ((string= subcommand "launch")
         (run-launch sub-args))
        ((string= subcommand "push")
         (run-push sub-args))
        ((string= subcommand "pull")
         (run-pull sub-args))
        ((string= subcommand "watch")
         (run-watch sub-args))
        (t
         (format *error-output* "Error: unknown subcommand '~A'~%~%" subcommand)
         (print-help)
         (sb-ext:exit :code 1))))))
