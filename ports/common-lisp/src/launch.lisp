(in-package :sprite-tool)

;;; ============================================================
;;; sprite-tool launch: Create and configure a sprite with
;;; coding agent, git, beads
;;; ============================================================

(defun launch-usage ()
  "Print usage for the launch subcommand and exit."
  (format t "Usage: sprite-tool launch [options] <sprite-name> [plan-file]~%~%")
  (format t "Options:~%")
  (format t "  --dry-run              Show what would happen without executing~%")
  (format t "  --no-checkpoint        Disable auto-checkpointing~%")
  (format t "  --upload <dir>         Upload a local directory to /home/sprite/<dirname>~%")
  (format t "                         (repeatable: --upload ./data --upload ./tests)~%~%")
  (format t "Environment variables:~%")
  (format t "  ENV_FILE               Path to .env file (default: ./.env)~%")
  (format t "  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)~%")
  (format t "  AGENT                  \"opencode\" (default) or \"claude\"~%")
  (format t "  CLAUDE_AUTH            \"subscription\" (default) or \"apikey\"~%")
  (format t "  MODEL                  Model override (see below)~%")
  (format t "  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)~%~%")
  (format t "Model examples:~%")
  (format t "  OpenCode: MODEL=opencode/big-pickle  (free, default)~%")
  (format t "            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)~%")
  (format t "            MODEL=openai/gpt-4o~%")
  (format t "            MODEL=google/gemini-2.5-pro~%")
  (format t "  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku~%~%")
  (format t "Examples:~%")
  (format t "  sprite-tool launch my-project plan.md~%")
  (format t "  sprite-tool launch --upload ./data my-project plan.md~%")
  (format t "  sprite-tool launch --upload ./data --upload ./tests my-project plan.md~%")
  (format t "  sprite-tool launch --dry-run my-project plan.md~%")
  (format t "  MODEL=groq/llama-3.3-70b-versatile sprite-tool launch dev plan.md~%")
  (format t "  AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md~%")
  (sb-ext:exit :code 1))

;;; ------------------------------------------------------------
;;; Background checkpoint loop
;;; ------------------------------------------------------------

(defstruct checkpoint-loop
  (sprite-name "" :type string)
  (interval 300 :type integer)
  (stop-flag nil)
  (thread nil)
  (lock (sb-thread:make-mutex :name "checkpoint-lock")))

(defun start-checkpoint-loop (loop-obj)
  "Start the background checkpoint thread."
  (setf (checkpoint-loop-stop-flag loop-obj) nil)
  (setf (checkpoint-loop-thread loop-obj)
        (sb-thread:make-thread
         (lambda ()
           (loop
             ;; Sleep in small increments to allow quick stopping
             (let ((elapsed 0))
               (loop while (and (< elapsed (checkpoint-loop-interval loop-obj))
                                (not (checkpoint-loop-stop-flag loop-obj)))
                     do (sleep 1)
                        (incf elapsed)))
             (when (checkpoint-loop-stop-flag loop-obj)
               (return))
             (let ((now (current-time-string)))
               (format t "[checkpoint] Creating checkpoint at ~A...~%" now)
               (force-output))
             (handler-case
                 (let ((exit-code
                         (nth-value 2
                           (uiop:run-program
                            (list "sprite" "checkpoint" "create"
                                  "-s" (checkpoint-loop-sprite-name loop-obj))
                            :output nil
                            :error-output nil
                            :ignore-error-status t))))
                   (if (zerop exit-code)
                       (format t "[checkpoint] Done.~%")
                       (format t "[checkpoint] Failed (non-fatal).~%")))
               (error ()
                 (format t "[checkpoint] Failed (non-fatal).~%")))
             (force-output)))
         :name "checkpoint")))

(defun stop-checkpoint-loop (loop-obj)
  "Stop the background checkpoint thread and wait for it to finish."
  (when loop-obj
    (setf (checkpoint-loop-stop-flag loop-obj) t)
    (when (and (checkpoint-loop-thread loop-obj)
               (sb-thread:thread-alive-p (checkpoint-loop-thread loop-obj)))
      (sb-thread:join-thread (checkpoint-loop-thread loop-obj)
                             :default nil :timeout 10))))

;;; ------------------------------------------------------------
;;; Main launch logic
;;; ------------------------------------------------------------

(defun run-launch (args)
  "Execute the launch subcommand."
  ;; Load config from .env + environment
  (let ((cfg (load-config))
        (positional nil))

    ;; Parse flags
    (let ((i 0))
      (loop while (< i (length args))
            for arg = (nth i args)
            do (cond
                 ((string= arg "--dry-run")
                  (setf (config-dry-run cfg) t)
                  (incf i))
                 ((string= arg "--no-checkpoint")
                  (setf (config-checkpointing cfg) nil)
                  (incf i))
                 ((string= arg "--upload")
                  (incf i)
                  (when (>= i (length args))
                    (format t "Error: --upload requires an argument~%")
                    (sb-ext:exit :code 1))
                  (push (nth i args) (config-upload-dirs cfg))
                  (incf i))
                 ((or (string= arg "--help") (string= arg "-h"))
                  (launch-usage))
                 ((and (>= (length arg) 2) (string= (subseq arg 0 2) "--"))
                  (format t "Unknown option: ~A~%" arg)
                  (launch-usage))
                 (t
                  (push arg positional)
                  (incf i)))))

    (setf positional (nreverse positional))
    (setf (config-upload-dirs cfg) (nreverse (config-upload-dirs cfg)))

    (when (< (length positional) 1)
      (launch-usage))

    (let* ((sprite-name (first positional))
           (plan-file (if (>= (length positional) 2) (second positional) ""))
           (dry-run (config-dry-run cfg)))

      ;; Local shorthand closures
      (flet ((lsx (cmd)
               (sx sprite-name cmd :dry-run dry-run))
             (lsx-pass (cmd)
               (sx-passthrough sprite-name cmd :dry-run dry-run))
             (lpush-file (src dest)
               (push-file sprite-name src dest :dry-run dry-run))
             (lpush-dir (src dest)
               (push-dir sprite-name src dest :dry-run dry-run)))

        ;; ===========================================================
        ;; 1. Check/install sprite CLI
        ;; ===========================================================
        (let ((sprite-found
                (zerop (nth-value 2
                         (uiop:run-program "command -v sprite"
                                           :output nil :error-output nil
                                           :ignore-error-status t
                                           :force-shell t)))))
          (unless sprite-found
            (if dry-run
                (format t "  [dry-run] Would install sprite CLI~%")
                (progn
                  (format t "Installing sprite CLI...~%")
                  (uiop:run-program "curl -fsSL https://sprites.dev/install.sh | sh"
                                    :output t :error-output t :force-shell t)
                  ;; Add common install location to PATH
                  (let ((local-bin (format nil "~A/.local/bin"
                                           (uiop:getenv "HOME"))))
                    (sb-posix:setenv "PATH"
                                     (format nil "~A:~A" local-bin (uiop:getenv "PATH"))
                                     1))))))

        ;; ===========================================================
        ;; 2. Auth sprite (non-interactive if token provided)
        ;; ===========================================================
        (if (plusp (length (config-sprite-token cfg)))
            (progn
              (format t "Authenticating sprite with token...~%")
              (unless dry-run
                (uiop:run-program
                 (list "sprite" "auth" "setup" "--token" (config-sprite-token cfg))
                 :output t :error-output t)))
            (progn
              (format t "No SPRITE_TOKEN set. Running interactive login...~%")
              (unless dry-run
                (uiop:run-program (list "sprite" "login")
                                  :output t :error-output t))))

        ;; ===========================================================
        ;; 3. Create sprite (or use existing)
        ;; ===========================================================
        (if dry-run
            (format t "  [dry-run] Would create or reuse sprite '~A'~%" sprite-name)
            (if (sprite-exists-p sprite-name)
                (format t "Sprite '~A' already exists, using it.~%" sprite-name)
                (progn
                  (format t "Creating sprite: ~A~%" sprite-name)
                  (uiop:run-program
                   (list "sprite" "create" "-skip-console" sprite-name)
                   :output t :error-output t))))

        ;; ===========================================================
        ;; 4. Push .env to sprite
        ;; ===========================================================
        (when (uiop:file-exists-p (config-env-file cfg))
          (format t "Pushing ~A...~%" (config-env-file cfg))
          (lpush-file (config-env-file cfg) "/home/sprite/.env"))

        ;; ===========================================================
        ;; 5. Push plan file if provided
        ;; ===========================================================
        (when (and (plusp (length plan-file))
                   (uiop:file-exists-p plan-file))
          (format t "Pushing ~A...~%" plan-file)
          (lpush-file plan-file "/home/sprite/plan.md"))

        ;; ===========================================================
        ;; 6. Upload directories if provided
        ;; ===========================================================
        (dolist (upload-dir (config-upload-dirs cfg))
          (if (uiop:directory-exists-p
               (uiop:parse-unix-namestring
                (if (uiop:string-suffix-p "/" upload-dir)
                    upload-dir
                    (concatenate 'string upload-dir "/"))))
              (let ((dirname (posix-basename upload-dir)))
                (format t "Uploading directory: ~A -> /home/sprite/~A~%" upload-dir dirname)
                (lpush-dir upload-dir (format nil "/home/sprite/~A" dirname)))
              (format t "WARNING: --upload dir '~A' not found, skipping.~%" upload-dir)))

        ;; ===========================================================
        ;; 7. Setup git + beads
        ;; ===========================================================
        (format t "Initializing git...~%")
        (lsx "cd /home/sprite && git init -b main 2>/dev/null || true")

        (format t "Installing beads...~%")
        (lsx "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash")

        ;; ===========================================================
        ;; 8. Install and auth coding agent
        ;; ===========================================================
        (cond
          ((string= (config-agent cfg) "claude")
           (format t "Setting up claude...~%")
           (lsx "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code")

           (cond
             ((string= (config-claude-auth cfg) "subscription")
              (let ((creds-path (format nil "~A/.claude/.credentials.json"
                                        (uiop:getenv "HOME"))))
                (if (uiop:file-exists-p creds-path)
                    (progn
                      (format t "Copying claude subscription credentials...~%")
                      (lpush-file creds-path "/home/sprite/.claude/.credentials.json")
                      (lsx "chmod 600 ~/.claude/.credentials.json"))
                    (progn
                      (format t "ERROR: ~/.claude/.credentials.json not found~%")
                      (format t "Run 'claude' locally first to authenticate, then re-run this script.~%")
                      (sb-ext:exit :code 1)))))

             ((and (string= (config-claude-auth cfg) "apikey")
                   (plusp (length (config-anthropic-api-key cfg))))
              (format t "Setting ANTHROPIC_API_KEY in sprite...~%")
              (lsx (format nil "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || echo 'export ANTHROPIC_API_KEY=\"~A\"' >> ~/.bashrc"
                           (config-anthropic-api-key cfg))))

             (t
              (format t "ERROR: No valid claude auth configured~%")
              (format t "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY~%")
              (sb-ext:exit :code 1))))

          ((string= (config-agent cfg) "opencode")
           (format t "Setting up opencode...~%")
           (lsx "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash")
           (lsx "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc"))

          (t
           (format t "ERROR: Unknown AGENT '~A'. Use 'claude' or 'opencode'.~%"
                   (config-agent cfg))
           (sb-ext:exit :code 1)))

        ;; ===========================================================
        ;; 9. Launch agent with plan (or open console)
        ;; ===========================================================
        (format t "~%")
        (format t "==========================================~%")
        (format t "Sprite '~A' is ready!~%" sprite-name)
        (let ((model-note (if (plusp (length (config-model cfg)))
                              (format nil " (model: ~A)" (config-model cfg))
                              "")))
          (format t "Agent: ~A~A~%" (config-agent cfg) model-note))
        (when (config-checkpointing cfg)
          (format t "Checkpointing: every ~Ds~%" (config-checkpoint-interval cfg)))
        (format t "==========================================~%")

        (when dry-run
          (format t "~%")
          (format t "[dry-run] Would launch ~A with plan. No changes were made.~%"
                  (config-agent cfg))
          (return-from run-launch))

        (let ((checkpoint-obj nil))
          ;; Ensure cleanup on exit
          (unwind-protect
               (if (plusp (length plan-file))
                   (progn
                     ;; Start auto-checkpointing before agent runs
                     (when (config-checkpointing cfg)
                       (setf checkpoint-obj
                             (make-checkpoint-loop
                              :sprite-name sprite-name
                              :interval (config-checkpoint-interval cfg)))
                       (start-checkpoint-loop checkpoint-obj))

                     (format t "Launching ~A with plan...~%" (config-agent cfg))

                     (cond
                       ((string= (config-agent cfg) "claude")
                        (let ((model-flag (if (plusp (length (config-model cfg)))
                                              (format nil "--model ~A " (config-model cfg))
                                              "")))
                          (lsx-pass
                           (format nil "cd /home/sprite && claude ~A-p 'read plan.md and complete the plan please'"
                                   model-flag))))

                       ((string= (config-agent cfg) "opencode")
                        (let ((oc-model (if (plusp (length (config-model cfg)))
                                            (config-model cfg)
                                            "opencode/big-pickle")))
                          (lsx-pass
                           (format nil "set -a && source /home/sprite/.env 2>/dev/null && set +a && cd /home/sprite && ~/.opencode/bin/opencode run -m ~A 'read plan.md and complete the plan please'"
                                   oc-model)))))

                     ;; Final checkpoint after agent completes
                     (when checkpoint-obj
                       (stop-checkpoint-loop checkpoint-obj)
                       (setf checkpoint-obj nil))
                     (format t "Creating final checkpoint...~%")
                     (let ((exit-code
                             (nth-value 2
                               (uiop:run-program
                                (list "sprite" "checkpoint" "create" "-s" sprite-name)
                                :output nil :error-output nil
                                :ignore-error-status t))))
                       (if (zerop exit-code)
                           (format t "Final checkpoint saved.~%")
                           (format t "Final checkpoint failed (non-fatal).~%"))))

                   ;; No plan file -- open console
                   (progn
                     (format t "Opening console...~%")
                     (uiop:run-program
                      (list "sprite" "console" "-s" sprite-name)
                      :output t :error-output t :input t)))

            ;; Cleanup form -- always stop checkpointing
            (when checkpoint-obj
              (stop-checkpoint-loop checkpoint-obj))))))))
