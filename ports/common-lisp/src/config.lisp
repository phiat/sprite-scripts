(in-package :sprite-tool)

;;; ============================================================
;;; Configuration: .env parsing and environment variable loading
;;; ============================================================

(defstruct config
  (sprite-token "" :type string)
  (agent "opencode" :type string)
  (claude-auth "subscription" :type string)
  (anthropic-api-key "" :type string)
  (model "" :type string)
  (checkpoint-interval 300 :type integer)
  (env-file "./.env" :type string)
  ;; CLI flags
  (dry-run nil :type boolean)
  (checkpointing t :type boolean)
  (upload-dirs nil :type list))

;;; ------------------------------------------------------------
;;; .env file parser
;;; ------------------------------------------------------------

(defun split-lines (str)
  "Split STR into a list of lines."
  (loop for start = 0 then (1+ end)
        for end = (position #\Newline str :start start)
        collect (subseq str start (or end (length str)))
        while end))

(defun strip-quotes (value)
  "Strip matching single or double quotes from VALUE if present."
  (let ((len (length value)))
    (if (and (>= len 2)
             (char= (char value 0) (char value (1- len)))
             (or (char= (char value 0) #\")
                 (char= (char value 0) #\')))
        (subseq value 1 (1- len))
        value)))

(defun parse-env-file (path)
  "Parse a .env file at PATH. Returns an alist of (KEY . VALUE).
Sets parsed values into the process environment (does not overwrite existing)."
  (unless (uiop:file-exists-p path)
    (return-from parse-env-file nil))
  (let ((content (uiop:read-file-string path))
        (result nil))
    (dolist (line (split-lines content))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
        ;; Skip blank lines and comments
        (when (or (zerop (length trimmed))
                  (char= (char trimmed 0) #\#))
          (go :next))
        ;; Must contain =
        (let ((eq-pos (position #\= trimmed)))
          (unless eq-pos (go :next))
          (let* ((key (string-trim '(#\Space #\Tab)
                                   (subseq trimmed 0 eq-pos)))
                 (raw-value (string-trim '(#\Space #\Tab)
                                         (subseq trimmed (1+ eq-pos))))
                 (value (strip-quotes raw-value)))
            (push (cons key value) result)
            ;; Set in environment if not already present
            (unless (uiop:getenv key)
              (sb-posix:setenv key value 1)))))
      :next)
    (nreverse result)))

(defun getenv-or (key default)
  "Get environment variable KEY, or DEFAULT if not set or empty."
  (let ((val (uiop:getenv key)))
    (if (and val (plusp (length val)))
        val
        default)))

(defun load-config ()
  "Load configuration from .env file and environment variables.
Returns a CONFIG struct."
  (let ((env-file (getenv-or "ENV_FILE" "./.env")))
    ;; Parse .env file first (populates environment for keys not already set)
    (parse-env-file env-file)

    ;; SPRITE_TOKEN with SPRITES_TOKEN fallback
    (let* ((sprite-token (getenv-or "SPRITE_TOKEN" ""))
           (sprite-token (if (zerop (length sprite-token))
                             (getenv-or "SPRITES_TOKEN" "")
                             sprite-token))
           ;; Agent
           (agent (getenv-or "AGENT" "opencode"))
           ;; Claude auth
           (claude-auth (getenv-or "CLAUDE_AUTH" "subscription"))
           ;; Anthropic API key
           (anthropic-api-key (getenv-or "ANTHROPIC_API_KEY" ""))
           ;; Model
           (model (getenv-or "MODEL" ""))
           ;; Checkpoint interval
           (interval-str (getenv-or "CHECKPOINT_INTERVAL" "300"))
           (checkpoint-interval (handler-case (parse-integer interval-str)
                                  (error ()
                                    (format *error-output*
                                            "Error: invalid CHECKPOINT_INTERVAL ~S (must be integer)~%"
                                            interval-str)
                                    (sb-ext:exit :code 1)))))
      (make-config
       :sprite-token sprite-token
       :agent agent
       :claude-auth claude-auth
       :anthropic-api-key anthropic-api-key
       :model model
       :checkpoint-interval checkpoint-interval
       :env-file env-file))))
