(ns sprite-tool.launch
  "sprite-tool launch: Create and configure a sprite with coding agent, git, beads."
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [sprite-tool.config :as config]
            [sprite-tool.sprite :as sprite])
  (:import [java.io File]
           [java.time LocalTime]
           [java.time.format DateTimeFormatter]))

(def cli-options
  [[nil "--dry-run" "Show what would happen without executing"
    :id :dry-run
    :default false]
   [nil "--no-checkpoint" "Disable auto-checkpointing"
    :id :no-checkpoint
    :default false]
   [nil "--upload DIR" "Upload a local directory to /home/sprite/<dirname> (repeatable)"
    :id :upload-dirs
    :default []
    :assoc-fn (fn [m k v] (update m k conj v))]
   ["-h" "--help" "Show usage"]])

(defn- usage [summary]
  (println
   (str "Usage: sprite-tool launch [options] <sprite-name> [plan-file]\n"
        "\n"
        "Options:\n"
        summary
        "\n"
        "\n"
        "Environment variables:\n"
        "  ENV_FILE               Path to .env file (default: ./.env)\n"
        "  SPRITE_TOKEN           sprites.dev API token (or SPRITES_TOKEN in .env)\n"
        "  AGENT                  \"opencode\" (default) or \"claude\"\n"
        "  CLAUDE_AUTH            \"subscription\" (default) or \"apikey\"\n"
        "  MODEL                  Model override (see below)\n"
        "  CHECKPOINT_INTERVAL    Seconds between checkpoints (default: 300 = 5min)\n"
        "\n"
        "Model examples:\n"
        "  OpenCode: MODEL=opencode/big-pickle  (free, default)\n"
        "            MODEL=groq/llama-3.3-70b-versatile  (free w/ GROQ_API_KEY)\n"
        "            MODEL=openai/gpt-4o\n"
        "            MODEL=google/gemini-2.5-pro\n"
        "  Claude:   MODEL=opus, MODEL=sonnet, MODEL=haiku\n"
        "\n"
        "Examples:\n"
        "  sprite-tool launch my-project plan.md\n"
        "  sprite-tool launch --upload ./data my-project plan.md\n"
        "  sprite-tool launch --upload ./data --upload ./tests my-project plan.md\n"
        "  sprite-tool launch --dry-run my-project plan.md\n"
        "  MODEL=groq/llama-3.3-70b-versatile sprite-tool launch dev plan.md\n"
        "  AGENT=claude MODEL=sonnet sprite-tool launch dev plan.md"))
  (System/exit 1))

(defn- time-now []
  (.format (LocalTime/now) (DateTimeFormatter/ofPattern "HH:mm:ss")))

(defn- start-checkpointing
  "Start a background checkpoint loop. Returns an atom (stop-flag) and the thread."
  [sprite-name interval]
  (let [stop-flag (atom false)
        thread    (Thread.
                   (fn []
                     (try
                       (loop []
                         (Thread/sleep (* interval 1000))
                         (when-not @stop-flag
                           (println (str "[checkpoint] Creating checkpoint at " (time-now) "..."))
                           (try
                             (let [result (sprite/run-process
                                          ["sprite" "checkpoint" "create" "-s" sprite-name]
                                          :inherit-io false :capture true)]
                               (if (zero? (:exit result))
                                 (println "[checkpoint] Done.")
                                 (println "[checkpoint] Failed (non-fatal).")))
                             (catch Exception _
                               (println "[checkpoint] Failed (non-fatal).")))
                           (recur)))
                       (catch InterruptedException _))))]
    (.setDaemon thread true)
    (.start thread)
    (println (str "Auto-checkpointing every " interval "s (thread)"))
    {:stop-flag stop-flag :thread thread}))

(defn- stop-checkpointing [{:keys [stop-flag thread]}]
  (when stop-flag
    (reset! stop-flag true))
  (when thread
    (try
      (.interrupt ^Thread thread)
      (.join ^Thread thread 5000)
      (catch Exception _))))

(defn run
  "Execute the launch subcommand."
  [args]
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)
        _ (when (:help options) (usage summary))
        _ (when errors
            (doseq [e errors] (println e))
            (System/exit 1))
        _ (when (empty? arguments) (usage summary))

        ;; Load config
        cfg (config/load-config)

        ;; Merge CLI options into config
        dry-run      (:dry-run options)
        checkpointing (not (:no-checkpoint options))
        upload-dirs   (:upload-dirs options)

        sprite-name  (first arguments)
        plan-file    (second arguments)

        ;; Shortcut helpers
        sx-fn        (fn [cmd] (sprite/sx sprite-name cmd :dry-run dry-run))
        sx-pass-fn   (fn [cmd] (sprite/sx-passthrough sprite-name cmd :dry-run dry-run))
        push-file-fn (fn [src dest] (sprite/push-file sprite-name src dest :dry-run dry-run))
        push-dir-fn  (fn [src dest] (sprite/push-dir sprite-name src dest :dry-run dry-run))]

    ;; 1. Check/install sprite CLI
    (when-not (sprite/find-sprite-cli)
      (if dry-run
        (println "  [dry-run] Would install sprite CLI")
        (do
          (println "Installing sprite CLI...")
          (sprite/run-process ["bash" "-c" "curl -fsSL https://sprites.dev/install.sh | sh"]
                              :inherit-io true))))

    ;; 2. Auth sprite (non-interactive if token provided)
    (if (not (str/blank? (:sprite-token cfg)))
      (do
        (println "Authenticating sprite with token...")
        (when-not dry-run
          (sprite/run-process ["sprite" "auth" "setup" "--token" (:sprite-token cfg)]
                              :inherit-io true)))
      (do
        (println "No SPRITE_TOKEN set. Running interactive login...")
        (when-not dry-run
          (sprite/run-process ["sprite" "login"] :inherit-io true))))

    ;; 3. Create sprite (or use existing)
    (if dry-run
      (println (str "  [dry-run] Would create or reuse sprite '" sprite-name "'"))
      (if (sprite/sprite-exists? sprite-name)
        (println (str "Sprite '" sprite-name "' already exists, using it."))
        (do
          (println (str "Creating sprite: " sprite-name))
          (sprite/run-process ["sprite" "create" "-skip-console" sprite-name]
                              :inherit-io true))))

    ;; 4. Push .env to sprite
    (let [env-file (:env-file cfg)]
      (when (.isFile (File. ^String env-file))
        (println (str "Pushing " env-file "..."))
        (push-file-fn env-file "/home/sprite/.env")))

    ;; 5. Push plan file if provided
    (when (and plan-file (.isFile (File. ^String plan-file)))
      (println (str "Pushing " plan-file "..."))
      (push-file-fn plan-file "/home/sprite/plan.md"))

    ;; 6. Upload directories if provided
    (doseq [dir upload-dirs]
      (if (.isDirectory (File. ^String dir))
        (let [dirname (.getName (File. ^String dir))]
          (println (str "Uploading directory: " dir " -> /home/sprite/" dirname))
          (push-dir-fn dir (str "/home/sprite/" dirname)))
        (println (str "WARNING: --upload dir '" dir "' not found, skipping."))))

    ;; 7. Setup git + beads
    (println "Initializing git...")
    (sx-fn "cd /home/sprite && git init -b main 2>/dev/null || true")

    (println "Installing beads...")
    (sx-fn "curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash")

    ;; 8. Install and auth coding agent
    (let [agent-name (:agent cfg)]
      (cond
        (= agent-name "claude")
        (do
          (println "Setting up claude...")
          (sx-fn "command -v claude >/dev/null 2>&1 || npm install -g @anthropic-ai/claude-code")

          (cond
            (= (:claude-auth cfg) "subscription")
            (let [creds-path (str (System/getProperty "user.home") "/.claude/.credentials.json")]
              (if (.isFile (File. ^String creds-path))
                (do
                  (println "Copying claude subscription credentials...")
                  (push-file-fn creds-path "/home/sprite/.claude/.credentials.json")
                  (sx-fn "chmod 600 ~/.claude/.credentials.json"))
                (do
                  (println "ERROR: ~/.claude/.credentials.json not found")
                  (println "Run 'claude' locally first to authenticate, then re-run this script.")
                  (System/exit 1))))

            (and (= (:claude-auth cfg) "apikey")
                 (not (str/blank? (:anthropic-api-key cfg))))
            (do
              (println "Setting ANTHROPIC_API_KEY in sprite...")
              (sx-fn (str "grep -q ANTHROPIC_API_KEY ~/.bashrc 2>/dev/null || "
                          "echo 'export ANTHROPIC_API_KEY=\"" (:anthropic-api-key cfg) "\"' >> ~/.bashrc")))

            :else
            (do
              (println "ERROR: No valid claude auth configured")
              (println "Set CLAUDE_AUTH=subscription (default) or CLAUDE_AUTH=apikey with ANTHROPIC_API_KEY")
              (System/exit 1))))

        (= agent-name "opencode")
        (do
          (println "Setting up opencode...")
          (sx-fn "[ -x ~/.opencode/bin/opencode ] || curl -fsSL https://opencode.ai/install | bash")
          (sx-fn (str "grep -q 'source.*\\.env' ~/.bashrc 2>/dev/null || "
                      "echo '[ -f /home/sprite/.env ] && set -a && source /home/sprite/.env && set +a' >> ~/.bashrc")))

        :else
        (do
          (println (str "ERROR: Unknown AGENT '" agent-name "'. Use 'claude' or 'opencode'."))
          (System/exit 1))))

    ;; 9. Launch agent with plan (or open console)
    (println)
    (println "==========================================")
    (println (str "Sprite '" sprite-name "' is ready!"))
    (let [model-note (if (str/blank? (:model cfg)) "" (str " (model: " (:model cfg) ")"))]
      (println (str "Agent: " (:agent cfg) model-note)))
    (when checkpointing
      (println (str "Checkpointing: every " (:checkpoint-interval cfg) "s")))
    (println "==========================================")

    (if dry-run
      (do
        (println)
        (println (str "[dry-run] Would launch " (:agent cfg) " with plan. No changes were made.")))

      (if plan-file
        ;; Launch with plan
        (let [checkpoint-state (when checkpointing
                                 (start-checkpointing sprite-name (:checkpoint-interval cfg)))
              ;; Shutdown hook for cleanup
              shutdown-hook (Thread.
                             (fn []
                               (when checkpoint-state
                                 (stop-checkpointing checkpoint-state))))]
          (.addShutdownHook (Runtime/getRuntime) shutdown-hook)

          (try
            (println (str "Launching " (:agent cfg) " with plan..."))

            (cond
              (= (:agent cfg) "claude")
              (let [model-flag (if (str/blank? (:model cfg))
                                 ""
                                 (str "--model " (:model cfg) " "))]
                (sx-pass-fn (str "cd /home/sprite && claude " model-flag
                                 "-p 'read plan.md and complete the plan please'")))

              (= (:agent cfg) "opencode")
              (let [oc-model (if (str/blank? (:model cfg))
                               "opencode/big-pickle"
                               (:model cfg))]
                (sx-pass-fn (str "set -a && source /home/sprite/.env 2>/dev/null && set +a && "
                                 "cd /home/sprite && ~/.opencode/bin/opencode run -m " oc-model
                                 " 'read plan.md and complete the plan please'"))))

            ;; Final checkpoint
            (when checkpoint-state
              (stop-checkpointing checkpoint-state))
            (println "Creating final checkpoint...")
            (let [result (sprite/run-process
                          ["sprite" "checkpoint" "create" "-s" sprite-name]
                          :inherit-io false :capture true)]
              (if (zero? (:exit result))
                (println "Final checkpoint saved.")
                (println "Final checkpoint failed (non-fatal).")))

            (finally
              (try
                (.removeShutdownHook (Runtime/getRuntime) shutdown-hook)
                (catch IllegalStateException _)))))

        ;; No plan file — open console
        (do
          (println "Opening console...")
          (sprite/run-process ["sprite" "console" "-s" sprite-name]
                              :inherit-io true))))))
