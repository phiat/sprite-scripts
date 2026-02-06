(ns sprite-tool.config
  "Configuration: .env parsing and environment variable loading."
  (:require [clojure.string :as str])
  (:import [java.io File]))

(defn parse-env-file
  "Hand-rolled .env parser.
   Skips blank lines and comments. Parses KEY=VALUE with optional
   single/double quote stripping. Returns a map of parsed key-value pairs.
   Also sets parsed values as system properties (does not overwrite existing env vars)."
  [path]
  (let [f (File. ^String path)]
    (if (.isFile f)
      (let [lines (str/split-lines (slurp f))]
        (reduce
         (fn [acc line]
           (let [trimmed (str/trim line)]
             (if (or (str/blank? trimmed)
                     (str/starts-with? trimmed "#")
                     (not (str/includes? trimmed "=")))
               acc
               (let [idx       (str/index-of trimmed "=")
                     key       (str/trim (subs trimmed 0 idx))
                     raw-value (str/trim (subs trimmed (inc idx)))
                     value     (if (and (>= (count raw-value) 2)
                                        (= (first raw-value) (last raw-value))
                                        (contains? #{\' \"} (first raw-value)))
                                 (subs raw-value 1 (dec (count raw-value)))
                                 raw-value)]
                 ;; Set in system environment only if not already set
                 (when (nil? (System/getenv key))
                   (System/setProperty key value))
                 (assoc acc key value)))))
         {}
         lines))
      {})))

(defn- env-or-prop
  "Get value from environment variable, falling back to system property, then default."
  [key default]
  (or (System/getenv key)
      (System/getProperty key)
      default))

(defn load-config
  "Load configuration from .env file and environment variables.
   Returns a map with all config keys."
  []
  (let [env-file (env-or-prop "ENV_FILE" "./.env")
        _parsed  (parse-env-file env-file)

        ;; SPRITE_TOKEN with SPRITES_TOKEN fallback
        sprite-token (let [t (env-or-prop "SPRITE_TOKEN" "")]
                       (if (str/blank? t)
                         (env-or-prop "SPRITES_TOKEN" "")
                         t))

        agent-name   (env-or-prop "AGENT" "opencode")
        claude-auth  (env-or-prop "CLAUDE_AUTH" "subscription")
        api-key      (env-or-prop "ANTHROPIC_API_KEY" "")
        model        (env-or-prop "MODEL" "")

        interval-str (env-or-prop "CHECKPOINT_INTERVAL" "300")
        interval     (try
                       (Integer/parseInt interval-str)
                       (catch NumberFormatException _
                         (println (str "Error: invalid CHECKPOINT_INTERVAL '"
                                       interval-str "' (must be integer)"))
                         (System/exit 1)
                         300))]

    {:sprite-token        sprite-token
     :agent               agent-name
     :claude-auth         claude-auth
     :anthropic-api-key   api-key
     :model               model
     :checkpoint-interval interval
     :env-file            env-file
     ;; CLI flags — set by subcommands
     :dry-run             false
     :checkpointing       true
     :upload-dirs         []}))
