(ns sprite-tool.watch
  "sprite-tool watch: Poll a sprite's beads tracker task for progress."
  (:require [clojure.string :as str]
            [sprite-tool.sprite :as sprite])
  (:import [java.time LocalTime]
           [java.time.format DateTimeFormatter]))

(defn- usage []
  (println "Usage: sprite-tool watch <sprite-name> [task-id] [poll-interval]")
  (println)
  (println "Arguments:")
  (println "  sprite-name     Name of the sprite to watch")
  (println "  task-id         Beads task ID to track (default: auto-detect first open critical task)")
  (println "  poll-interval   Seconds between polls (default: 30)")
  (println)
  (println "Examples:")
  (println "  sprite-tool watch ember-red-hawk")
  (println "  sprite-tool watch ember-red-hawk CRM-1")
  (println "  sprite-tool watch ember-red-hawk CRM-1 60")
  (System/exit 1))

(defn- time-now []
  (.format (LocalTime/now) (DateTimeFormatter/ofPattern "HH:mm:ss")))

(defn- sx-quiet
  "Run command in sprite, suppress stderr, return stdout."
  [sprite-name cmd]
  (let [result (sprite/run-process
                ["sprite" "exec" "-s" sprite-name "bash" "-c" cmd]
                :inherit-io false :capture true)]
    (or (:stdout result) "")))

(defn run
  "Execute the watch subcommand."
  [args]
  (when (empty? args) (usage))

  (let [sprite-name   (first args)
        task-id-arg   (second args)
        poll-interval (if (> (count args) 2)
                        (try
                          (Integer/parseInt (nth args 2))
                          (catch NumberFormatException _
                            (println "Error: poll-interval must be an integer")
                            (System/exit 1)
                            30))
                        30)

        ;; Auto-detect tracker task if not specified
        task-id (if (not (str/blank? task-id-arg))
                  task-id-arg
                  (do
                    (println "Detecting tracker task...")
                    (let [critical (str/trim
                                   (sx-quiet sprite-name
                                             "cd /home/sprite && bd list --priority critical 2>/dev/null | head -1 | awk '{print $1}'"))
                          task (if (str/blank? critical)
                                 (do
                                   (println "No critical task found. Falling back to first open task...")
                                   (str/trim
                                    (sx-quiet sprite-name
                                              "cd /home/sprite && bd list 2>/dev/null | head -1 | awk '{print $1}'")))
                                 critical)]
                      (when (str/blank? task)
                        (println (str "ERROR: No beads tasks found on sprite '" sprite-name "'"))
                        (println (str "Specify a task ID manually: sprite-tool watch " sprite-name " <task-id>"))
                        (System/exit 1))
                      (println (str "Tracking task: " task))
                      task)))]

    (println (str "Watching sprite '" sprite-name "' task '" task-id "' (every " poll-interval "s)"))
    (println "Press Ctrl+C to stop")
    (println)

    (loop []
      ;; Clear screen: \033[2J\033[H
      (print "\033[2J\033[H")
      (flush)

      (println (str "=== sprite-watch: " sprite-name " / " task-id " === " (time-now) " ==="))
      (println)

      ;; Show task status
      (let [task-output (sx-quiet sprite-name
                                  (str "cd /home/sprite && bd show " task-id " 2>/dev/null"))]
        (if (str/blank? task-output)
          (println "(could not read task)")
          (println task-output)))
      (println)

      ;; Show recent comments
      (println "--- Recent updates ---")
      (let [comments (sx-quiet sprite-name
                               (str "cd /home/sprite && bd comments " task-id " 2>/dev/null | tail -8"))]
        (if (str/blank? comments)
          (println "(no comments)")
          (println comments)))
      (println)

      ;; Check if done
      (let [status-line (sx-quiet sprite-name
                                   (str "cd /home/sprite && bd show " task-id " 2>/dev/null | grep -i status"))
            done? (and (not (str/blank? status-line))
                       (re-find #"(?i)closed|done|completed" status-line))]
        (if done?
          (do
            (println "==========================================")
            (println "PROJECT COMPLETE")
            (println "=========================================="))
          (do
            (Thread/sleep (* poll-interval 1000))
            (recur)))))))
