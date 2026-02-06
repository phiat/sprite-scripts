(ns sprite-tool.core
  "sprite-tool: Clojure port of sprite-scripts.
   Entry point and subcommand dispatch."
  (:require [sprite-tool.launch :as launch]
            [sprite-tool.push :as push]
            [sprite-tool.pull :as pull]
            [sprite-tool.watch :as watch])
  (:gen-class))

(defn- usage []
  (println "Usage: sprite-tool <command> [args...]")
  (println)
  (println "Commands:")
  (println "  launch    Create and configure a sprite with coding agent, git, beads")
  (println "  push      Push local file or directory to a sprite")
  (println "  pull      Pull file or directory from a sprite")
  (println "  watch     Poll a sprite's beads tracker task for progress")
  (println)
  (println "Run 'sprite-tool <command> --help' for more information on a command.")
  (println)
  (println "Examples:")
  (println "  clj -M:run launch --dry-run my-sprite plan.md")
  (println "  clj -M:run push ./file.txt /home/sprite/file.txt")
  (println "  clj -M:run pull /home/sprite/output ./output my-sprite")
  (println "  clj -M:run watch my-sprite")
  (System/exit 1))

(defn -main
  "Main entry point. Dispatches to subcommand handlers."
  [& args]
  (when (empty? args) (usage))

  (let [command  (first args)
        sub-args (rest args)]
    (case command
      "launch" (launch/run (vec sub-args))
      "push"   (push/run (vec sub-args))
      "pull"   (pull/run (vec sub-args))
      "watch"  (watch/run (vec sub-args))
      "--help" (usage)
      "-h"     (usage)
      (do
        (println (str "Unknown command: " command))
        (usage)))))
