(ns sprite-tool.sprite
  "Subprocess wrapper for the sprite CLI using ProcessBuilder."
  (:require [clojure.string :as str])
  (:import [java.io File]
           [java.lang ProcessBuilder ProcessBuilder$Redirect]))

(defn find-sprite-cli
  "Check if the sprite CLI is on PATH. Returns true if found."
  []
  (try
    (let [pb (ProcessBuilder. ["which" "sprite"])]
      (.redirectErrorStream pb true)
      (let [proc (.start pb)
            exit (.waitFor proc)]
        (zero? exit)))
    (catch Exception _ false)))

(defn run-process
  "Run a command via ProcessBuilder. Returns the exit code.
   Options:
     :inherit-io  - inherit stdin/stdout/stderr (default true)
     :capture     - capture stdout as string (default false)
     :stdin-file  - redirect stdin from this file path
     :stdin-pipe  - a Process whose stdout should be piped to this stdin"
  [args & {:keys [inherit-io capture stdin-file stdin-pipe]
           :or {inherit-io true capture false}}]
  (let [pb (ProcessBuilder. ^java.util.List (vec args))]
    (when stdin-file
      (.redirectInput pb (File. ^String stdin-file)))
    (when inherit-io
      (when-not capture
        (if stdin-file
          ;; inheritIO would override stdin redirect, so only inherit stdout/stderr
          (doto pb
            (.redirectOutput ProcessBuilder$Redirect/INHERIT)
            (.redirectError ProcessBuilder$Redirect/INHERIT))
          (.inheritIO pb))))
    (when stdin-pipe
      ;; stdin-pipe is an InputStream from another process
      ;; We handle this after start
      nil)
    (let [proc (.start pb)]
      (when stdin-pipe
        ;; Copy from the input stream to process stdin
        (let [out (.getOutputStream proc)]
          (try
            (let [buf (byte-array 8192)]
              (loop []
                (let [n (.read ^java.io.InputStream stdin-pipe buf)]
                  (when (pos? n)
                    (.write out buf 0 n)
                    (recur)))))
            (finally
              (.close out)))))
      (if capture
        (let [stdout (slurp (.getInputStream proc))
              exit   (.waitFor proc)]
          {:exit exit :stdout (str/trim stdout)})
        (let [exit (.waitFor proc)]
          {:exit exit})))))

(defn sx
  "Run a command inside a sprite via bash.
   Equivalent to: sprite exec -s SPRITE bash -c CMD
   Returns captured stdout as string, or nil in dry-run mode."
  [sprite-name cmd & {:keys [dry-run] :or {dry-run false}}]
  (if dry-run
    (do
      (println (str "  [dry-run] sprite exec -s " sprite-name " bash -c \"" cmd "\""))
      nil)
    (let [result (run-process ["sprite" "exec" "-s" sprite-name "bash" "-c" cmd]
                              :inherit-io false :capture true)]
      (:stdout result))))

(defn sx-passthrough
  "Run a command inside a sprite, passing stdout/stderr through to terminal.
   Returns the exit code."
  [sprite-name cmd & {:keys [dry-run] :or {dry-run false}}]
  (if dry-run
    (do
      (println (str "  [dry-run] sprite exec -s " sprite-name " bash -c \"" cmd "\""))
      0)
    (let [result (run-process ["sprite" "exec" "-s" sprite-name "bash" "-c" cmd]
                              :inherit-io true)]
      (:exit result))))

(defn push-file
  "Push a local file to a sprite.
   Equivalent to:
     sprite exec -s SPRITE bash -c 'mkdir -p $(dirname DEST)'
     sprite exec -s SPRITE bash -c 'cat > DEST' < src"
  [sprite-name src dest & {:keys [dry-run] :or {dry-run false}}]
  (if dry-run
    (println (str "  [dry-run] push " src " -> sprite:" dest))
    (let [dest-dir (.getParent (File. ^String dest))
          dest-dir (or dest-dir "/")]
      ;; mkdir -p
      (run-process ["sprite" "exec" "-s" sprite-name "bash" "-c"
                    (str "mkdir -p '" dest-dir "'")]
                   :inherit-io true)
      ;; cat > dest < src
      (run-process ["sprite" "exec" "-s" sprite-name "bash" "-c"
                    (str "cat > '" dest "'")]
                   :inherit-io true :stdin-file src))))

(defn push-dir
  "Push a local directory to a sprite via tar.
   Equivalent to:
     sprite exec -s SPRITE bash -c 'mkdir -p DEST'
     tar czf - -C parent base | sprite exec -s SPRITE bash -c 'tar xzf - -C parent_of_dest'"
  [sprite-name src dest & {:keys [dry-run] :or {dry-run false}}]
  (if dry-run
    (println (str "  [dry-run] push dir " src " -> sprite:" dest))
    (let [src-file    (File. ^String src)
          parent      (.getParent src-file)
          base        (.getName src-file)
          dest-parent (or (.getParent (File. ^String dest)) "/")]
      ;; mkdir -p dest
      (run-process ["sprite" "exec" "-s" sprite-name "bash" "-c"
                    (str "mkdir -p '" dest "'")]
                   :inherit-io true)
      ;; tar czf - -C parent base | sprite exec ... bash -c "tar xzf - -C dest_parent"
      (let [tar-pb (doto (ProcessBuilder. ^java.util.List
                                          ["tar" "czf" "-" "-C" parent base])
                     (.redirectOutput ProcessBuilder$Redirect/PIPE)
                     (.redirectError ProcessBuilder$Redirect/INHERIT))
            tar-proc (.start tar-pb)

            sprite-pb (doto (ProcessBuilder. ^java.util.List
                                             ["sprite" "exec" "-s" sprite-name
                                              "bash" "-c"
                                              (str "tar xzf - -C '" dest-parent "'")])
                        (.redirectOutput ProcessBuilder$Redirect/INHERIT)
                        (.redirectError ProcessBuilder$Redirect/INHERIT))
            sprite-proc (.start sprite-pb)]
        ;; Pipe tar stdout -> sprite stdin in a thread
        (let [tar-out (.getInputStream tar-proc)
              sprite-in (.getOutputStream sprite-proc)
              pipe-thread (Thread.
                           (fn []
                             (try
                               (let [buf (byte-array 8192)]
                                 (loop []
                                   (let [n (.read tar-out buf)]
                                     (when (pos? n)
                                       (.write sprite-in buf 0 n)
                                       (recur)))))
                               (catch Exception _)
                               (finally
                                 (.close sprite-in)))))]
          (.start pipe-thread)
          (.join pipe-thread)
          (.waitFor tar-proc)
          (let [exit (.waitFor sprite-proc)]
            (when-not (zero? exit)
              (throw (ex-info "push-dir failed" {:exit exit})))))))))

(defn sprite-list
  "Run 'sprite ls' and return stdout."
  []
  (let [result (run-process ["sprite" "ls"] :inherit-io false :capture true)]
    (or (:stdout result) "")))

(defn sprite-exists?
  "Check if a sprite with the given name already exists."
  [sprite-name]
  (let [output (sprite-list)]
    (some (fn [line]
            (let [parts (str/split (str/trim line) #"\s+")]
              (some #(= % sprite-name) parts)))
          (str/split-lines output))))
