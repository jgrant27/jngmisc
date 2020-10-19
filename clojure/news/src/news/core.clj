(ns news.core
  (:gen-class)
  (:require [clojure.core.async :as async :refer [>! >!! <! <!!]]
            [clj-http [client :as client] [conn-mgr :as conn-mgr]]
            [criterium.core :as criterium]))

(def ^:const HN_BASE_URL "https://hacker-news.firebaseio.com/v0")
(def ^:const HN_TOP_STORIES_URL (format "%s/topstories.json" HN_BASE_URL))
(def ^:const HN_STORY_BASE_URL (format "%s/item" HN_BASE_URL))
(def ^:const THREADS_IN_POOL (* 10 (. (Runtime/getRuntime) availableProcessors)))

(client/with-connection-pool {:timeout 3 :so-keep-alive true :threads THREADS_IN_POOL}
  (defn -get-story [i story_id]
    (let [story_url (format "%s/%s.json" HN_STORY_BASE_URL story_id)
          story (:body (client/get story_url {:as :json}))]
      [i (format "%s - %s - %s - %s" i (:id story) (:title story) (:url story))])))

(System/setProperty "clojure.core.async.pool-size" (str THREADS_IN_POOL))

(defn -main
  [& args]
  (let [story_ids (:body (client/get HN_TOP_STORIES_URL {:as :json}))
        story_cnt (if (empty? args)
                    (count story_ids)
                    (min (Integer/parseInt (nth args 0)) (count story_ids)))
        story_ids (into [] (take story_cnt story_ids))]
    (printf "%s stories to get ...\n%s\n\n" story_cnt story_ids)
    (flush)
    (let [ch (async/merge
              (for [[i story_id] (map-indexed vector story_ids)]
                (async/go (-get-story (inc i) story_id)))
              story_cnt)
          stories (sort (for [_ story_ids] (<!! ch)))]
      (doseq [[i story] stories] (println story) (flush)))
    (println)))

(defn -bench []
  (criterium/quick-bench (-main)))
