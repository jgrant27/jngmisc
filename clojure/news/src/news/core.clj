(ns news.core
  (:gen-class)
  (:require [clojure.core.async :as async :refer [>! >!! <! <!!]]
            [clj-http.client :as client]
            [clj-http.conn-mgr :as conn-mgr]))

(def ^:const HN_BASE_URL "https://hacker-news.firebaseio.com/v0")
(def ^:const HN_TOP_STORIES_URL (format "%s/topstories.json" HN_BASE_URL))
(def ^:const HN_STORY_BASE_URL (format "%s/item" HN_BASE_URL))
(def ^:const THREADS_IN_POOL 64)

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
        ch (async/merge
            (for [[i story_id] (map-indexed vector (take story_cnt story_ids))]
                  (async/go (-get-story (inc i) story_id)))
            story_cnt)
        stories (sort (for [_ (take story_cnt story_ids)] (<!! ch)))]
    (doseq [[i story] stories] (println story))
    (flush)))
