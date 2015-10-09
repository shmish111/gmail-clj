(ns gmail-clj.core
  (:require [aleph.http :as http]
            [cheshire.core :as json]
            [clojure.walk :as walk]
            [clojure.data.codec.base64 :as b64]
            [byte-streams :as bs]
            [manifold.deferred :as d])
  (:import (javax.mail Session Address Message Message$RecipientType)
           (javax.mail.internet MimeMessage InternetAddress)
           (java.net URLEncoder)))

(def base-url "https://www.googleapis.com/gmail/v1")

(def #^{:dynamic true} *client-id* nil)
(def #^{:dynamic true} *client-secret* nil)
(def #^{:dynamic true} *refresh-token* nil)
(def #^{:dynamic true} *access-token* nil)

(defn- now [] (quot (System/currentTimeMillis) 1000))

(defn set-client-id!
  "Set the client ID for the library."
  [id]
  (alter-var-root (var *client-id*) (fn [_] id)))

(defn set-client-secret!
  "Set the client secret for the library."
  [secret]
  (alter-var-root (var *client-secret*) (fn [_] secret)))

(defn set-refresh-token!
  "Set the refresh token for the library."
  [token]
  (alter-var-root (var *refresh-token*) (fn [_] token)))

(defn set-access-token!
  "Set the temp access token and it's expires time => {:token xxxxxx :expires xxxxxx}"
  [token expires]
  (alter-var-root (var *access-token*) (fn [_] {:token token :expires (+ (now) expires)})))

(defn prepend-url [url]
  (if (.startsWith url "http")
    url
    (str base-url url)))

(defn- clean-params
  "Removes any nil params."
  [params]
  (->> params
       (filter #(not (nil? (val %))))
       (into {})))

(defn- request-auth
  "Get an authorization token"
  []
  (when (not *client-id*) (throw (Exception. "Missing client id!")))
  (when (not *client-secret*) (throw (Exception. "Missing client secret!")))
  (when (not *refresh-token*) (throw (Exception. "Missing refresh token!")))
  (if-let [{:keys [body]} @(http/post "https://accounts.google.com/o/oauth2/token" {:as          :json
                                                                                    :form-params {:client_id     *client-id*
                                                                                                  :client_secret *client-secret*
                                                                                                  :refresh_token *refresh-token*
                                                                                                  :grant_type    "refresh_token"}})]
    (set-access-token! (:access_token body) (:expires_in body))
    false))

(defn- get-token
  "Returns the current active auth token."
  []
  (when (or (nil? *access-token*)
            (> (:expires *access-token*) (now)))
    (request-auth))
  (:token *access-token*))

(defn- bytes->clj
  [bytes]
  (-> bytes
      bs/to-string
      json/parse-string
      walk/keywordize-keys))

(defn api-middleware
  "middleware for making API requests"
  [client]
  (fn [{:keys [as request-timeout]
        :or {as :json request-timeout 2000}
        :as req}]
    (-> (client (-> req
                    (update-in [:query-params] clean-params)
                    (update-in [:url] prepend-url)
                    (assoc :as as)
                    (assoc :oauth-token (get-token))
                    (assoc :request-timeout request-timeout)))
        (d/chain (fn [{:keys [body]}] body))
        (d/catch Exception
                 (fn [e]
                   (let [{:keys [status] :as rsp} (update-in (ex-data e) [:body] bytes->clj)]
                     (d/error-deferred (ex-info (str "status: " status) rsp))))))))

(defmulti api-request (fn [method _ _] method))

(defmethod api-request :get [_ url params]
  (http/get url {:middleware   api-middleware
                 :query-params params}))

(defmethod api-request :post [_ url params]
  (http/post url {:middleware  api-middleware
                  :form-params params}))

(defmethod api-request :delete [_ url params]
  (http/delete url {:middleware  api-middleware
                    :form-params params}))

(defmethod api-request :post-json [_ url params]
  (http/post url {:middleware   api-middleware
                  :content-type :json
                  :query-params params}))

(defmethod api-request :put-json [_ url params]
  (http/put url {:middleware   api-middleware
                 :content-type :json
                 :query-params params}))

(defn bytes? [x]
  (= (Class/forName "[B")
     (.getClass x)))

(defn str->b64 [input]
  (let [input (if (bytes? input) input (.getBytes input))]
    (String. (b64/encode input) "UTF-8")))

(defn b64->str [input]
  (let [input (if (bytes? input) input (.getBytes input))]
    (String. (b64/decode input))))

(defmacro address [& args]
  `(new InternetAddress ~@args))

(defn addresses [addrs]
  (->> addrs
       vector
       flatten
       (map #(address %))))

(defn map->mime
  "Takes a map and returns a mime-formatted object."
  [{:keys [to cc bcc from subject body] :as msg}]
  (let [session (Session/getDefaultInstance (java.util.Properties.))
        message (MimeMessage. session)]
    (when from (.setFrom message (address from)))
    (when to (.addRecipients message Message$RecipientType/TO (into-array (addresses to))))
    (when cc (.addRecipients message Message$RecipientType/CC (into-array (addresses cc))))
    (when bcc (.addRecipients message Message$RecipientType/BCC (into-array (addresses bcc))))
    (when subject (.setSubject message subject))
    (when body (.setText message body))
    message))

; https://developers.google.com/gmail/api/v1/reference/users/drafts

(defn draft-create
  "Create a draft with the DRAFT label."
  [message]
  (let [message (if (map? message) (map->mime message) message)
        bytes (with-open [os (java.io.ByteArrayOutputStream.)]
                (.writeTo message os)
                (.toByteArray os))
        b64 (str->b64 bytes)]
    (api-request :post-json "/users/me/drafts" {:message {:raw (URLEncoder/encode b64 "UTF-8")}})))

(defn draft-delete
  "Delete a draft by message-id."
  [message-id]
  (api-request :delete (str "/users/me/drafts/" message-id) {}))

(defn draft-get
  "Returns the record for a given draft message id."
  [message-id & {:keys [format]
                 :or   {format :full}}]
  (api-request :get (str "/users/me/drafts/" message-id) {:format (name format)}))

(defn draft-list
  "Returns all draft messages."
  [& {:keys [maxResults pageToken] :or {maxResults 10}}]
  (api-request :get "/users/me/drafts/" {:maxResults maxResults :pageToken pageToken}))

(defn draft-update
  "Update a draft via a message-id."
  [message-id]
  (api-request :put (str "/users/me/drafts/" message-id) {}))

(defn draft-send
  "Send a draft via the message-id."
  [message-id]
  (api-request :post-json "/users/me/drafts/send" {:id message-id}))

; https://developers.google.com/gmail/api/v1/reference/users/history

(defn history-list
  "Pull a list of items from a user's history."
  [startHistoryId & {:keys [maxResults pageToken labelId]
                     :or   {maxResults 10}}]
  (let [params {:maxResults     maxResults
                :pageToken      pageToken
                :startHistoryId startHistoryId
                :labelId        labelId}]
    (api-request :get "/users/me/history" params)))

; https://developers.google.com/gmail/api/v1/reference/users/labels

(defn label-create
  "Creates a new label."
  [name & {:keys [labelListVisibility messageListVisibility]
           :or   {labelListVisibility "labelShowIfUnread" messageListVisibility "show"}}]
  (let [params {:labelListVisibility   labelListVisibility
                :messageListVisibility messageListVisibility}]
    (api-request :post-json "/users/me/labels" params)))

(defn label-delete
  "Deletes the specified label and removes it from any messages and threads that it is applied to."
  [label-id]
  (api-request :delete (str "/users/me/labels/" label-id) {}))

(defn label-list
  "Lists all labels in the user's mailbox."
  []
  (api-request :get "/users/me/labels/" {}))

(defn label-update
  "Updates the specified label."
  [label-id & {:keys [labelListVisibility messageListVisibility name]
               :or   {labelListVisibility "labelShowIfUnread" messageListVisibility "show"}}]
  (let [params {:id                    label-id
                :labelListVisibility   labelListVisibility
                :messageListVisibility messageListVisibility
                :name                  name}]
    (api-request :put-json (str "/users/me/labels/" label-id) params)))

(defn label-get
  "Gets the specified label."
  [label-id]
  (api-request :get (str "/users/me/labels/" label-id) {}))

(defn label-patch
  "Updates the specified label. This method supports patch semantics."
  []
  (throw (Exception. "Not yet implemented.")))

; https://developers.google.com/gmail/api/v1/reference/users/messages

(defn message-list
  "Returns a list of messages based on a given query."
  [& {:keys [labels max-results page query] :or {max-results 10}}]
  (let [params {:includeSpamTrash false
                :labelIds         labels
                :maxResults       max-results
                :pageToken        page
                :q                query}]
    (api-request :get "/users/me/messages/" params)))

(defn message-trash
  "Moves a specific message to the trash can."
  [message-id]
  (api-request :post (str "/users/me/messages/" message-id "/trash") {}))

(defn message-get
  "Returns the record for a given message id."
  [message-id & {:keys [format]
                 :or   {format :full}}]
  (-> (api-request :get (str "/users/me/messages/" message-id) {:format (name format)})
      (d/chain (fn [msg]
                 (if (= :full format)
                   (update-in msg [:payload :body :data] b64->str)
                   msg))
               (fn [msg]
                 (if (-> msg :payload :headers)
                   (update-in msg [:payload :headers]
                              (fn [hs] (->> hs
                                            (map (fn [{name :name value :value}]
                                                   {(keyword name) value}))
                                            (apply merge))))
                   msg)))))

(defn message-send
  "Sends an email to a specified user. Takse MIME obj or map with :to :subject :body"
  [message & {:keys [thread-id]}]
  (let [message (if (map? message) (map->mime message) message)
        bytes (with-open [os (java.io.ByteArrayOutputStream.)]
                (.writeTo message os)
                (.toByteArray os))
        b64 (str->b64 bytes)]
    (api-request :post-json "/users/me/messages/send" {:raw b64})))

; https://developers.google.com/gmail/api/v1/reference/users/threads

(defn thread-get
  "Returns a specific thread."
  [thread-id]
  (api-request :get (str "/users/me/threads/" thread-id) {}))

(defn thread-list
  "Lists all threads in the user's mailbox."
  [& {:keys [includeSpamTrash labelIds maxResults pageToken q]
      :or   {includeSpamTrash false maxResults 25}}]
  (let [labelIds (clojure.string/join "&" (map #(str "labelIds=" %) labelIds))
        params {:includeSpamTrash includeSpamTrash
                :maxResults       maxResults
                :pageToken        pageToken
                :q                q}]
    (api-request :get (str "/users/me/threads/?" labelIds) params)))

(defn thread-modify
  "Modifies the labels applied to the thread. This applies to all messages in the thread."
  [thread-id & {:keys [addLabelIds removeLabelIds]}]
  (let [params {:addLabelIds    addLabelIds
                :removeLabelIds removeLabelIds}]
    (api-request :post-json (str "/users/me/threads/" thread-id "/modify") params)))

(defn thread-delete
  "Immediately and permanently deletes the specified thread."
  [thread-id]
  (api-request :delete (str "/users/me/threads/" thread-id) {}))

(defn thread-trash
  "Moves the specified thread to the trash."
  [thread-id]
  (api-request :post (str "/users/me/threads/" thread-id "/trash") {}))

(defn thread-untrash
  "Removes the specified thread from the trash."
  [thread-id]
  (api-request :post (str "/users/me/threads/" thread-id "/untrash") {}))
