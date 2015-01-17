(ns context-io.core "Core functions for the library"
  (:use
    [context-io utils api oauth request])
  (:require
    [clojure.string :as string]
    [http.async.client :as ac])
  (:import
    (clojure.lang Keyword PersistentArrayMap)))

(defn- fix-keyword
  "Takes a parameter name and replaces the -s with _s"
  [param-name]
  (keyword (.replace (name param-name) \- \_)))

(defn- fix-colls
  "Turns collections into their string, comma-separated equivalents."
  [val]
  (if (coll? val) (string/join "," val) val))

(defn- add-form-content-type
  "Adds a content type of url-encoded-form to the supplied headers."
  [headers]
  (merge headers
    {:content-type "application/x-www-form-urlencoded"}))

(def memo-create-client (memoize ac/create-client))

(defn default-client
  "Makes a default async client for the HTTP communications

   The only thing this does, is basically to call a memoized version of
   http.async.client/create-client.

   Returns an AsyncHttpClient."
  []
  (memo-create-client :follow-redirects false))

(defn- get-request-args
  "Takes URI, action and optional args and returns the final URI and HTTP
   parameters for the subsequent call. Note that the parameters are transformed
   (from lispy -s to x-header-style _s) and added to the query. So :params could
   be {:screen-name 'blah'}, and it would be merged into :query as {:screen_name
   'blah'}. The URI has the parameters substituted in, so {:id} in the URI would
   use the :id in the :params map. Also, the OAuth headers are added."
  [^Keyword action
   ^String uri
   ^PersistentArrayMap arg-map]
  (let [params (transform-map (:params arg-map)
                              :key-trans fix-keyword
                              :val-trans fix-colls)
        body (:body arg-map)
        query (merge (:query arg-map) params)
        final-uri (subs-uri uri params)
        oauth-map (sign-query (:oauth-creds arg-map)
                              action
                              final-uri
                              :query query)
        headers (merge (:headers arg-map)
                       {:Authorization (oauth-header-string oauth-map)})
        my-args (cond (= action :get) (hash-map :query query
                                                :headers headers
                                                :body body)
                      (nil? body) (hash-map
                                    :headers (add-form-content-type headers)
                                    :body query)
                      :else (hash-map :query query
                                      :headers headers
                                      :body body))]
    {:action action
     :uri final-uri
     :processed-args (merge (dissoc arg-map :query :headers :body :params
                                            :oauth-creds :client :api
                                            :callbacks)
                            my-args)}))

(defn http-request
  "Calls the action on the resource specified in the URI

   This will also sign the request with OAuth.

   action  - The keyword HTTP action (:get, :post, etc.).
   uri     - The URI to request.
   arg-map - The map of options to pass to the request. This also takes args to
             be passed on to async.http.client.
             :client      - The HTTP client to use for this call (optional).
             :callbacks   - The callbacks to use for this call.
             :params      - The parameters to pass to this call. This
             :oauth-creds - The OAuthCredentials to use to sign this call.

   Examples

     (http-request :get \"https://api.context.io/accounts/{:id}\"
       {:callbacks (get-default-callbacks)
        :params {:id \"0\"}
        :oauth-creds (OAuthCredentials. \"consumer-key\"
                                        \"consumer-secret\")})

   Returns what the appropriate callback returns."
  [^Keyword action
   ^String uri
   ^PersistentArrayMap arg-map]
  (let [client (or (:client arg-map) (default-client))
        callbacks (or (:callbacks arg-map)
                      (throw (Exception. "need to specify a callback argument for http-request")))
        request-args (get-request-args action uri arg-map)
        request (apply prepare-request-with-multi
                       (:action request-args)
                       (:uri request-args)
                       (apply concat (:processed-args request-args)))
        async (:async arg-map)]
    (execute-request client request callbacks async)))

(defmacro def-context-io-method
  "Declares a Context.IO method with the supplied information

   name          - The symbol name of the function to make.
   action        - The keyword HTTP action (:get, :post, etc.).
   resource-path - The path to append to the URI generated by the API context.
   rest          - The map of options to pass.
                   :api       - The ApiContext to use for this call.
                   :client    - The HTTP client to use for this call (optional).
                   :callbacks - The callbacks to use for this call.

   Examples

     (def-context-io-method list-accounts :get \"accounts\"
       :api (ApiContext. \"https\" \"api.context.io\" \"2.0\")
       :callbacks (get-default-callbacks))

   Returns nothing"
  [name action resource-path & rest]
  (let [rest-map (apply sorted-map rest)]
    `(defn ~name
      [creds# & {:as args#}]
      (let [arg-map# (merge ~rest-map args# {:oauth-creds creds#})
            api-context# (assert-throw (:api arg-map#) "must include an ':api entry in the params")
            uri# (make-uri api-context# ~resource-path)]
        (http-request ~action uri# arg-map#)))))
