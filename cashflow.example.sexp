;; cashflow configuration. Copy to /usr/local/etc/cashflow.sexp, or pass --config.
;; Unknown or misspelt fields are errors; check with: cashflow check-config --config FILE

((database /var/db/cashflow/cashflow.sqlite3)  ; on a local ZFS dataset in the jail, never NFS

 ;; The private listener; Caddy owns public TLS in front of it.
 (listen 127.0.0.1:8081)
 (public_url https://cashflow.example.com)

 (oidc
  ((issuer https://id.example.com)
   ;; What MCP bearer tokens must carry as their audience: normally the public URL.
   (audience https://cashflow.example.com)
   ;; Members of this Pocket ID group are admins. Optional; this is the default.
   (admin_group cashflow-admin)))

 ;; The web UI's confidential OIDC client. Its callback is <public_url>/auth/callback.
 ;; Leave the whole field out to serve only MCP.
 (web
  ((client_id cashflow-web)
   ;; A secret is (Inline "..."), (File /path) or (Env NAME), so this file can live in git.
   (client_secret (File /usr/local/etc/cashflow/web-client-secret))))

 ;; Where the built web UI is installed. Optional; this is the default.
 (static_dir /usr/local/share/cashflow/web)

 ;; Quiet, Error, Warning, Info or Debug. Optional; this is the default.
 ;; CASHFLOW_LOG_LEVEL overrides it for a single run.
 (log_level Info)

 ;; Advisor generation remains unavailable unless openrouter.advisor_limits is supplied.
 ;; Its required fields are context_tokens, completion_tokens,
 ;; whole_completion_cap_supported, request_tokens_per_byte, request_token_overhead.
 ;; No defaults: independently substantiate these for the exact model and ALL eligible
 ;; routes, including reasoning/tool completion caps and tokenizer/provider framing.
 ;; Positive arithmetic alone is not evidence. No production route is attested here.
 ;; Missing/unsupported limits do not affect legacy categorisation or summaries.
 ;; Optional: category suggestions and month summaries through OpenRouter.
 ;; Remove #; to enable. Every request requires ZDR and denies data collection.
 ;; provider_only is optional; these are its defaults. An empty list is refused.
 ;; If no allowed ZDR endpoint serves the model, AI features stay unavailable;
 ;; requests never retry with weaker privacy restrictions.
 #;(openrouter
    ((api_key (Env OPENROUTER_API_KEY))
     (model anthropic/claude-haiku-5.5)
     (provider_only (google-vertex amazon-bedrock azure))))
 )
