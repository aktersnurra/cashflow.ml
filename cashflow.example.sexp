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

 ;; Optional: category suggestions and month summaries through OpenRouter.
 ;; (openrouter ((api_key (Env OPENROUTER_API_KEY)) (model anthropic/claude-haiku-5.5)))
 )
