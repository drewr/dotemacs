;;; agents.el --- LLM helpers for gptel and agent-shell -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)

;; Defined in the agent-shell package, which is loaded on first use
;; (and which pulls in `map' itself).
(declare-function map-elt "map")
(declare-function agent-shell-default-agent-config-makers "agent-shell")
(defvar agent-shell-agent-configs)
(defvar agent-shell-preferred-agent-config)
(defvar agent-shell-google-gemini-acp-command)
(defvar agent-shell-opencode-default-config-options)

;; Defined in the gptel package, which is loaded after startup.
(declare-function gptel "gptel")
(declare-function gptel-make-openai "gptel-openai")
(declare-function gptel-make-anthropic "gptel-anthropic")
(declare-function gptel-make-ollama "gptel-ollama")
(declare-function gptel-backend-models "gptel-request")
(defvar gptel-backend)
(defvar gptel-model)

(defun aar/llm-command-available-p (command)
  "Return non-nil when COMMAND is available on this machine."
  (and command (executable-find command)))

(defun aar/llm-ollama-models ()
  "Return the local Ollama models available on this machine."
  (when (aar/llm-command-available-p "ollama")
    (condition-case nil
        (let ((lines (process-lines "ollama" "list")))
          (cl-loop for line in (cdr lines)
                   for model = (car (split-string line))
                   when (and model (not (string= model "")))
                   collect model))
      (error nil))))

(defun aar/agent-shell-default-agent ()
  "Return the preferred `agent-shell' config for this machine."
  (cond
   ((aar/llm-command-available-p "claude-agent-acp") 'claude-code)
   ((aar/llm-command-available-p "codex-acp") 'codex)
   (t nil)))

(defun aar/agent-shell-installed-agents ()
  "Return the default agent config makers whose ACP command is installed.
Building a client does not start a process, so this only inspects
the command each agent would run."
  (seq-filter
   (lambda (maker)
     (when-let* ((config (ignore-errors (funcall maker)))
                 (client-maker (map-elt config :client-maker))
                 (client (ignore-errors (funcall client-maker (current-buffer))))
                 (command (map-elt client :command)))
       (executable-find command)))
   (agent-shell-default-agent-config-makers)))

(defun aar/agents-setup-agent-shell ()
  "Configure `agent-shell' defaults for the current machine.
Only installed agents appear in the picker, with the preferred one
preselected; `C-x M-a' still prompts so any installed harness is
one keystroke away."
  ;; The default `--experimental-acp' flag is deprecated in gemini-cli.
  (setq agent-shell-google-gemini-acp-command '("gemini" "--acp"))
  ;; OpenCode's ACP server ignores the TUI's model history and starts on
  ;; its own default, a paid model this account has no credits for.
  (setq agent-shell-opencode-default-config-options
        '(("model" . "opencode/deepseek-v4.1-flash-free")))
  (setq agent-shell-agent-configs #'aar/agent-shell-installed-agents)
  (setq agent-shell-preferred-agent-config
        (when-let* ((agent (aar/agent-shell-default-agent)))
          (cons 'preselect agent))))

(defvar aar/gptel-openai-backend nil)
(defvar aar/gptel-anthropic-backend nil)
(defvar aar/gptel-ollama-backend nil)

(defun aar/gptel-with-backend (backend)
  "Open gptel with BACKEND selected.
`gptel-backend' is only read when a request is sent, so binding it
around `gptel' is not enough: pin it buffer-locally in the chat."
  (require 'gptel)
  (unless backend
    (user-error "That gptel backend is not configured"))
  (let* ((models (gptel-backend-models backend))
         (model (if (memq gptel-model models) gptel-model (car models)))
         (buf (let ((gptel-backend backend)
                    (gptel-model model))
                (call-interactively #'gptel))))
    (with-current-buffer buf
      (setq-local gptel-backend backend
                  gptel-model model))
    buf))

(defun aar/gptel-anthropic-chat ()
  "Open gptel using the Anthropic backend."
  (interactive)
  (aar/gptel-with-backend aar/gptel-anthropic-backend))

(defun aar/gptel-openai-chat ()
  "Open gptel using the OpenAI backend."
  (interactive)
  (aar/gptel-with-backend aar/gptel-openai-backend))

(defun aar/gptel-local-chat ()
  "Open gptel using the local Ollama backend."
  (interactive)
  (if aar/gptel-ollama-backend
      (aar/gptel-with-backend aar/gptel-ollama-backend)
    (user-error "No Ollama backend is available on this machine")))

(defun aar/agents-setup-gptel ()
  "Register gptel backends and choose the default backend."
  (setq aar/gptel-openai-backend
        (gptel-make-openai "OpenAI"
          :key 'gptel-api-key
          :stream t)
        aar/gptel-anthropic-backend
        (gptel-make-anthropic "Anthropic"
          :key 'gptel-api-key
          :stream t)
        aar/gptel-ollama-backend
        (let ((models (aar/llm-ollama-models)))
          (when models
            (gptel-make-ollama "Ollama"
              :stream t
              :models models)))
        gptel-backend
        (or aar/gptel-ollama-backend
            aar/gptel-anthropic-backend
            aar/gptel-openai-backend)))
