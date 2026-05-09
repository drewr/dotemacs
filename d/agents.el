;;; agents.el --- LLM helpers for gptel and agent-shell -*- lexical-binding: t; -*-

(require 'cl-lib)

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

(defun aar/agents-setup-agent-shell ()
  "Configure `agent-shell' defaults for the current machine."
  (setq agent-shell-preferred-agent-config (aar/agent-shell-default-agent)))

(defvar aar/gptel-openai-backend nil)
(defvar aar/gptel-anthropic-backend nil)
(defvar aar/gptel-ollama-backend nil)

(defun aar/gptel-with-backend (backend)
  "Open gptel with BACKEND selected."
  (let ((gptel-backend backend))
    (call-interactively #'gptel)))

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
