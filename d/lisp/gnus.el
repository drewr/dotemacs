(setq gnus-select-method '(nnnil ""))

(setq gnus-secondary-select-methods
      '((nnimap "aaraines@gmail.com"
                (nnimap-address "localhost")
                (nnimap-authenticator login))
        (nnml ""
              (nnir-search-engine swish-e)
              (nnir-swish-e-remove-prefix "/home/aar/Mail/")
              (nnir-swish-e-index-file "/home/aar/Mail/index.swish-e"))
        (nnfolder "mboxes"
                  (nnfolder-directory   "~/Mail/mboxes")
                  (nnfolder-active-file "~/Mail/mboxes/active")
                  (nnfolder-get-new-mail nil)
                  (nnfolder-inhibit-expiry nil))
        (nntp "news.gmane.org"
              (nntp-marks-is-evil t))))

