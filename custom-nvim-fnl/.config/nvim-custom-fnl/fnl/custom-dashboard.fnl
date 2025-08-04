(local dashboard (require :dashboard))

(local logo (vim.fn.system "toilet -f future 'Ezechukwu69'"))

(dashboard.setup {
              :theme "hyper" ;; doom or hyper 
              :config {
                  :header (vim.split logo "\n")
              }
})
