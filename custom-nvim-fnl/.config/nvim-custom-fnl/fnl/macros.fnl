(fn map! [...]
  """Set a mapping in the form of:
  Example:
  (map! 
    :mode 'mode' or ['mode1', 'mode'] 
    :key 'key'
    :command 'command' or (fn command) 
    :options {:desc 'Test command'}
  )"""

  (let [args [...]]
    (var tbl {})
    (for [i 1 (length args) 2]
      (tset tbl (tostring (. args i)) (. args (+ i 1))))
      `(vim.keymap.set 
         (or ,(. tbl "mode") "n")
         ,(. tbl "key")
         ,(. tbl "command")
         ,(. tbl "options")
        )
   )
)


(fn call! [& form]
  "Wrap a call expression in pcall, returning a table: {:ok boolean :result ... :err msg?}"
  ;; form is something like: (mod.fn arg1 arg2) or (obj:method arg)
  `(let [(ok result) (pcall ,form)]
     (if ok
       {:ok true :result result}
       {:ok false :err result})))

{ :map! map!
:call! call! }
